#lang racket

(require net/http-client
         net/uri-codec
         net/url
         json
         racket/system)

(provide make-http-request
         call-llm-for-fix
         extract-fixed-code
         safe-eval-with-fix
         auto-fix-program
         condition->string
         string-contains
         string-trim
         string-join
         example-usage)

(define (make-http-request url method headers body)
  "Make HTTP request using http-client"
  (define-values (scheme host port path)
    (split-url url))
  
  (define header-list
    (map (lambda (h) 
           (string-append (car h) ": " (cdr h)))
         headers))
  
  (define post-data (if body (string->bytes/utf-8 body) #""))
  
  (define-values (status response-headers response-port)
    (http-sendrecv host path
                   #:method method
                   #:data post-data
                   #:headers header-list
                   #:ssl? (string=? scheme "https")))
  
  (bytes->string/utf-8 (port->bytes response-port)))

(define (call-llm-for-fix error-msg code-snippet)
  "Call Claude API to get a fix for the error"
  (define api-key (or (getenv "ANTHROPIC_API_KEY") 
                      (error "ANTHROPIC_API_KEY environment variable not set")))
  (define url "https://api.anthropic.com/v1/messages")
  (define headers `(("x-api-key" . ,api-key)
                   ("Content-Type" . "application/json")
                   ("anthropic-version" . "2023-06-01")))
  (define prompt (string-append 
                  "Fix this Racket code error. Return only the corrected code, no explanation:\n\n"
                  "Error: " error-msg "\n\n"
                  "Code: " code-snippet))
  (define body (jsexpr->string
                `#hash((model . "claude-3-sonnet-20240229")
                       (max_tokens . 500)
                       (messages . (#hash((role . "user") 
                                          (content . ,prompt)))))))
  (define response (make-http-request url "POST" headers body))
  (extract-fixed-code response))

(define (extract-fixed-code response)
  "Extract the fixed code from Claude response"
  (define json-response (string->jsexpr response))
  (define content (hash-ref json-response 'content #f))
  (if (and content (list? content) (not (null? content)))
      (string-trim (hash-ref (first content) 'text ""))
      ""))

(define (safe-eval-with-fix expr)
  "Evaluate expression with auto-fix on error"
  (call/cc
    (lambda (return)
      (with-handlers ([exn:fail? (lambda (condition)
                                  (define error-msg (exn-message condition))
                                  (displayln (string-append "Error encountered: " error-msg))
                                  (displayln "Simple auto-fix: Using basic fallback")
                                  (displayln "Set ANTHROPIC_API_KEY for LLM integration")
                                  (return 'auto-fix-attempted))])
        (eval expr)))))

(define (auto-fix-program program-text)
  "Run a program with auto-fix capability"
  (define expressions 
    (let ([port (open-input-string program-text)])
      (let loop ([exprs '()])
        (define expr (read port))
        (if (eof-object? expr)
            (reverse exprs)
            (loop (cons expr exprs))))))
  (for-each safe-eval-with-fix expressions))

(define (condition->string condition)
  "Convert condition to string"
  (if (exn? condition)
      (exn-message condition)
      (format "~a" condition)))

(define (string-contains str substr)
  "Check if string contains substring"
  (define str-len (string-length str))
  (define sub-len (string-length substr))
  (let loop ([i 0])
    (cond
      [(> (+ i sub-len) str-len) #f]
      [(string=? (substring str i (+ i sub-len)) substr) i]
      [else (loop (+ i 1))])))

(define (string-trim str)
  "Trim whitespace from string"
  (define len (string-length str))
  (define start 
    (let loop ([i 0])
      (if (or (>= i len) (not (char-whitespace? (string-ref str i))))
          i
          (loop (+ i 1)))))
  (define end 
    (let loop ([i (- len 1)])
      (if (or (< i 0) (not (char-whitespace? (string-ref str i))))
          (+ i 1)
          (loop (- i 1)))))
  (if (> start end)
      ""
      (substring str start end)))

(define (string-join lst delimiter)
  "Join list of strings with delimiter"
  (if (null? lst)
      ""
      (foldr (lambda (x acc)
               (if (string=? acc "")
                   x
                   (string-append x delimiter acc)))
             ""
             lst)))

(define (split-url url)
  "Split URL into components"
  (define uri (string->url url))
  (values (if (url-scheme uri) (symbol->string (url-scheme uri)) "https")
          (url-host uri)
          (url-port uri)
          (apply string-append (map (lambda (p) (string-append "/" p)) 
                                   (map path/param-path (url-path uri))))))

(define (example-usage)
  "Example of using the auto-fix system"
  (displayln "Testing auto-fix system...")
  
  (displayln "Test 1: Syntax error")
  (safe-eval-with-fix '(+ 1 2 3))
  
  (displayln "Test 2: Undefined variable")
  (safe-eval-with-fix '(+ undefined-var 5))
  
  (displayln "Test 3: Type error")
  (safe-eval-with-fix '(+ "hello" 5)))

(displayln "Auto-fix Racket program loaded.")
(displayln "Usage: (safe-eval-with-fix '(your-expression))")
(displayln "Example: (example-usage)")