#lang racket

(require net/http-client
         net/uri-codec
         net/url
         json
         racket/system)

(provide define-auto-fix-handler
         with-auto-fix
         generate-llm-client
         auto-fix-function
         generate-error-patterns
         generate-api-request
         make-code-generator
         create-fix-strategies
         define-llm-providers
         meta-generate-auto-fix-system
         define-auto-fix-module
         find-error-handler
         try-pattern-fixes
         extract-variable-name
         call-llm-for-complex-fix
         generate-syntax-fix
         generate-variable-fix
         generate-type-fix
         generate-generic-fix
         string-trim
         string-contains?
         split-url
         parse-llm-response
         make-http-call
         error-pattern-map
         current-llm-client)

(define-syntax-rule (define-auto-fix-handler name error-type fix-strategy)
  (define name
    (lambda (condition code)
      (displayln (format "Auto-fixing ~a error..." 'error-type))
      (fix-strategy condition code))))

(define-syntax-rule (with-auto-fix expr)
  (call/cc
    (lambda (escape)
      (with-handlers ([exn:fail? (lambda (condition)
                                  (define error-msg (format "~a" condition))
                                  (define code-text (format "~s" 'expr))
                                  (define fixed-code (auto-fix-dispatcher condition code-text))
                                  (displayln (string-append "Original error: " error-msg))
                                  (displayln "Attempting fix...")
                                  (if (and fixed-code
                                           (not (string-contains? fixed-code "Auto-fix not available"))
                                           (not (string-contains? fixed-code "Auto-fix attempted")))
                                      (with-handlers ([exn? (lambda (e)
                                                             (displayln "Fix evaluation failed, using fallback")
                                                             (escape 'auto-fix-attempted))])
                                        (displayln (string-append "Fixed code: " fixed-code))
                                        (escape (eval (read (open-input-string fixed-code)))))
                                      (begin
                                        (displayln "Set ANTHROPIC_API_KEY for LLM integration")
                                        (escape 'auto-fix-attempted))))])
        expr))))

(define-syntax-rule (generate-llm-client api-endpoint model-name)
  (lambda (prompt)
    (define request-body (generate-api-request model-name prompt))
    (define response (make-http-call api-endpoint request-body))
    (parse-llm-response response)))

(define-syntax-rule (auto-fix-function name args body ...)
  (define name
    (lambda args
      (with-auto-fix
        (begin body ...)))))

(define-syntax-rule (generate-error-patterns (pattern handler) ...)
  (define error-pattern-map
    (list (cons pattern handler) ...)))

(define (generate-api-request model prompt)
  `#hash((model . ,model)
         (messages . (#hash((role . "user")
                            (content . ,(string-append
                                        "Fix this Racket code. Return only corrected code:\n"
                                        "Error context: " prompt)))))
         (max_tokens . 500)))

(define (make-code-generator error-type)
  (lambda (condition original-code)
    (case error-type
      [(syntax-error)
       (generate-syntax-fix condition original-code)]
      [(undefined-variable)
       (generate-variable-fix condition original-code)]
      [(type-error)
       (generate-type-fix condition original-code)]
      [else
       (generate-generic-fix condition original-code)])))

(define-syntax-rule (create-fix-strategies)
  (begin
    (define syntax-fix-generator (make-code-generator 'syntax-error))
    (define variable-fix-generator (make-code-generator 'undefined-variable))
    (define type-fix-generator (make-code-generator 'type-error))
    (define generic-fix-generator (make-code-generator 'generic))))

(define-syntax-rule (define-llm-providers (name endpoint model) ...)
  (begin
    (define name (generate-llm-client endpoint model)) ...))

(define (meta-generate-auto-fix-system)
  (set! error-pattern-map
        (list (cons "undefined.*variable" generate-variable-fix)
              (cons "syntax.*error" generate-syntax-fix)
              (cons "type.*error" generate-type-fix)
              (cons ".*" generate-generic-fix)))
  (set! current-llm-client (lambda (prompt) "Auto-fix not available")))

(define (auto-fix-dispatcher condition code)
  (define error-string (format "~a" condition))
  (define handler (find-error-handler error-string))
  (if handler
      (handler condition code)
      (current-llm-client (string-append "Error: " error-string "\nCode: " code))))

(define-syntax-rule (define-auto-fix-module name (export-list ...))
  (begin
    (meta-generate-auto-fix-system)
    (define name
      (list (cons 'export-list export-list) ...))))

(define (generate-syntax-fix condition code)
  (define patterns '(("missing.*paren" . "(begin ~a)")
                    ("extra.*paren" . "~a")
                    ("malformed.*quote" . "'~a")))
  (or (try-pattern-fixes patterns condition code)
      (call-llm-for-complex-fix condition code)))

(define (generate-variable-fix condition code)
  (define error-msg (format "~a" condition))
  (define var-name (extract-variable-name error-msg))
  (if var-name
      (string-append "(define " var-name " 'undefined-placeholder)\n" code)
      (call-llm-for-complex-fix condition code)))

(define (generate-type-fix condition code)
  (define type-conversions '(("string.*number" . "(string->number ~a)")
                            ("number.*string" . "(number->string ~a)")
                            ("list.*vector" . "(list->vector ~a)")))
  (or (try-pattern-fixes type-conversions condition code)
      (call-llm-for-complex-fix condition code)))

(define (generate-generic-fix condition code)
  (call-llm-for-complex-fix condition code))

(define (try-pattern-fixes patterns condition code)
  (let loop ([ps patterns])
    (if (null? ps)
        #f
        (let* ([pattern (caar ps)]
               [fix-template (cdar ps)]
               [error-string (format "~a" condition)])
          (if (regexp-match pattern error-string)
              (format fix-template code)
              (loop (cdr ps)))))))

(define current-llm-client (lambda (prompt) "Auto-fix not available"))
(define error-pattern-map '())

(define (call-llm-for-complex-fix condition code)
  (if current-llm-client
      (current-llm-client (string-append
                           "Fix this Racket code error:\n"
                           "Error: " (format "~a" condition) "\n"
                           "Code: " code))
      "Auto-fix not available (no LLM client configured)"))

(define (find-error-handler error-string)
  (let loop ([patterns error-pattern-map])
    (if (null? patterns)
        #f
        (let* ([pattern (caar patterns)]
               [handler (cdar patterns)])
          (if (regexp-match pattern error-string)
              handler
              (loop (cdr patterns)))))))

(define (extract-variable-name error-msg)
  (define match (or (regexp-match #rx"variable.*'([^']*)'" error-msg)
                    (regexp-match #rx"unbound identifier.*in: ([^\\s\\n]+)" error-msg)
                    (regexp-match #rx"undefined.*([a-zA-Z_][a-zA-Z0-9_-]*)" error-msg)))
  (if match
      (second match)
      "unknown-var"))

(define (make-http-call endpoint body)
  (define json-body (jsexpr->string body))
  (define api-key (or (getenv "ANTHROPIC_API_KEY") (getenv "OPENAI_API_KEY")))
  (if (not api-key)
      "Auto-fix not available (no API key configured)"
      (let* ([is-claude (string-contains? endpoint "anthropic")]
             [headers (if is-claude
                         `(("Content-Type" . "application/json")
                           ("x-api-key" . ,api-key)
                           ("anthropic-version" . "2023-06-01"))
                         `(("Content-Type" . "application/json")
                           ("Authorization" . ,(string-append "Bearer " api-key))))])
        (define-values (scheme host port path) (split-url endpoint))
        (define header-list (map (lambda (h) (string-append (car h) ": " (cdr h))) headers))
        (define post-data (string->bytes/utf-8 json-body))

        (with-handlers ([exn? (lambda (e) "Auto-fix not available (network error)")])
          (define-values (status response-headers response-port)
            (http-sendrecv host path
                           #:method "POST"
                           #:data post-data
                           #:headers header-list
                           #:ssl? (string=? scheme "https")))

          (bytes->string/utf-8 (port->bytes response-port))))))

(define (parse-llm-response response)
  (define json (string->jsexpr response))
  (define content (or (hash-ref json 'content #f)
                     (let ([choices (hash-ref json 'choices #f)])
                       (if (and choices (not (null? choices)))
                           (let* ([first-choice (first choices)]
                                  [message (hash-ref first-choice 'message #f)])
                             (if message
                                 (hash-ref message 'content #f)
                                 #f))
                           #f))))
  (if content
      (if (list? content)
          (string-trim (hash-ref (first content) 'text ""))
          (string-trim content))
      #f))

(define (string-trim str)
  (define len (string-length str))
  (define start
    (let loop ([i 0])
      (if (or (>= i len)
              (not (char-whitespace? (string-ref str i))))
          i
          (loop (+ i 1)))))
  (define end
    (let loop ([i (- len 1)])
      (if (or (< i 0)
              (not (char-whitespace? (string-ref str i))))
          (+ i 1)
          (loop (- i 1)))))
  (if (> start end) "" (substring str start end)))

(define (split-url url)
  "Split URL into components"
  (define uri (string->url url))
  (values (if (url-scheme uri)
              (let ([scheme (url-scheme uri)])
                (if (symbol? scheme) (symbol->string scheme) scheme))
              "https")
          (url-host uri)
          (url-port uri)
          (apply string-append (map (lambda (p) (string-append "/" p))
                                   (map path/param-path (url-path uri))))))

(define (string-contains? str substr)
  "Check if string contains substring"
  (define str-len (string-length str))
  (define sub-len (string-length substr))
  (let loop ([i 0])
    (cond
      [(> (+ i sub-len) str-len) #f]
      [(string=? (substring str i (+ i sub-len)) substr) #t]
      [else (loop (+ i 1))])))

(meta-generate-auto-fix-system)

(displayln "Metaprogramming auto-fix system loaded.")
(displayln "Usage: (with-auto-fix (your-expression))")
(displayln "       (auto-fix-function name args body...)")
