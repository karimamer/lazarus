(define (make-http-request url method headers body)
  "Make HTTP request using curl"
  (let* ((header-args (apply append 
                            (map (lambda (h) 
                                   (list "-H" (string-append (car h) ": " (cdr h))))
                                 headers)))
         (curl-cmd (append '("curl" "-s" "-X") 
                          (list method)
                          header-args
                          (if body (list "-d" body) '())
                          (list url)))
         (cmd-string (string-join curl-cmd " ")))
    (call-with-input-string 
     (with-output-to-string
       (lambda () (system cmd-string)))
     read-string)))

(define (call-llm-for-fix error-msg code-snippet)
  "Call Claude API to get a fix for the error"
  (let* ((api-key (or (getenv "ANTHROPIC_API_KEY") 
                      (error "ANTHROPIC_API_KEY environment variable not set")))
         (url "https://api.anthropic.com/v1/messages")
         (headers `(("x-api-key" . ,api-key)
                   ("Content-Type" . "application/json")
                   ("anthropic-version" . "2023-06-01")))
         (prompt (string-append 
                  "Fix this Scheme code error. Return only the corrected code, no explanation:\n\n"
                  "Error: " error-msg "\n\n"
                  "Code: " code-snippet))
         (body (string-append 
                "{"
                "\"model\": \"claude-3-sonnet-20240229\","
                "\"max_tokens\": 500,"
                "\"messages\": [{\"role\": \"user\", \"content\": \"" prompt "\"}]"
                "}"))
         (response (make-http-request url "POST" headers body)))
    (extract-fixed-code response)))

(define (extract-fixed-code response)
  "Extract the fixed code from Claude response"
  (let* ((json-start (string-contains response "\"text\":"))
         (content-start (+ json-start 8))
         (content-end (string-contains response "\"" content-start))
         (content (substring response content-start content-end)))
    (string-trim content)))

(define (safe-eval-with-fix expr)
  "Evaluate expression with auto-fix on error"
  (call/cc
    (lambda (return)
      (with-exception-handler
        (lambda (condition)
          (let* ((error-msg (condition->string condition))
                 (code-snippet (format "~s" expr))
                 (fixed-code (call-llm-for-fix error-msg code-snippet)))
            (display "Error encountered: ") (display error-msg) (newline)
            (display "Attempting auto-fix...") (newline)
            (display "Fixed code: ") (display fixed-code) (newline)
            (let ((fixed-expr (call-with-input-string fixed-code read)))
              (return (eval fixed-expr)))))
        (lambda ()
          (eval expr))))))

(define (auto-fix-program program-text)
  "Run a program with auto-fix capability"
  (let ((expressions (call-with-input-string program-text
                       (lambda (port)
                         (let loop ((exprs '()))
                           (let ((expr (read port)))
                             (if (eof-object? expr)
                                 (reverse exprs)
                                 (loop (cons expr exprs)))))))))
    (for-each safe-eval-with-fix expressions)))

(define (condition->string condition)
  "Convert condition to string"
  (call-with-output-string
    (lambda (port)
      (display condition port))))

(define (string-contains str substr)
  "Check if string contains substring"
  (let ((str-len (string-length str))
        (sub-len (string-length substr)))
    (let loop ((i 0))
      (cond
        ((> (+ i sub-len) str-len) #f)
        ((string=? (substring str i (+ i sub-len)) substr) i)
        (else (loop (+ i 1)))))))

(define (string-trim str)
  "Trim whitespace from string"
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (or (>= i len) (not (char-whitespace? (string-ref str i))))
                      i
                      (loop (+ i 1)))))
         (end (let loop ((i (- len 1)))
                (if (or (< i 0) (not (char-whitespace? (string-ref str i))))
                    (+ i 1)
                    (loop (- i 1))))))
    (if (> start end)
        ""
        (substring str start end))))

(define (string-join lst delimiter)
  "Join list of strings with delimiter"
  (if (null? lst)
      ""
      (fold-right (lambda (x acc)
                    (if (string=? acc "")
                        x
                        (string-append x delimiter acc)))
                  ""
                  lst)))

(define (example-usage)
  "Example of using the auto-fix system"
  (display "Testing auto-fix system...") (newline)
  
  (display "Test 1: Syntax error") (newline)
  (safe-eval-with-fix '(+ 1 2 3))
  
  (display "Test 2: Undefined variable") (newline)
  (safe-eval-with-fix '(+ undefined-var 5))
  
  (display "Test 3: Type error") (newline)
  (safe-eval-with-fix '(+ "hello" 5)))

(display "Auto-fix Scheme program loaded.") (newline)
(display "Usage: (safe-eval-with-fix '(your-expression))") (newline)
(display "Example: (example-usage)") (newline)