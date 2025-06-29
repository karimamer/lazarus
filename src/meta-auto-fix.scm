(define-syntax define-auto-fix-handler
  (syntax-rules ()
    ((define-auto-fix-handler name error-type fix-strategy)
     (define name
       (lambda (condition code)
         (display "Auto-fixing ") (display 'error-type) (display " error...") (newline)
         (fix-strategy condition code))))))

(define-syntax with-auto-fix
  (syntax-rules ()
    ((with-auto-fix expr)
     (call/cc
       (lambda (escape)
         (with-exception-handler
           (lambda (condition)
             (let* ((error-msg (format "~a" condition))
                    (code-text (format "~s" 'expr))
                    (fixed-code (auto-fix-dispatcher condition code-text)))
               (display "Original error: ") (display error-msg) (newline)
               (display "Attempting fix...") (newline)
               (if fixed-code
                   (begin
                     (display "Fixed code: ") (display fixed-code) (newline)
                     (escape (eval (read-from-string fixed-code))))
                   (begin
                     (display "Could not auto-fix") (newline)
                     (raise condition)))))
           (lambda () expr)))))))

(define-syntax generate-llm-client
  (syntax-rules ()
    ((generate-llm-client api-endpoint model-name)
     (lambda (prompt)
       (let* ((request-body (generate-api-request model-name prompt))
              (response (make-http-call api-endpoint request-body)))
         (parse-llm-response response))))))

(define-syntax auto-fix-function
  (syntax-rules ()
    ((auto-fix-function name args body ...)
     (define name
       (lambda args
         (with-auto-fix
           (begin body ...)))))))

(define-syntax generate-error-patterns
  (syntax-rules ()
    ((generate-error-patterns (pattern handler) ...)
     (define error-pattern-map
       (list (cons pattern handler) ...)))))

(define (generate-api-request model prompt)
  `((model . ,model)
    (messages . (((role . "user") 
                  (content . ,(string-append 
                              "Fix this Scheme code. Return only corrected code:\n"
                              "Error context: " prompt)))))
    (max_tokens . 500)))

(define (make-code-generator error-type)
  (lambda (condition original-code)
    (case error-type
      ((syntax-error)
       (generate-syntax-fix condition original-code))
      ((undefined-variable)
       (generate-variable-fix condition original-code))
      ((type-error)
       (generate-type-fix condition original-code))
      (else
       (generate-generic-fix condition original-code)))))

(define-syntax create-fix-strategies
  (syntax-rules ()
    ((create-fix-strategies)
     (begin
       (define syntax-fix-generator (make-code-generator 'syntax-error))
       (define variable-fix-generator (make-code-generator 'undefined-variable))
       (define type-fix-generator (make-code-generator 'type-error))
       (define generic-fix-generator (make-code-generator 'generic))))))

(define-syntax define-llm-providers
  (syntax-rules ()
    ((define-llm-providers (name endpoint model) ...)
     (begin
       (define name (generate-llm-client endpoint model)) ...))))

(define (meta-generate-auto-fix-system)
  (eval 
    `(begin
       (create-fix-strategies)
       
       (define-llm-providers
         (claude-client "https://api.anthropic.com/v1/messages" "claude-3-sonnet-20240229")
         (openai-client "https://api.openai.com/v1/chat/completions" "gpt-3.5-turbo"))
       
       (generate-error-patterns
         ("undefined.*variable" variable-fix-generator)
         ("syntax.*error" syntax-fix-generator) 
         ("type.*error" type-fix-generator)
         (".*" generic-fix-generator))
       
       (define current-llm-client claude-client)
       
       (define (auto-fix-dispatcher condition code)
         (let* ((error-string (format "~a" condition))
                (handler (find-error-handler error-string)))
           (if handler
               (handler condition code)
               (current-llm-client (string-append "Error: " error-string "\nCode: " code))))))))

(define-syntax define-auto-fix-module
  (syntax-rules ()
    ((define-auto-fix-module name (export-list ...))
     (begin
       (meta-generate-auto-fix-system)
       (define name
         (list (cons 'export-list export-list) ...))))))

(define (generate-syntax-fix condition code)
  (let ((patterns '(("missing.*paren" . "(begin ~a)")
                   ("extra.*paren" . "~a")
                   ("malformed.*quote" . "'~a"))))
    (or (try-pattern-fixes patterns condition code)
        (call-llm-for-complex-fix condition code))))

(define (generate-variable-fix condition code)
  (let* ((error-msg (format "~a condition"))
         (var-name (extract-variable-name error-msg)))
    (if var-name
        (string-append "(define " var-name " 'undefined-placeholder)\n" code)
        (call-llm-for-complex-fix condition code))))

(define (generate-type-fix condition code)
  (let ((type-conversions '(("string.*number" . "(string->number ~a)")
                           ("number.*string" . "(number->string ~a)")
                           ("list.*vector" . "(list->vector ~a)"))))
    (or (try-pattern-fixes type-conversions condition code)
        (call-llm-for-complex-fix condition code))))

(define (generate-generic-fix condition code)
  (call-llm-for-complex-fix condition code))

(define (try-pattern-fixes patterns condition code)
  (let loop ((ps patterns))
    (if (null? ps)
        #f
        (let* ((pattern (caar ps))
               (fix-template (cdar ps))
               (error-string (format "~a" condition)))
          (if (string-match pattern error-string)
              (format fix-template code)
              (loop (cdr ps)))))))

(define (call-llm-for-complex-fix condition code)
  (current-llm-client (string-append 
                       "Fix this Scheme code error:\n"
                       "Error: " (format "~a" condition) "\n"
                       "Code: " code)))

(define (find-error-handler error-string)
  (let loop ((patterns error-pattern-map))
    (if (null? patterns)
        #f
        (let* ((pattern (caar patterns))
               (handler (cdar patterns)))
          (if (string-match pattern error-string)
              handler
              (loop (cdr patterns)))))))

(define (string-match pattern string)
  (let ((regex (make-regexp pattern)))
    (regexp-exec regex string)))

(define (extract-variable-name error-msg)
  (let ((match (string-match "variable.*'([^']*)" error-msg)))
    (if match
        (match:substring match 1)
        #f)))

(define (read-from-string str)
  (call-with-input-string str read))

(define (make-http-call endpoint body)
  (let* ((json-body (scm->json body))
         (api-key (or (getenv "ANTHROPIC_API_KEY") (getenv "OPENAI_API_KEY")))
         (is-claude (string-contains endpoint "anthropic"))
         (headers (if is-claude
                     `(("Content-Type" . "application/json")
                       ("x-api-key" . ,api-key)
                       ("anthropic-version" . "2023-06-01"))
                     `(("Content-Type" . "application/json")
                       ("Authorization" . ,(string-append "Bearer " api-key)))))
         (curl-cmd (append (list "curl" "-s" "-X" "POST")
                          (apply append (map (lambda (h)
                                              (list "-H" (string-append (car h) ": " (cdr h))))
                                            headers))
                          (list "-d" json-body endpoint))))
    (with-output-to-string
      (lambda ()
        (apply system* curl-cmd)))))

(define (parse-llm-response response)
  (let* ((json (json->scm response))
         (content (or (assoc-ref json "content")
                     (let ((choices (assoc-ref json "choices")))
                       (if (and choices (not (null? choices)))
                           (let* ((first-choice (car choices))
                                  (message (assoc-ref first-choice "message")))
                             (if message
                                 (assoc-ref message "content")
                                 #f))
                           #f)))))
    (if content
        (if (list? content)
            (string-trim (assoc-ref (car content) "text"))
            (string-trim content))
        #f)))

(define (scm->json obj)
  (cond
    ((list? obj)
     (string-append "{"
                   (string-join
                    (map (lambda (pair)
                           (string-append "\"" (symbol->string (car pair)) "\":"
                                        (scm->json (cdr pair))))
                         obj)
                    ",") "}"))
    ((string? obj) (string-append "\"" obj "\""))
    ((number? obj) (number->string obj))
    ((boolean? obj) (if obj "true" "false"))
    ((vector? obj) 
     (string-append "["
                   (string-join (map scm->json (vector->list obj)) ",")
                   "]"))
    (else (string-append "\"" (format "~a" obj) "\""))))

(define (json->scm str)
  (call-with-input-string str
    (lambda (port)
      (read port))))

(define (string-trim str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (or (>= i len) 
                         (not (char-whitespace? (string-ref str i))))
                      i
                      (loop (+ i 1)))))
         (end (let loop ((i (- len 1)))
                (if (or (< i 0) 
                       (not (char-whitespace? (string-ref str i))))
                    (+ i 1)
                    (loop (- i 1))))))
    (if (> start end) "" (substring str start end))))

(define (string-join lst delimiter)
  (if (null? lst)
      ""
      (fold-right (lambda (x acc)
                    (if (string=? acc "")
                        x
                        (string-append x delimiter acc)))
                  ""
                  lst)))

(meta-generate-auto-fix-system)

(display "Metaprogramming auto-fix system loaded.") (newline)
(display "Usage: (with-auto-fix (your-expression))") (newline)
(display "       (auto-fix-function name args body...)") (newline)