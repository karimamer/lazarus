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

; Create the fix generators at top level
(create-fix-strategies)

; Global variables that will be initialized by meta-generate-auto-fix-system
(define current-llm-client
  (lambda (prompt) "Auto-fix not available (no LLM client configured)"))
(define error-pattern-map '())

(define (meta-generate-auto-fix-system)
  (set! error-pattern-map
    (list (cons "undefined.*variable" generate-variable-fix)
          (cons "syntax.*error" generate-syntax-fix)
          (cons "type.*error" generate-type-fix)
          (cons ".*" generate-generic-fix)))

  ; Set up a simple fallback LLM client
  (set! current-llm-client
    (lambda (prompt)
      (string-append "Auto-fix attempted for: "
                     (substring prompt 0 (min 50 (string-length prompt)))
                     "..."))))

(define (auto-fix-dispatcher condition code)
  (let* ((error-string (format "~a" condition))
         (handler (find-error-handler error-string)))
    (if handler
        (handler condition code)
        (let ((llm-result (current-llm-client (string-append "Error: " error-string "\nCode: " code))))
          (if llm-result llm-result "Auto-fix not available")))))

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
  (let* ((error-msg (format "~a" condition))
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

(define (string-contains? haystack needle)
  (let ((hay-len (string-length haystack))
        (needle-len (string-length needle)))
    (and (<= needle-len hay-len)
         (let loop ((i 0))
           (cond ((> (+ i needle-len) hay-len) #f)
                 ((string=? (substring haystack i (+ i needle-len)) needle) #t)
                 (else (loop (+ i 1))))))))

(define (string-match pattern string)
  ;; Simple fallback for when regexp is not available
  (cond
    ((string-contains? pattern "undefined") (string-contains? string "undefined"))
    ((string-contains? pattern "syntax") (string-contains? string "syntax"))
    ((string-contains? pattern "type") (string-contains? string "type"))
    (else #t))) ; matches ".*" pattern

(define (extract-variable-name error-msg)
  ;; Simple fallback - just return a placeholder
  "unknown-var")

(define (read-from-string str)
  ;; Simple fallback - return the string as is
  str)

(define (make-http-call endpoint body)
  (let* ((json-body (scm->json body))
         (api-key (or (getenv "ANTHROPIC_API_KEY") (getenv "OPENAI_API_KEY")))
         (is-claude (string-contains? endpoint "anthropic"))
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
    ;; Fallback when system* is not available
    "Auto-fix attempted (LLM not available in this Scheme implementation)"))

(define (parse-llm-response response)
  ;; Simple fallback - just return the response as is
  response)

(define (scm->json obj)
  ;; Simplified JSON conversion that avoids complex cases
  (cond
    ((string? obj) (string-append "\"" obj "\""))
    ((number? obj) (number->string obj))
    ((boolean? obj) (if obj "true" "false"))
    (else (string-append "\"" (format "~a" obj) "\""))))

(define (json->scm str)
  ;; Simple fallback - just return the string
  str)

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
