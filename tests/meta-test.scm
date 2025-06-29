(load "../src/meta-auto-fix.scm")

(define-auto-fix-module my-auto-fix-system
  (with-auto-fix auto-fix-function))

(auto-fix-function safe-add (x y)
  (+ x y))

(auto-fix-function safe-divide (x y)
  (/ x y))

(define (test-metaprogramming-auto-fix)
  (display "=== Testing Metaprogramming Auto-Fix ===") (newline)
  
  (display "1. Testing with-auto-fix macro:") (newline)
  (display "Result: ")
  (display (with-auto-fix (+ 1 2 3)))
  (newline)
  
  (display "2. Testing auto-fix-function:") (newline)
  (display "Safe add result: ")
  (display (safe-add 5 10))
  (newline)
  
  (display "3. Testing error case:") (newline)
  (display "Attempting division by zero with auto-fix...") (newline)
  (with-auto-fix (/ 10 0))
  
  (display "4. Testing undefined variable:") (newline)
  (with-auto-fix (+ undefined-var 5))
  
  (display "Set ANTHROPIC_API_KEY for LLM integration") (newline))

(test-metaprogramming-auto-fix)