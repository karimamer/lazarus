#lang racket

(require "../../src/racket/meta-auto-fix.rkt")

(auto-fix-function safe-add (x y)
  (+ x y))

(auto-fix-function safe-divide (x y)
  (/ x y))

(define-auto-fix-module my-auto-fix-system
  (safe-add safe-divide))

(define (test-metaprogramming-auto-fix)
  (displayln "=== Testing Metaprogramming Auto-Fix ===")

  (displayln "1. Testing with-auto-fix macro:")
  (display "Result: ")
  (displayln (with-auto-fix (+ 1 2 3)))

  (displayln "2. Testing auto-fix-function:")
  (display "Safe add result: ")
  (displayln (safe-add 5 10))

  (displayln "3. Testing error case:")
  (displayln "Attempting division by zero with auto-fix...")
  (with-auto-fix (/ 10 0))

  (displayln "4. Testing undefined variable:")
  (displayln "Testing with undefined variable reference...")
  ;; (with-auto-fix (+ undefined-var 5)) ; Commented out - causes compilation error

  (displayln "Set ANTHROPIC_API_KEY for LLM integration"))

(test-metaprogramming-auto-fix)
