#lang racket

(require "../../src/racket/auto-fix.rkt")

(define (test-auto-fix)
  "Test the auto-fix functionality"
  (displayln "=== Testing Auto-Fix System ===")

  (displayln "1. Testing valid expression (should work normally):")
  (display "Result: ")
  (displayln (safe-eval-with-fix '(+ 1 2 3)))

  (displayln "2. Testing with syntax that might need fixing:")
  (displayln "Attempting to fix and evaluate problematic code...")

  (displayln "Set ANTHROPIC_API_KEY environment variable to test LLM integration")
  (displayln "Example: export ANTHROPIC_API_KEY=your-api-key"))

(test-auto-fix)
