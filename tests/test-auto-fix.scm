(load "../src/auto-fix.scm")

(define (test-auto-fix)
  "Test the auto-fix functionality"
  (display "=== Testing Auto-Fix System ===") (newline)
  
  (display "1. Testing valid expression (should work normally):") (newline)
  (display "Result: ") (display (safe-eval-with-fix '(+ 1 2 3))) (newline)
  
  (display "2. Testing with syntax that might need fixing:") (newline)
  (display "Attempting to fix and evaluate problematic code...") (newline)
  
  (display "Set ANTHROPIC_API_KEY environment variable to test LLM integration") (newline)
  (display "Example: export ANTHROPIC_API_KEY=your-api-key") (newline))

(test-auto-fix)