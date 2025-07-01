# Auto-Fix System for Racket & Scheme AKA lazarus

This serves as an experimental project and provides an opportunity to revisit Scheme and Racket.

## Prerequisites

### For Racket
- **Racket 8.0+** installed
- Required packages: `net/http-client`, `json`, `racket/system`

### For Scheme
- **Chez Scheme 9.5+** (recommended) or compatible Scheme implementation
- Basic R6RS/R7RS support

### API Access (Optional but Recommended)
- Anthropic API key for Claude models (primary)
- OpenAI API key for GPT models (fallback)

## Setup

1. **Configure API Keys** (for LLM integration):
   ```bash
   export ANTHROPIC_API_KEY=your-anthropic-api-key-here
   # Optional fallback:
   export OPENAI_API_KEY=your-openai-api-key-here
   ```

2. **Clone and Navigate**:
   ```bash
   git clone <repository-url>
   cd experiment
   ```

## Usage

### Basic Auto-Fix System

#### Racket
```racket
#lang racket
(require "src/racket/auto-fix.rkt")

;; Evaluate with automatic error fixing
(safe-eval-with-fix '(+ 1 2 3))        ; Works normally: 6

;; Handle undefined variables
(safe-eval-with-fix '(+ undefined-var 5))  ; Attempts to fix

;; Fix type errors
(safe-eval-with-fix '(+ "hello" 5))     ; Suggests type conversion

;; Run example demonstrations
(example-usage)
```

#### Scheme
```scheme
(load "src/scheme/auto-fix.scm")

;; Same syntax as Racket
(safe-eval-with-fix '(+ 1 2 3))
(safe-eval-with-fix '(+ undefined-var 5))
(example-usage)
```

### Metaprogramming System

#### Racket
```racket
#lang racket
(require "src/racket/meta-auto-fix.rkt")

;; Use the with-auto-fix macro for expressions
(with-auto-fix (/ 10 0))               ; Handles division by zero

;; Define functions with built-in error handling
(auto-fix-function safe-divide (x y)
  (/ x y))

(safe-divide 10 0)                     ; Automatically handles errors

;; Create custom error handlers
(define-auto-fix-handler my-handler syntax-error
  (lambda (condition code)
    (displayln "Custom syntax fix applied")))
```

#### Scheme
```scheme
(load "src/scheme/meta-auto-fix.scm")

;; Same macro syntax
(with-auto-fix (/ 10 0))

;; Define auto-fixing functions
(auto-fix-function safe-divide (x y)
  (/ x y))

;; Create custom fix modules
(define-auto-fix-module my-system
  (safe-divide safe-multiply))
```

## Testing

Run the comprehensive test suite:

### Racket Tests
```bash
# From project root directory:
racket tests/racket/test-auto-fix.rkt    # Basic functionality
racket tests/racket/meta-test.rkt        # Metaprogramming features
```

### Scheme Tests
```bash
# From project root directory:
scheme tests/scheme/test-auto-fix.scm    # Basic functionality
scheme tests/scheme/meta-test.scm        # Metaprogramming features
```

## API Configuration

### Anthropic Claude (Primary)
```bash
export ANTHROPIC_API_KEY=your-key
# Uses: claude-3-sonnet-20240229 model
# Endpoint: https://api.anthropic.com/v1/messages
```

### OpenAI GPT (Fallback)
```bash
export OPENAI_API_KEY=your-key
# Uses: gpt-3.5-turbo model
# Endpoint: https://api.openai.com/v1/chat/completions
```
