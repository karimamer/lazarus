# Auto-Fix System for Racket & Scheme AKA lazarus

This is just an experiment. 
This experiment contains a dual-language implementation providing automatic code fixing capabilities using LLMs. This system can automatically detect, analyze, and fix common programming errors by leveraging AI assistance from Anthropic Claude and OpenAI GPT models.


## Features

- **🔧 Automatic Error Detection & Fixing**: Catches runtime errors and attempts fixes
- **🎯 Metaprogramming Support**:  macro system for generating custom fixes
- **📝 Pattern Matching**: Built-in patterns for common error types (syntax, undefined variables, type errors)
- **🔄 Dual Implementation**: Complete implementations in both Racket and Scheme

## Project Structure

```
experiment/
├── src/
│   ├── racket/
│   │   ├── auto-fix.rkt         # Core auto-fix functionality (Racket)
│   │   └── meta-auto-fix.rkt    # Metaprogramming system (Racket)
│   └── scheme/
│       ├── auto-fix.scm         # Core auto-fix functionality (Scheme)
│       └── meta-auto-fix.scm    # Metaprogramming system (Scheme)
├── tests/
│   ├── racket/
│   │   ├── test-auto-fix.rkt    # Basic functionality tests (Racket)
│   │   └── meta-test.rkt        # Metaprogramming tests (Racket)
│   └── scheme/
│       ├── test-auto-fix.scm    # Basic functionality tests (Scheme)
│       └── meta-test.scm        # Metaprogramming tests (Scheme)
├── examples/                    # Usage examples and demos
├── docs/                       # Additional documentation
└── README.md                   # This file
```

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

### Expected Output
Both test suites will demonstrate:
- ✅ Normal expression evaluation
- ✅ Error detection and handling
- ✅ Auto-fix attempts (with or without LLM)
- ✅ Metaprogramming macro functionality

## Error Types Supported

| Error Type | Description | Fix Strategy |
|------------|-------------|--------------|
| **Syntax Errors** | Missing/extra parentheses, malformed expressions | Pattern-based fixes + LLM analysis |
| **Undefined Variables** | Reference to unbound identifiers | Placeholder generation, scope analysis |
| **Type Errors** | Incorrect argument types | Automatic type conversion suggestions |
| **Division by Zero** | Mathematical exceptions | Safe fallback values |
| **Generic Errors** | Complex or unknown issues | LLM-powered intelligent analysis |

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

### Offline Mode
The system gracefully degrades to pattern-based fixes when no API keys are configured, ensuring basic functionality without external dependencies.

## Advanced Features

### Custom Error Patterns
```racket
;; Define custom error detection patterns
(generate-error-patterns
  ("custom-error-regex" . custom-fix-handler)
  ("another-pattern" . another-handler))
```

### LLM Client Generation
```racket
;; Create custom LLM clients
(generate-llm-client "https://api.example.com/v1/chat" "custom-model")
```

### Fix Strategy Creation
```racket
;; Generate specialized fix strategies
(create-fix-strategies)
(define my-fixer (make-code-generator 'custom-error-type))
```

## Development

### Adding New Error Types
1. Define pattern in `generate-error-patterns`
2. Implement fix function following the signature: `(condition code) -> fixed-code`
3. Register in the error pattern map
4. Add tests in the appropriate test file

### Extending LLM Support
1. Implement client in `generate-llm-client`
2. Add API-specific request/response handling
3. Update authentication headers as needed


**🚀 Quick Start**: `racket tests/racket/meta-test.rkt` or `scheme tests/scheme/meta-test.scm`
