# Scheme Auto-Fix System

A Scheme/Lisp implementation that provides automatic code fixing capabilities using Large Language Models (LLMs). The system can automatically detect and fix common programming errors in Scheme code by leveraging Anthropic's Claude AI assistance.

## Features

- **Automatic Error Detection**: Catches runtime errors and attempts to fix them automatically
- **LLM Integration**: Uses Anthropic Claude and OpenAI GPT APIs for intelligent code fixes
- **Metaprogramming Support**: Advanced macro system for generating fix strategies
- **Pattern Matching**: Built-in patterns for common error types (syntax, undefined variables, type errors)
- **Extensible Architecture**: Easy to add new error handlers and fix strategies

## Project Structure

```
.
├── src/
│   ├── auto-fix.scm         # Core auto-fix functionality
│   └── meta-auto-fix.scm    # Metaprogramming and macro system
├── tests/
│   ├── test-auto-fix.scm    # Basic functionality tests
│   └── meta-test.scm        # Metaprogramming tests
├── examples/                # Usage examples
└── docs/                   # Documentation
```

## Setup

1. Set your Anthropic API key:
   ```bash
   export ANTHROPIC_API_KEY=your-api-key-here
   ```

2. Load the system in your Scheme interpreter:
   ```scheme
   (load "src/auto-fix.scm")
   ```

## Usage

### Basic Auto-Fix

```scheme
;; Automatically fix and evaluate expressions
(safe-eval-with-fix '(+ 1 2 3))

;; Handle undefined variables
(safe-eval-with-fix '(+ undefined-var 5))

;; Fix type errors
(safe-eval-with-fix '(+ "hello" 5))
```

### Metaprogramming System

```scheme
;; Load the metaprogramming system
(load "src/meta-auto-fix.scm")

;; Use the with-auto-fix macro
(with-auto-fix (/ 10 0))

;; Define auto-fixing functions
(auto-fix-function safe-divide (x y)
  (/ x y))
```

## Error Types Supported

- **Syntax Errors**: Missing/extra parentheses, malformed expressions
- **Undefined Variables**: Automatic placeholder generation
- **Type Errors**: Automatic type conversion suggestions
- **Generic Errors**: LLM-powered fixes for complex issues

## API Keys

The system supports multiple LLM providers:
- Anthropic Claude models (set `ANTHROPIC_API_KEY`) - **Primary/Default**
- OpenAI GPT models (set `OPENAI_API_KEY`) - **Fallback**

## Testing

Run the test suite:
```bash
# Basic functionality tests
scheme tests/test-auto-fix.scm

# Metaprogramming tests
scheme tests/meta-test.scm
```
