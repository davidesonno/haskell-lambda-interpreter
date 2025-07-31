# Haskell Lambda Calculus Interpreters

A comprehensive implementation of both **untyped** and **typed** lambda calculus interpreters written in Haskell. This project demonstrates various evaluation strategies, type checking, and lambda calculus encodings including Church numerals, booleans, and pairs.

## Features

### Untyped Lambda Calculus

- **Multiple Evaluation Strategies**:
  - Call-by-Value (CBV)
  - Call-by-Name (CBN)
  - Normal Order reduction
- **Lambda Calculus Encodings**:
  - Church Booleans (`true`, `false`, `and`)
  - Church Numerals (zero, successor, addition, multiplication)
  - Church Pairs (`pair`, `fst`, `snd`)
- **Advanced Operations**:
  - Variable renaming with capture avoidance
  - Substitution with α-conversion
  - Free variable computation
  - Pretty printing with step-by-step evaluation

### Typed Lambda Calculus

- **Type System**:
  - Integer types (`TInt`)
  - Function types (`T1 -> T2`)
  - Product types (`T1 * T2`)
- **Language Constructs**:
  - Arithmetic operations (`+`, `-`, `*`)
  - Conditional expressions (`if-then-else`)
  - Let bindings
  - Recursive functions (`rec`)
  - Pairs and projections (`fst`, `snd`)
- **Type Checking**: Complete type inference and error reporting
- **Evaluation**: Call-by-value evaluation with type safety

## Repository Structure

```
src/
├── Untyped/
│   ├── Expr.hs          # Core expression ADT and evaluation
│   ├── Bool.hs          # Church boolean encodings
│   ├── Numerals.hs      # Church numeral encodings
│   ├── Pair.hs          # Church pair encodings
│   └── Utils.hs         # Pretty printing and evaluation utilities
├── Typed/
│   ├── Expr.hs          # Typed expression ADT and evaluation
│   ├── Type.hs          # Type system definitions
│   └── Utils.hs         # Typed expression utilities
├── Untyped.hs           # Untyped module exports
└── Typed.hs             # Typed module exports

test/
├── UntypedTest.hs       # Comprehensive untyped lambda calculus tests
└── TypedTest.hs         # Typed lambda calculus examples and tests

Main.hs                  # Entry point
ghci.ps1                 # PowerShell script for easy GHCi loading
```

## Usage

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- PowerShell (for the convenience script)

### Quick Start

1. **Using the PowerShell script** (recommended):

   ```powershell
   .\ghci.ps1 Main.hs
   ```
2. **Manual GHCi launch**:

   ```bash
   ghci -i./src Main.hs
   ```
3. **Loading test modules**:

   ```powershell
   .\ghci.ps1 test/UntypedTest.hs
   .\ghci.ps1 test/TypedTest.hs
   ```

### Examples

#### Untyped Lambda Calculus

```haskell
-- Church numerals
ghci> prettyCbnEval (plus (scc zero) (scc (scc zero)))  -- 1 + 2
ghci> showNumbers 5  -- Display first 5 Church numerals

-- Boolean operations
ghci> cbnEval (and_ tru fls)  -- Church boolean AND

-- Pairs
ghci> prettyCbnEval (fst_ (pair x y))  -- Extract first element
```

#### Typed Lambda Calculus

```haskell
-- Simple arithmetic
ghci> prettyEvalSteps (Add (Lit 3) (Lit 5))

-- Function application
ghci> prettyEvalSteps (Appl (Lam "x" TInt (Add (Var "x") (Lit 1))) (Lit 5))

-- Recursive factorial
ghci> prettyEvalSteps (fact 5)  -- Factorial of 5

-- Type checking
ghci> typecheck (Lam "x" TInt (Add (Var "x") (Lit 1)))
```

## Key Algorithms

### Substitution with Capture Avoidance

The implementation includes sophisticated variable substitution that automatically performs α-conversion to avoid variable capture:

```haskell
sub "x" (Var "y") (Lam "y" (Appl (Var "x") (Var "y")))
-- Result: Lam "y'" (Appl (Var "y") (Var "y'"))
```

### Multiple Evaluation Strategies

- **Call-by-Value**: Arguments are evaluated before function application
- **Call-by-Name**: Arguments are passed unevaluated (lazy evaluation)
- **Normal Order**: Leftmost, outermost redex reduction

### Type Inference

Complete type checking with informative error messages for the typed lambda calculus.
