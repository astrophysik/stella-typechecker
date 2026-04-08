# Stella Typechecker

A typechecker for the [Stella](https://fizruk.github.io/stella/) programming language, implemented in Haskell.

## Overview

This project provides a typechecker for the Stella language. Stella is a pedagogical functional language designed for teaching type systems. See the [Stella documentation](https://fizruk.github.io/stella/) for language details.

## Building

This project uses [Stack](https://docs.haskellstack.org/) as its build system.

```bash
# Build the project
stack build
```

## Usage

### Command Line

```bash
# Typecheck a file
stack run -- --input program.stella

# Parse only (print AST without typechecking)
stack run -- --input program.stella --parse

# Read from stdin
echo 'language core; fn main(n : Nat) -> Nat { return n; }' | stack run
```

### Library Usage

```haskell
import Parsing.LexSyntax (tokens)
import Parsing.ParSyntax (pProgram)
import TypeCheck.TypeCheck (typeCheck)

-- Typecheck a program
main :: IO ()
main = do
    src <- readFile "program.stella"
    case pProgram (tokens src) of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right ast -> case typeCheck ast of
            Left err -> putStrLn $ "Type error: " ++ err
            Right () -> putStrLn "Well-typed!"
```

## Testing

```bash
# Run all tests
stack test

# Run with verbose output
stack test --verbose
```

## License

BSD-3-Clause
