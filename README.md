# EPL
A programming language that resembles English. Write some sentences and watch the interpreter try to understand what you meant.

![Version](https://img.shields.io/badge/version-v3.10.0-blue)
![License](https://img.shields.io/badge/license-MIT-brightgreen)

# Introduction
The goal of this project is to make a language that is as similar as possible to plain English. Natural language is ambiguous, so we apply a series of rules and constraints to it. With them, we end up with a language that looks like English, but that is stable enough to be understood by a computer.

Here are some sentences that the interpreter can understand:
```
Let L be the
Set
```

[Here](docs/syntax.md) is the full documentation for the language's syntax.

[Here](docs/prelude.md) is the documentation for the language's prelude.

[Here](examples) are some examples of EPL programs.

# Usage
Requirements: [Stack](https://docs.haskellstack.org/en/stable/README/)

To run a program, for example `program.epl`, simply use:
```
stack run -- program.epl
```

The `-h` or `--help` option displays usage help.

The `-s` or `--silent` option runs the interpreter without printing warnings or success messages.

# Features
- Spaces are allowed in variable and function names.
- Arguments can be passed by value or by reference.
- Garbage collector.
- Try-catch, loops and conditionals.
- Optional function parameter names, with aliases generation.

# Work in progress
- Adding more built-in functions.
- Adding "break" for loops and "exit" for procedures ("stop the loop" and "exit"?).
- Special variable "it" for referencing the result of the previous sentence.

# Possible improvements:
- Finding a better syntax for specifying function return types.
- Implementing importing other files as libraries.
- Adding records to the language.
- Implementing an interactive environment.

