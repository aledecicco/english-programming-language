# EPL
A programming language that resembles English. Write some sentences and watch the interpreter try to understand what you meant.

![Version](https://img.shields.io/badge/version-v3.14.0-blue)
![License](https://img.shields.io/badge/license-MIT-brightgreen)

# Introduction
The goal of this project is to make a language that is as similar as possible to plain English. Natural language is ambiguous, so we apply a series of rules and constraints to it. With them, we end up with a language that looks like English, but that is stable enough to be understood by a computer.

Of course, nothing can stop you from writing code that is obfuscated or not similar at all to a text in english, but it's up to you to write EPL programs in a readable way,

Here is a snippet with some sentences that the interpreter can understand:
```
Let L be the list from 0 to 5.
Add 1 to each number in L.
Print each element in L plus 2.
```

[Here](docs/syntax.md) is the full documentation for the language's syntax.

[Here](docs/prelude.md) is the documentation for the language's prelude.

[Here](examples) are some examples of EPL programs.

# Features
- Spaces are allowed in variable and function names.
- Arguments can be passed by value or by reference.
- Garbage collector.
- Try-catch, loops, and conditionals.
- Optional function parameter names, with aliases generation.

# Work in progress
- Adding more built-in functions.
- Special variable "it" for referencing the result of the previous sentence.
- Built-in filtering with callbacks as arguments.
- Reading from terminal.

# Possible improvements:
- Finding a better syntax for specifying function return types.
- Implementing importing other files as libraries.
- Adding records to the language.
- Implementing an interactive environment.

# Usage
Requirements: [Stack](https://docs.haskellstack.org/en/stable/README/)

To run a program, for example `program.epl`, simply use:
```
stack run -- program.epl
```

The `-h` or `--help` option displays usage help.

The `-s` or `--silent` option runs the interpreter without printing warnings or success messages.


