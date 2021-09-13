# EPL
A programming language that resembles English. Write some sentences and watch the interpreter try to understand what you meant.

![Version](https://img.shields.io/badge/version-v3.9.0-blue)
![License](https://img.shields.io/badge/license-MIT-brightgreen)

# Introduction
The goal of this project is to make a language that is as similar as possible to plain English. Natural language is ambiguous and hard to understand for computers, so we apply a series of rules and constraints to it. With them, we end up with a language that looks like English

[Here](docs/syntax.md) is the documentation for the language's syntax.

[Here](docs/prelude.md) is the documentation for the language's prelude.

[Here](examples) are some examples of EPL programs.

# Running the interpreter
Requirements: [Stack](https://docs.haskellstack.org/en/stable/README/)

To run a program, for example `program.epl`, simply use:
```
stack run -- program.epl
```


# Work in progress
- Adding more built-in functions.
- Adding "break" for loops and "exit" for procedures (stop the loop and exit?).
- Implementing command line arguments for the interpreter.

# Possible improvements:
- Finding a better syntax for specifying function return types
- Implementing importing other files as libraries
- Adding records to the language.


