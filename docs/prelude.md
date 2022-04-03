# Built-in functions

This is an extensive list of all the built-in functions EPL has to offer.

## Input and output

| Read a *%type%* into *%reference%* |
| - |
| Reads a value from standard input and saves it at the given address. The type must be either `string` or a primitive. |

| A *%type%* read from input |
| - |
| Reads a value from standard input and returns it. The type must be either `string` or a primitive. |

| Print *%value%* |
| - |
| Writes the string representation of the given value to standard output. |

| Print *%value%* in a line |
| - |
| Like `print` but followed by an end of line. |

| Print an empty line |
| - |
| Writes an end of line to standard output. |

## Generic

| *%value%* as a string |
| -- |
| Returns the string representation of the given value. |

| Swap *%reference%* with *%reference%* |
| - |
| Swaps the values at the two given addresses between each other. |

| Set *%reference%* to *%value%* |
| - |
| Saves a value at the given address. |

## Algebraic operations

## Comparisons

| *%value%* is equal to *%value%* |
| - |
| Returns `true` if the given values are equal (performing a deep comparison) and `false` otherwise. |

| *%value%* is not equal to *%value%* |
| - |
| Returns `true` if the given values are different (performing a deep comparison) and `false` otherwise. |

## Lists

## Chars
