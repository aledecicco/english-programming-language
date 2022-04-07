# Built-in functions

This is an extensive list of all the built-in functions EPL has to offer.

## Input and output

Input can be handled with `read a %type% into %reference%`, which reads a value from standard input and stores it at the given address. Values can also be fetched directly without storing with `a %type% read from input`. Note that in both cases the type has to be specified, and it must be either `string` or a primitive.

For output, there are three variations of `print`:

| Print %value% |
| - |
| Writes the string representation of the given value to standard output. |

| Print %value% in a line |
| - |
| Prints a value followed by an end of line. |

| Print an empty line |
| - |
| Writes an end of line to standard output. |

The string representation of any value can be obtained with `%value% as a string`.


## References

A reference to an aribtrary element of a list can be obtained by using `the element of %list% at %int%`. As with all other references, once it's stored it becomes a value, so the original value in the list can no longer be modified. However, it can be modified during the call in which it's obtained. For example:
```
Add 5 to the element of L at 3.
```

The function `Swap %reference% with %reference%` allows swapping the values at the two given addresses between each other. Both references must point to values of the same type.

A single reference can be modified as well using `Set %reference% to %value%`. The given value and the one pointed at by the reference must have the same type.


## Algebraic operations

## Comparisons

You can check for equality using `%value% is equal to %value%` and `%value% is not equal to %value%`, where the two arguments are any two values of the same type. They both perform a deep comparison of the given values.

Numbers can also be compared using other operators (>, >=, <, and <=), which are called `greater than`, `greater than or equal to`, `less than`, and `less than or equal to`.

## Lists

## Chars
