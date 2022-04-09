# Built-in functions

This is an extensive list of all the built-in functions EPL has to offer.

## Input and output

Input can be handled with `Read a %type% into %reference%`, which reads a value from standard input and stores it at the given address. Values can also be fetched directly without storing with `a %type% read from input`. Note that in both cases the type has to be specified, and it must be either `string` or a primitive.

For output, there are three variations of `print`:

| Operation | Procedure |
| --- | --- |
| Printing a value to standard output | `Print %value%` |
| Printing a value followed by an end of line | `Print %value% in a line` |
| Printing an empty line | `Print an empty line` |

The string representation of any value can be obtained with `%value% as a string`.


## References

A reference to an aribtrary element of a list can be obtained by using `the element of %list% at %int%`. As with all other references, once it's stored it becomes a value, so the original value in the list can no longer be modified. However, it can be modified during the call in which it's obtained. For example:
```
Add 5 to the element of L at 3.
```

The function `Swap %reference% with %reference%` allows swapping the values at the two given addresses between each other. Both references must point to values of the same type.

A single reference can be modified as well using `Set %reference% to %value%`. The given value and the one pointed at by the reference must have the same type.


## Algebraic operations

Most operations on numbers have two variations: one in the form of an operator that returns the resulting value, and one in the form of a procedure that applies the operation to an existing reference:

| Operation | Operator | Procedure |
| --- | --- | --- |
| Addition | `%number% plus %number%` | `Add %number% to %reference%` |
| Subtraction | `%number% minus %number%` | `Subtract %number% from %reference%` |
| Multiplication | `%number% times %number%` <br> `%number% multiplied by %number%` | `Multiply %reference% by %number%` |
| Division | `%number% divided by %number%` | `Divide %reference% by %number%` |
| Whole division | `the quotient of %whole number% and %whole number%` | `Quotient %reference% by %whole number%` |
| Exponentiation | `%number% raised to the power of %number%` | `Raise %reference% to the power of %number%` |
| Rounding | `%number% rounded` | `Round %reference%` |
| Ceiling | `the ceiling of %number%` | `Round %reference% up` |
| Floor | `the floor of %number%` | `Round %reference% down` |
| Truncating | `%number% truncated` | `Truncate %reference%` |

A few more complex ones are only available as operators, since they don't really work as a verb:

| Operation | Operator |
| --- | --- |
| Logarithm | `the logarithm of %number% to base %number%` |
| Module | `%whole number% module %whole number%` <br> `the remainder of dividing %whole number% by %whole number%` |
| Root | `the square root of %number%` |
| Absolute value | `the absolute value of %number%` |


## Comparisons

Equality can be tested for using `%value% is equal to %value%` and `%value% is not equal to %value%`, where the two arguments are any two values of the same type. They both perform a deep comparison.

Numbers can also be compared using operators >, >=, <, and <=, which are called `greater than`, `greater than or equal to`, `less than`, and `less than or equal to` respectively.


## Lists

There are three queries about the size of a list:

| Operation | Operator |
| --- | --- |
| Check if empty | `%list% is empty` |
| Check if not empty | `%list% is not empty` |
| Get length | `the length of %list%` |

There are queries about the presence of a value as well:

| Operation | Operator |
| --- | --- |
| Check if element is in list | `%list% contains %value%` |
| Get index of element | `the index of %value% in %list%` |

Lists can also be modified. Operators return deep copies, and procedures modify the original lists passed by reference.

Adding items:

| Operation | Operator | Procedure |
| --- | --- | --- |
| Push item at beggining | `%list% with %value% added at the beggining` | `Add %value% at the beggining of %reference%` |
| Push item at end | `%list% with %value% added at the end` | `Add %value% at the end of %reference%` |
| Insert item at position | `%list% with %value% added at %whole number%` | `Add %value% to %reference% at %whole number%` |

Removing items:

| Operation | Operator | Procedure |
| --- | --- | --- |
| Remove nth element | `%list% without the element at %whole number%` | `Remove the element at %whole number% from %reference%` |
| Remove first apparition of element | `%list% without the first apparition of %value%` | `Remove the first apparition of %value% from %reference%` |
| Remove all apparitions of element | `%list without all apparitions of %value%` | `Remove all apparitions of %value% from %reference%` |
| Tail | `%list% without its first element` | `Remove the first element from %reference%` |
| Init | `%list% without its last element` | `Remove the last element from %reference%` |
| Take from beggining | `the first %whole number% elements in %list%` | `Leave the first %whole number% elements in %reference` |
| Take from end | `the last %whole number% elements in %list%` | `Leave the last %whole number% elements in %reference` |
| Drop from beggining | `%list% without its first %whole number% elements` | `Remove the first %whole number% elements from %reference%` |
| Drop from end | `%list% without its last %whole number% elements` | `Remove the last %whole number% elements from %reference%` |

Lists can also be modified according to other lists:

| Operation | Operator | Procedure |
| --- | --- | --- |
| Append | `%list% appended to %list%` | `Append %list% to %reference%` |
| Prepend | `%list% prepended to %list%` | `Prepend %list% to %reference%` |
| Remove items in common | `%list% without the elements in %list%` | `Remove the elements in %list% from %reference%` |

A list can be emptied out by using `Empty out %reference%`.

A list with the numbers in an interval can be generated by using `the list from %whole number% to %whole number%`.


## Chars

| Operation | Operator | Procedure |
| --- | --- | --- |
| To uppercase | `%char% in uppercase` | `Transform %reference% to uppercase` |
| To lowercase | `%char% in lowercase` | `Transform %reference% to lowercase` |


## Booleans

All basic boolean operations are available:

| Operation | Operator |
| --- | --- |
| AND | `%bool% and %bool%` |
| OR | `%bool% or %bool%` |
| XOR | `%bool% or %bool% but not both` |
| NOT | `not %bool%` <br> `%bool% negated` |

A reference to a boolean can be negated using `negate %reference%`.

There are a few other operations: `%bool% is true`, `%bool% is false`, and `whether %boolean%`. They don't make too much sense on their own, but allow writing more natural sentences such as:

```
Let X be whether N is larger than N.
If X is true:
    ...
```
