# Expressive
A programming language that resembles English. Write some sentences and watch the interpreter try to understand what you meant.

# Syntax
An Expressive program is composed of a series of function definitions. Those functions can either be operators, which return a value, or procedures, which don't return anything.

Each function has a title and a list of sentences. Titles are used to specify the name, return type, and arguments of a function. Sentences are expressions which tell the interpreter to define certain variables or call certain functions.

This language doesn't use braces, so in order to specify blocks of code, indentation is used. That's why titles can't be indented, and sentences in the same list must be written one under the other, with the same indentation level, and with at least one level more than the title.

Parenthesis are allowed in order to group sequences of words together, solving cases in which there is ambiguity.

Also, titles must end in a semicolon, sentences must end in a stop, and both must begin with an upper case letter.

## Titles
A title is composed of:
* Some words specifying a return type
* Words that act as identifiers for the function
* Words that define the name and type of a function parameter

Syntax:
```
%Return type% [%identifier% | %parameter%]:
```
There can be many of the last two as long as they are intercalated.

### Return type

For procedures:
```
To ...
```
This allows writing function titles such as `To print something` and then call them saying `Print something`.

For operators:
```
A %type name% equal to ...
```
This way, operators can have titles like `A number equal to some result` and be used in expressions such as `Let x be some result`.

For boolean operators:
```
Whether ...
```
This shorthand allows defining questions such as `Whether something is true` that can be used like this: `If something is true, do some other thing`.

Note that in all cases the words used in the return type part of the title, as well as the semicolons, are ignored when calling the function.

### Type names
The possible names of primitive types are `boolean`, `whole number`, `number`, `character`, and `string`. Also, `list of ...` is used for lists, which requires the type inside the list to be specified in plural: `list of booleans` or `list of lists of numbers`. Note that `string` is an alias for `list of chars`.

### Parameters
```
a %singular type name% (%variable name%)
```
The type name specifies the expected type of the parameter and the variable name specifies the name the parameter will have in the body of the function. For example, if a function takes a number as one of its parameters, it could be specified as `a number (m)`.

### Identifiers
Identifiers can be made of any sequence of words except for `a`. They are used to understand which function is being called. For example, with the title `A number equal the product of a number (m) and a number (n)`, the identifying parts are `the product of` and `and`. When calling this function, the gaps should be filled in with actual values.

### Examples
```
A number equal to the sum of a list of numbers (L):
```
This specifies that the function returns a number and takes a list of numbers, which can be referenced as `"L"` in the function's body.

```
Whether a number (x) is between a number (m) and a number (n):"`
```
This specifies that the function returns a boolean and takes two numbers called `m` and `n`.

```
To print a number (m):
```
This specifies that the function doesn't return anything, making it a procedure, and takes a number called `m`.

## Sentences

### Let
```
Let %variable names list% be %value%
```
This allows declaring the value of one or more variables.
The syntax for the list is the following:
```
([%name%,] and %name%) | %name%
```

Note that if the list has more than one element, there has to be a comma after each element except the last one (including the one right before `and`). This is called [serial comma](https://en.wikipedia.org/wiki/Serial_comma).

### Result
```
The result is %value%
```
Tells the function to return a specific value.

### Procedure call
Any procedure can be used as a sentence, filling in with values the gaps where function parameters are.

### Control flow
These sentences all have something in common: they contain other sentences. All of them have a simple form and a block form, both sharing the same header.

Sentences in their simple form can only have one sentence inside them, and it can only be a variable declaration, a result statement or a procedure call. The header must end in a comma, after which comes the contained sentence.

Sentences in their block form contain a list of sentences without any restrictions. The header must end in a semicolon and indentation rules used for function definitions also apply here.

Simple if:
```
If %boolean value%, %basic sentence%.
```

Block if:
```
If %boolean value%:
    %sentence 1%
    %sentence 2%
    ...
```

Simple if-else:
```
If %boolean value%, %basic sentence%, otherwise %basic sentence%.
```

Block if-else:
```
If %boolean value%:
    %sentence 1%
    %sentence 2%
    ...
Otherwise:
    %sentence 3%
    %sentence 4%
    ...
```
The `else` statement must come after an `if` statement. They must both have the same form, and if they are blocks they have to be defined at the same indentation level

Simple for each:
```
For each %variable name% in %list value%, %basic sentence%.
```

Block for each:
```
For each %variable name% in %list value%:
    %sentence 1%
    %sentence 2%
    ...
```

Simple until:
```
Until %boolean value%, %basic sentence%.
```

Block until:
```
Until %boolean value%:
    %sentence 1%
    %sentence 2%
    ...
```

Simple while:
```
While %boolean value%, %basic sentence%.
```

Block while:
```
While %boolean value%:
    %sentence 1%
    %sentence 2%
    ...
```

### Examples
Assign the value `2` to variables `x`, `y`, and `z`:
```
Let x, y, and z be 2
```

Make the current function return `6`:
```
The result is 2 times 3
```

Call the procedure `"print"` with `6` as its argument:
```
Print 2 times 3
```

Call the procedure `print` for each element of the list `L`:
```
For each element in L, print the element
```

Add one to variable `x` until it's larger than `3`:
```
Until x is larger than 3, let x be x plus 1
```

Print a `1` if `x` is larger than `0`, and a `0` otherwise:
```
If x is larger than 0:
    Print 1.
Otherwise:
    Print 0.
```

## Values

Values can be primitives, lists, variables, and operator calls. Available primitives are booleans, numbers, and whole numbers.

### Booleans
The two possible values are refered to with the words `true` and `false`.

### Whole numbers
Whole numbers are used writing the numeric values, not the words.

### Number
Numbers are written with a dot separating the decimal part. If a function takes a number and a whole number is given, it is converted to a number implicitly.

### Chars
Char literals are written between single quotes.

### Strings
String literals are written between quotes.

### Lists
An empty list:
```
a list of %plural type name%
```

A list with elements:
```
a list of %plural type name% containing %list of values%
```

A string literal:
```
"[%character%]"
```
The only way to declare a list by extension is inside a `Let` expression, except for string literals which can be used anywhere. Note that all values have to be of the same type as the one declared.

### Variables
Variable names can be any series of words except for `be` and `in`, which are the only two reserved keywords. They are referenced using their names, which can optionally be preceded by the word `the`. This applies to all variables (declared in `let` expressions, passed as parameters to a function, or defined as iterators in a `for each` loop).

### Examples

Declare variable `L` as the list `[1-1, 2.2, 3.3]`:
```
Let L be a list of floats containing 1.1, 2.2, and 3.3
```

Print each element of `L`:
```
For each element in L, print the element
```
Note that here, the iterator is referenced as `the element`.


# How it works
EPL takes a text file as input, tries to understand what it means, and produces its output. In order to do so, it runs the input through many stages.

First, there is a fuzzy parser which parses the general structure of the program, but it can't understand function calls.

After that, there comes a solver which looks at the structure created by the fuzzy parser and finds all the function calls that it left uninterpreted. Then, it runs them through a matcher that transforms them into specific function calls. After that, the solver performs a series of validations such as type checking.

Finally, the evaluator takes the structure created by the solver and runs the actual program.

It's important to note that, in order to work, a program must define the procedure called `Run`:

```
To run:
    Print "Hello world!\n"
```

# Possible improvements:
- Clean up the methods that checks type bindings in the solver
- Change the state so that ids are generated for each variable, and each variable name points to an id
- Infer variable aliases from their types in titles
- Allow passing by reference
- Implement importing other files as libraries (without the `Run` procedure)
- Prevent the `Run` procedure from being called explicitly
