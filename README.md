# Expressive
A programming language that resembles English. Write some sentences and watch the interpreter try to understand what you meant.

# Syntax
An Expressive program is composed of a series of function definitions. Those functions can either be operators, which return a value, or procedures, which don't return anything.

Each function has a title and a list of sentences. Titles are used to specify the name, return type, and arguments of a function. Sentences are expressions which tell the interpreter to define certain variables or call certain functions.

This language doesn't use braces, so in order to specify blocks of code, indentation is used. That's why titles can't be indented, and sentences in the same list must be written one under the other, with the same indentation level, and with at least one level more than the title.

Also, titles must end in a semicolon, sentences must end in a stop, and both begin with an upper case letter.

## Titles
A title is composed of:
* Some words specifying a return type
* Words that act as identifiers for the function
* Words that define the name and type of a function parameter

There can be many of the last two as long as they are intercalated.

### Return type
If the function is a procedure, this first segment should be just the word `"To"`. This allows writing function titles such as `"To print something:"` and then call them saying `"Print something"`.

If the function is an operator, this part should start with the word `"A"` followed by the name of a type (such as number or list of numbers) and then the words `"equal"` and `"to"`. These operators can have titles like `"A number equal to some result:"` and be called with `"Let x be some result"`.

There's also a shorthand for boolean functions that allows specyfing the return type using just the word `"Whether"`. This way, a function can be defined as `"Whether something is true:"` and used like this: `"If something is true, do some other thing"`.

Note that in all cases the words used in this part of the title, as well as the semicolons, are ignored when calling the function.

### Type names
The possible names of prmitive types are `"boolean"`, `"number"` and `"float"`. Also, `"list of ..."` is used for lists, which requires the type inside the list to be specified in plural: `"list of booleans"` or `"list of lists of numbers"`

### Parameters
Parameters must start with the indefinite article `"a"`, followed by the name of a type (which specifies the expected type of the parameter) and ending with a variable name between parenthesis (which specifies the name the parameter will have in the body of the function). For example, if a function takes a number as one of its parameters, it could be specified as `"a number (m)"`.

### Identifiers
Identifiers can be made of any sequence of words except for `"a"`. They are used to understand which function is being called. For example, with the title `"A number equal the product of a number (m) and a number (n):"`, the identifying parts are `"the product of ... and ..."`.

### Examples
`"A number equal to the sum of a list of numbers (L):"`

This specifies that the function returns a number and takes a list of numbers, which can be referenced as `"L"` in the function's body.

`"Whether a number (x) is between a number (m) and a number (n):"`

This specifies that the function returns a boolean and takes two numbers called `"m"` and `"n"`.

`"To print a number (m):"`

This specifies that the function doesn't return anything, making it a procedure, and takes a number called `"m"`.

## Sentences

### Let
This allows declaring the value of one or more variables. It must start with the word `"Let"` followed by an enumeration of variable names, then the word `"be"` and finally the value that the variables should be assigned.

Note that if the enumeration has more than one element, there has to be a comma after each element except the last one (including the one right before `"and`). This is called [serial comma](https://en.wikipedia.org/wiki/Serial_comma).

### Result
Tells the function to return a specific value. It starts with `"The result is"` followed by the value to be returned.

### Procedure call
Any procedure can be used as a sentence, filling in with values the gaps where function parameters are.

### Control flow
These sentences all have something in common: they contain other sentences. All of them have a simple form and a block form, both of them sharing the same header.

Sentences in their simple form can only have one sentence inside them, and it can only be a variable declaration, a result statement or a procedure call. The header must end in a comma, after which comes the contained sentence.

Sentences in their block form contain a list of sentences without any restrictions. The header must end in a semicolon and indentation rules used for function definitions also apply here.

* The header for `If` starts with the word `"If"` and ends with a boolean value
* An `Else` statement must come after an `if` statement, and its header is just the word `"otherwise"`. They must both have the same form, and if they are blocks they have to be defined at the same indentation level
* The header for `ForEach` starts with the words `"For each"` followed by a variable name (to be used as the iterator), the word `"in"` and a list value
* The header for `Until` starts with the word `"Until"` and ends with a boolean value
* The header for `While` starts with the word `"While"` and ends with a boolean value

### Examples
`"Let x, y, and z be 2"`

This assigns the value `2` to variables `x`, `y`, and `z`.

`"The result is 2 times 3"`

This makes the current function return `6`.

`"Print 2 times 3"`

This calls the procedure called `"print"` with `6` as its argument.

`"For each element in L, print the element"`

This calls the procedure called `"print"` for each element of the list `L`.

`"Until x is larger than 3, let x be x plus 1"`

This adds one to the variable `x` until it's larger than `3`.

```
If x is larger than 0:
    Print 1.
Otherwise:
    Print 0.
```

This prints a `1` if `x` is larger than 0, and a `0` otherwise.

## Values

Values can be primitives, variables and operator calls. Available primitives are booleans, integers and floats.


## Variables

Variable names can be any series of words except for `be` and `in`, which are the only two reserved keywords.
