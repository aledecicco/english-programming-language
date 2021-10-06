# Syntax
An EPL program is composed of a series of function definitions. Those functions can either be operators, which return a value, or procedures, which don't return anything. The starting point of every program is a procedure called `Run`, which must always be defined.

Each function has a title and a list of sentences. Titles are used to specify the name, return type, and parameters of a function. Sentences are expressions which give instructions to the evaluator.

This language doesn't use braces, and blocks of code are defined through indentation.


## Defining functions
The title of a function is composed of a first part indicating its return type, and a second part intercalating parameters and identifiers. It must start with an upper case letter and end in a colon.

### Operators

Here is an example of a title:
```
A number equal to the sum of a list of numbers:
```
The first part, `a number equal to` defines this function as an operator which returns a `number` (the equivalent of `float` in other languages). The word `number` can be replaced by any other type for operators that return different things. Also, `a` can be replaced by `another`, which allows defining titles such as `... a number plus another number`.

The second part contains a series of words, `the sum of`, followed by a parameter, `a list of numbers`. This tells the interpreter that the function takes a list of numbers, and that its identifier is `the sum of`. So, if in some expression the parser sees a series of words that contains `the sum of`, it will check if calling that function makes sense in that context, and match those words accordingly. Note that there must be at least one identifier in every function's title.

Since this function was defined as an operator, it can only be used as a value and not as a sentence. An example of its usage would be the following. Let's say we have a list of numbers called `L`, then we can call the function we defined earlier passing `L` as argument, and store the result in another variable called `x`. That would look something like this:
```
Let x be the sum of L.
```

After the title of a function comes a colon, and under it an indented block of sentences, with instructions of what the function should do. For implementing the above function's body, we could do something like this:

```
A number equal to the sum of a list of numbers:
    Let the sum be 0.
    Add each number in the list to the sum.
    Return the sum.
```

First, we initialize `the sum` as 0. Then, we go through each element in the argument and add it to `the sum`. Finally, we return that variable as the result. Note that we are referencing the argument as `the list`. We could have also referenced it as `the list of numbers`, or added a name to it in the title and referenced it with that name, like so:
```
A number equal to the sum of a list of numbers (L):
    ...
```

### Procedures
When defining procedures, the same rules apply. But instead of putting a return type in the first part of the title, we put the word `to`. That way, we can define functions such as this one:
```
To print the even elements in a list of whole numbers:
```
Now, we can use that procedure as a sentence in the body of any other function, allowing us to print the contents of any list of whole numbers (ints).

### Questions

Another interesting case is when defining functions that asks questions (which means they return a boolean). A shorthand is available in those cases to avoid writing `a boolean equal to ...`. The shorthand is the word `whether`. Then, we can define functions like the following:
```
Whether a number is greater than another number:
    ...
```
Now, we can use it in other structures such as conditionals, which we will see up ahead. Let's say we have two lists of numbers `the first list` and `the second list`. We can combine the question we just defined with the operator we defined at the begginning, and do something like this:
```
If the sum of the first list is greather than the sum of the second list, ...
```
Using the defined functions and variables, and their types, the parser finds a valid way to match those words as specific function calls. In this case, the only way to match those words with the functions we have and without causing a type error is `If (the sum of (the first list)) is greather than (the sum of (the second list))`, so we don't need to write all those parenthesis.

If some expression is ambiguous, the parser will choose one of the ways in which it can be understood. To force that expression to be interpreted as some other option, parenthesis can be used to group words together.


## Built-in sentences

We have already seen how to define functions that can be called as sentences (procedures). But the language comes with some sentences to declare variables and control the flow of our programs.

### Let
This is perhaps the most complex of them all because of the different variations it can have. Let's see some examples:
```
Let s be "hello world".
Let a, b, and c be 2 plus 2.
Let x be a list of numbers.
Let y be a list of whole numbers containing 1, 2, and 3.
Let z be a character equal to the first element of y as a string.
```

In the first case, the string `"hello world"` is assigned to `s`, which is inferred to be a string because of the value it is being assigned.

In the second case, the value `2 plus 2` is assigned to `a`, `b`, and `c`. The operator `plus` will be called one time, and then the result will be copied and assigned to each variable. Note that there is a comma after each variable name, including the one before the last one. This is called [serial comma](https://en.wikipedia.org/wiki/Serial_comma) and it helps a lot with parsing.

The third case declares `x` as an empty list of numbers.

In the fourth case, we declare `y` as a list of whole  numbers and specify by extension the elements it contains. `Let` sentences are the only place where lists can be declared in this way. Note that the serial comma is also necessary here for the values.

The fifth case explicitly states the type of the variable `z`. It is useful in some border cases where the value is ambiguous, such as this one. The value can be understood as `the first element of (y as a string)`, which is the character `'1'`, or `(the first element of y) as a string`, which is the string `"1"`. However, since we are specifying that we want a character, only the first interpretation is valid.

### If-else
This sentence, among with other flow control sentences, contains other sentences and has two forms: a simple one and a block one. Here are some examples:
```
If a is greather than or equal to b, print a, otherwise print b.

If a is greater than b:
    Print a.
Otherwise:
    If a is equal to b:
        Print a.
    Otherwise:
        Print b.
```
Here, both `if-else` clauses do the same thing, printing `a` if it is greather than or equal to `b`, and `b` otherwise. However, the simple form has some restrictions. It must be written in a single line, and it can only contain one sentence in its simple form. The `let` expression is excluded from this, and can only be used at the top level of a function definition. Also note that the placement of commas is very important.

The block form doesn't have those restrictions, and can contain as many sentences as necessary in any form. In both froms, the `if` clause must be followed by an `else` clause, and in block form the conditions must be followed by a colon.

### When and unless
The sentence `when` is the same as an `if-else` but without the `else` block. The sentence `unless` is the same as a `when` but with its condition negated. Here are two equivalent examples:
```
When a is greater than or equal to b, print a.

Unless a is smaller than b:
    Print a.
```

### While and until
We can repeat some actions while, or until, a condition is true, like so:
```
While a is smaller than 5:
    Print a.
    Add 1 to a.

Until a is equal to or greater than 5:
    Print a.
    Add 1 to a.
```

### For-each
With this loop, we can define an iterator that goes through each element in a list, evaluating some sentences at each step. Through the iterator, we can modify the values in the original list. We must specify its name and type, and the list value to be iterated:
```
For each number (n) in L:
    Multiply n by 2.
    Add 1 to n.

For each number (n) in L, print n.
```

### Break
This sentence allows us to stop any loop earlier. A simple example to demonstrate how it works:
```
While true:
    Stop the loop.
```


### Return
Tells the function to return a specific value, giving us the ability to exit early from an operator. Keep in mind that this sentence is not allowed in procedures, and reaching the end of an operator without finding one of these will also cause an error.
```
Whether a whole number is even:
    Let c be the quotient of the whole number and 2.
    If c is equal to 0, return true, otherwise return false.
```

### Exit
Tells the function to return without a result, giving us the ability to exit early from a procedure. Of course, this sentence is not allowed in operators.
```
To print the first element in a list of numbers:
    When the list is empty, exit.
    Print the element of the list at 0.
```

### Try, catch, and attempt
We can recover from some errors using `try-catch` clauses. The `attempt` sentence works the same way, but doesn't have a `catch` clause. Here are some examples:
```
Try to:
    Multiply n by 2.
    Divide n by 0.
In case of error:
    Print "Can't divide by 0!".

Try to divide n by 0, and in case of error print "Can't divide by 0!".

Attempt to divide n by 0.
```

In the three cases we are trying to divide a variable `n` by 0. If an error like this one is generated, it will bubble up until it reaches a sentence catching it. If it is not caught, it will be printed to the console and the program will exit.

If it is caught, all changes made to the variables during the `try` (or `attempt`) block will be rolled back, and execution will continue in the `catch` block (or in the next sentence in the case of `attempt`).

So, in the first example, even though `n` was multiplied by 2 at some point, those changes will be undone after trying to divide it by 0.


### Throw
In case we reach some undesired condition in our program, we can signal it by manually triggering an exception. The `throw` sentence needs a reason for the error, which will be displayed in the console if the error is not caught:
```
When b is equal to 0, throw an error because division by zero is undefined.
Divide a by b.
```
Here, if `b` happens to be 0, we will throw a custom error instead of the one that `divide by` would eventually generate.


## Values, types, and variables

The following primitive types are available: `boolean`, `whole number` (int), `number` (float), and `character`.

Variables can also be lists of other types, including lists. When specifying the type of a list, the type of its elements must be in plural, like `list of whole numbers` or `list of lists of booleans`.

There is also the alias `string` for `list of characters`.

Constant boolean values are used with the words `true` and `false`.

Numbers and whole numbers are used witing the numeric values, not the words. Numbers must have a decimal part.

Char literals are written between single quotes, and string literals between double quotes. String literals are the only other way, appart from the `let` sentence, to define lists by extension and, of course, they only work for lists of chars.

Variable names can be any series of words except for `be` and `in`, which are the only two reserved keywords. They are referenced using their names, which can optionally be preceded by the word `the`. This applies to all variables (declared in `let` expressions, defined as function parameters, or defined as iterators in a `for-each` loop).

Aliases are generated for function parameters without explicit names, according to their types. If two or more parameters have the same type, the generated aliases have ordinals. For example, if a function takes two numbers, their aliases will be `the 1st number` and `the 2nd number`, and they can be referenced that way in the function's body.

In the case of lists, some extra aliases are generated: if a function takes a string and a list of strings, one of the aliases for the second parameter will be `the 2nd list`. This is because, even though it is of a different type, the first parameter is already a list. The second parameter will also have the aliases `the list of strings`, `the list of lists of characters`, and `the list of lists`.


## References
Function parameters can be defined to take references to values by adding `reference to` before their type:
```
To double a reference to a number:
    Multiply the whole number by two.
```
Here, we are saying that the function takes a number by reference. The number pointed at by the reference will be multiplied by two, modifying the original number that was passed as argument. References can be variables or list elements.

It is important to note that when a list is passed by value, a deep copy of it is created. To compensate for this high usage of memory, a garbage collector is implemented.

Some operators in the prelude also return references, but it's important to know that they are only alive during the call in which they are obtained. After that, a copy of the reference is generated, so storing it will result in a copy of the original value.

For example, we can increase the number in a specific position of a list by saying `Add 1 to the element of L at 5`. However, saying `Let x be the element of L at 5` and then `Add 1 to x` will only increase the value of the variable `x`, but not of the 5th element of the list `L`. In the first case, the reference returned by `the element of ... at ...` is still alive when `add ... to ...` is called on it, so the original element is modified. In the second case, saving the result of `the element of ... at ...` in a variable causes a copy of that element to be stored, so the original element is not modified but instead the variable is.


## Iterators
One special feature of this language are iterators, used with the keyword `each`. They allow going through every element in a list in a natural way. They are useful for mapping over lists, generating new versions of them, or running sentences for each of their elements:
```
Divide each number in L by 2.
Print each number in L.
```
Here, we specify the type of the iterator, and the functions `divide by` and `print` are called for each element of the list of numbers `L`. Note that references also work with iterators, passing each element of the list by reference.

We can also combine two lists using two iterators in the same sentence. A similar `for-each` loop is provided for clarity:
```
Print each number in M plus each number in N.

For each number (m) in M:
    For each number (n) in N:
        Print m plus n.
```
In both cases, we are looping through each element in `N` for each element in `M`, adding them together, and printing the result.

Another way to use them is to define a list mapping an operator over another list. For that, we use a `let` sentence in its `containing` form:
```
Let N be a list of numbers containing each number in M plus 2.
```
Here, `N` is the result of adding two to each element in `M`, and `M` is unmodified.

We can also combine two lists together as before:
```
Let L be a list of numbers containing each element in M plus each element in N.
```
In this case, instead of printing what we did in the second example, we store it in a list `L`.

Finally, we can use it to concatenate lists and elements:
```
Let L be a list containing each element in M, 1.0 plus 2.0, and each element in N plus 1.
```
Now, `L` is a list of whole numbers containing each element in `M`, followed by the element `3.0`, followed by the result of adding 1 to each element in `N`. So, if `M` was `[1.0, 2.0]` and `N` was `[3.0, 4.0]`, `L` would be `[1.0, 2.0, 3.0, 4.0, 5.0]`.


