# Code Organization
The project is a calculator with some spice added: Variable declaration and usage, nice,
column-numbered error handling, and conversion and support for prefix, infix, and postfix notation.
The code is into a few main sections which are commented, all of the code is in app/Main.hs

## Datatypes
`Op` contains the operations of the calculator. `AST` is the datatype that both prefix and infix
notation are parsed into. `Token` and `[IToken]` are the syntax objects. Tokens are the operators,
numbers, variables and parentheses. An `IToken` is a token paired with an integer, representing the
column at which the token occurs in the string. Using `IToken`, we are able to write more detailed
error messages.

## Scanning
The functions in the scanning section convert an input string into an `[IToken]`. Numbers and
variables are scanned by accumulation: so when an alphaabetic character or numeric character is
seen, we continue scanning until the next non-digit or non-alphabetic character to get the number.
The column offset for variables and numbers is the starting column of the number/variable.

## Detection
The notation detection section has a few functions to determine whether an input expression is in
prefix, postfix, or infix form. If we see a parentheses, we know we must be in either infix or
prefix notation, so we look further to see if there is a number or operation first. If we see a
number or variable at the front, we know we must be in either postfix or infix form, so we again
look further to if we see an operation or a number next. This can fail (for some expressions, we
cannot figure out even a guess of what notation it is), which is why these functions return a
`Failure NotationType`.

## Conversions
The notation conversions convert an expression from one form to another. Since both infix and prefix
notation end up parsed as an `AST`, we just need to convert `AST` expressions to those two as well
as postfix expressions, and convert postfix expressions, which are of type `[IToken]`, to these two
forms. Thus, in `infix` mode, for example, any postfix or prefix expression will be converted into
infix notation (actually, so will `infix` expressions, which mostly just messes with whitespace
since the programatic spacing is not perfect).

## Parsing
The parsing section parses the scanned token lists into an AST, depending on if they are in prefix
or infix form (postfix notation directly usees the scanned token list, thus not needing any parsing
at all). For infix notation, parsing enforces the order of operations, so that we get 1 + 2 * 3 = 7
and not 9, but (1 + 2) * 3 = 9 works correctly. No order of operations is needed for prefix
notation, it is inherently specified by the notation.

## Computation
Since infix and prefix notations both get parsed into an AST, we just need to be able to evalue the
AST or a `[IToken]` for postfix notation. We evaluate postfix notation by updating an integer list
as our stack, and prefix and infix notation are evaluated by recursively going down the tree. A
variable in an expression is lazily evaluated, so whatever expression it was defined as is then
computed and that value is used. These evaluation functions return a `Failure Int` since evaluation
can fail (if a variable is not defined or if we have a division by 0).

## Interaction
The interaction section contains `main` as well as all of the other functions involved in taking
either stdin or the REPL input and extracting the string representing either an arithmetic
expression or variable assignment. These also handle displaying computation or error results back to
the user.


# How to use the project
The `help` message displayed if no flags are passed or if "help" is input at the REPL explains the
gist of how to use the project. It can be run in stdin mode with a flag to specify what to do for
non-assignment lines, either converting to a different form or computing them. It can also be run in
REPL mode, where the current mode of what to do for non-assignment lines can be change at the REPL.
Variables are assigned with the form `!var {arithmetic expression}`, and variables can have any
notation form for the expression. `test.txt` is a helpful example of how the syntax works, and
`test_out.txt` is the expected result from running `test.txt` with the `--compute` flag (modulo the
haskell compiler output if using `cabal run`).
