# Code Organization
The code is into a few main sections which are commented, all of the code is in app/Main.hs

## Scanning
The functions in the scanning section convert an input string into an `[IToken]`, a list of
`(Token, Int)` pairs, where the integer represents the column offset of the token. The tokens are
numbers, variables, or operations.

## Detection
The notation detection section has a few functions to determine whether an input expression is in
prefix, postfix, or infix form. This can fail (for some expressions, we cannot figure out even a
guess of what notation it is), which is why these functions return a `Failure NotationType`.

## Conversions
The notation conversions convert an expression from one form to another. Since both infix and prefix
notation end up parsed as an `AST`, we just need to convert `AST` expressions to those two as well
as postfix expressions, and convert postfix expressions, which are of type `[IToken]`, to these two
forms. Thus, in `infix` mode, for example, any postfix or prefix expression will be converted into
infix notation.

## Parsing
The parsing section parses the scanned token lists into an AST, depending on if they are in prefix
or infix form (postfix notation directly usees the scanned token list, thus not needing any parsing
at all). For infix notation, parsing enforces the order of operations, so that we get 1 + 2 * 3 = 7
and not 9, but (1 + 2) * 3 = 9 works correctly. No order of operations is needed for prefix
notation, it is inherently specified by the notation.

## Computation
Since infix and prefix notations both get parsed into an AST, we just need to be able to evalue the
AST or a `[IToken]` for postfix notation. These evaluation functions return a `Failure Int` since
evaluation can fail (if a variable is not defined or if we have a division by 0).

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
