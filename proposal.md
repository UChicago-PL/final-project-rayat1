# Project proposal
Starting from the RPN calculator we discussed in lecture, one project that I think would be
interesting is a program to convert and evaluate between 3 different types of notation for
math expressions: prefix, postfix, and infix.

The final code would be an executable and be called like ./myproj --from infix --to postfix,
and then would take a stdin like (+ (1 (* 3 4))) and output 3 4 * 1 + to stdout. I think
there are a few different things that can be done with this project, especially with infix
notation.

## Easy goals
  - Ability to parse each of the three kinds of notation and output in the desired form
  - Guessing the notation (so --from flag is not needed)
    - If we see a number first, we know it is either postfix or infix, so then we see if
      there is another number or an operation to determine the notation type
  - Variables: Specify a list of assignments like `x = 1 + 5`, separated by newlines, and
    then convert/parse the last line of stdin as a general expression
    - Variables can be lazily managed, so something like `x = 1 / 0\n 1 + 1` does not throw
      an error
  - Good tests for the features

## Medium goals
  - More complex operators: &&, ||, ? with lazy evaluation
  - Detailed error messages, stuff like what Rust compiler does so the exact column/line
    number of the error and what was expected vs gotten

## Challenge goals
  - Generate assembly code which outputs the result, so like output a file like `calc.s`
    which has an entry point and the assembly instructions compute the solution into `%rax`
  - Custom operator precedence for infix notation (instead of doing PEMDAS, allow
    user-specified order of operations via like `* precedence 1, + precedence 2` means parse
    `+` before `*`)
  - Allow users to define custom operations in general with variable syntax, so like 
    `*3 = 3 *`
