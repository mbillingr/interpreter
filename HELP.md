Quick Help
==========

Learning Scheme
---------------

- [The Scheme Programming Language](https://www.scheme.com/tspl4/)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html) 

Please keep in mind that this interpreter was written by the author
in order to learn Scheme. Its standard library is far from complete,
but it should be sufficient to follow through the SICP book.

The REPL
--------

When hitting [RETURN] the interpreter evaluates all expressions typed so 
far in sequence and print the value of the last expression.

It is possible to write multi-line expressions by hitting [RETURN] on an
incomplete expression. The prompt changes to "..." and evaluation happens
when the expression is completed.

Hit [TAB] to automatically attempt completing the symbol at the cursor 
with definitions from the global environment. Hit [TAB] repeatedly to 
cycle through all possible completions. Note that the auto-completer is 
not smart enough to recognize local bindings. Only globally defined 
names can be completed.

Useful procedures
-----------------

| Signature         | Description                                        |
|-------------------|----------------------------------------------------| 
| (print-env)       | List all functions and variables.                  |
| (debug expr)      | Like (eval expr) but manually step through         |
