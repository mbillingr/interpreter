
# A Lisp/Scheme Interpreter

I wrote this interpreter while studying SICP ([Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html)), and later [Lisp In Small Pieces](https://www.cambridge.org/core/books/lisp-in-small-pieces/66FD2BE3EDDDC68CA87D652C82CF849E).

As I wrote the interpreter without prior knowledge about Lisp or Scheme it is not compliant to any accepted standard. However, it incorporates some concepts from R7RS that are not discussed in these older books. 
New features were added whenever example code from the books failed to run, so the source code has rather prototypical character and is highly unreadable.

To the interpreter type `cargo run --release`. This will drop you into the REPL.
Any command line arguments are treated as file names to run in sequence before entering the REPL.

For example, to run an exercise:
```
cargo run --release ../chapter1/exercise1-27.scm
```


