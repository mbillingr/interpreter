
# A Lisp/Scheme Interpreter

I wrote this interpreter while studying SICP ([Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html)), and later [Lisp In Small Pieces](https://www.cambridge.org/core/books/lisp-in-small-pieces/66FD2BE3EDDDC68CA87D652C82CF849E).

As I wrote the interpreter without prior knowledge about Lisp or Scheme it is not compliant to any accepted standard. However, it incorporates some concepts from R7RS that are not discussed in these older books. 
New features were added whenever example code from the books failed to run.
In consequence, the source code organically grew rather than following a clean design.
It has rather prototypical character and is highly unreadable.

To run the interpreter type `cargo run --release`. This will drop you into the REPL.
In order to pass command line arguments to the interpreter, seperate them with `--` from cargo's arguments.
For example `cargo run --release -- --help` will display the command line help of the interpreter.

# Examples

## Run a Scheme script before entering the REPL
From cargo:
```
cargo run --release -- ~/SICP/chapter1/exercise1-27.scm
```
Running the binary directly:
```
interpreter ~/SICP/chapter1/exercise1-27.scm
```

## Run a Scheme script without entering the REPL
From cargo:
```
cargo run --release -- ~/SICP/chapter1/exercise1-27.scm --non-interactive
```
Running the binary directly:
```
interpreter ~/SICP/chapter1/exercise1-27.scm --non-interactive
```


