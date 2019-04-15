
# A partial Scheme Interpreter

I use it for studying SICP.

To run it enter the `interpreter` directory and type `cargo run --release`. This will drop you into the REPL.
Any command line arguments are treated as file names to run in sequence before entering the REPL.

For example, to run an exercise (most of which depend on `utils.scm`):
```
cargo run --release utils.scm ../chapter1/exercise1-27.scm
```
