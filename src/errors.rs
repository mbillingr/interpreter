pub use errors_impl::*;

#[allow(deprecated)]
mod errors_impl {
    use crate::symbol::Symbol;

    error_chain! {
        links {
        }

        errors {
            GenericError(msg: String) {
                display("Error: {}", msg)
            }

            ArgumentError

            TypeError(msg: String) {
                display("Error: {}", msg)
            }

            Undefined(symbol: Symbol) {
                display("Undefined symbol: {}", symbol.name())
            }

            UnexpectedToken(repr: String) {
                display("Unexpected token: {:?}", repr)
            }

            UnexpectedEof

            UndelimitedString

            UnexpectedCharacter(expected: char, got: char) {
                display("Expected character '{}' but found '{}'", expected, got)
            }

            Utf8Error
        }

        foreign_links {
            Io(::std::io::Error);
            ReadlineError(rustyline::error::ReadlineError);
        }
    }

}
