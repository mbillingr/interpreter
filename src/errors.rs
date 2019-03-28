pub use errors::*;

#[allow(deprecated)]
mod errors {
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

            Undefined(symbol: String) {
                display("Undefined symbol: {}", symbol)
            }

            UnexpectedToken(repr: String) {
                display("Unexpected token: {:?}", repr)
            }

            UnexpectedEof

            UnexpectedCharacter(expected: char, got: char) {
                display("Expected character '{}' but found '{}'", expected, got)
            }

            Utf8Error
        }

        foreign_links {
            Io(::std::io::Error);
        }
    }

}
