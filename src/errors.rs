use crate::expression::Expression;
use crate::symbol::Symbol;
use rustyline::error::ReadlineError;

pub trait IntoResultExpression {
    fn into_result(self) -> Result<Expression>;
}

impl<T: Into<Expression>> IntoResultExpression for T {
    fn into_result(self) -> Result<Expression> {
        Ok(self.into())
    }
}

impl IntoResultExpression for Result<Expression> {
    fn into_result(self) -> Result<Expression> {
        self
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum ErrorKind {
    ArgumentError,
    FileNotFoundError(String),
    GenericError(String),
    TypeError(String),
    Unbound(Symbol),
    UndefinedExport(Symbol),
    UndelimitedString,
    UnexpectedEof,
    UnexpectedToken { found: String, expected: String },

    IoError(std::io::Error),
    ReadlineError(ReadlineError),
}

// here we can add some context to the error
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unimplemented!()
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error { kind }
    }
}

impl From<std::io::Error> for Error {
    fn from(ioe: std::io::Error) -> Self {
        Error {
            kind: ErrorKind::IoError(ioe),
        }
    }
}

impl From<ReadlineError> for Error {
    fn from(rle: ReadlineError) -> Self {
        Error {
            kind: ErrorKind::ReadlineError(rle),
        }
    }
}

impl From<String> for Error {
    fn from(msg: String) -> Self {
        Error {
            kind: ErrorKind::GenericError(msg),
        }
    }
}

impl From<&str> for Error {
    fn from(msg: &str) -> Self {
        msg.to_string().into()
    }
}

/*pub use errors_impl::*;


#[allow(deprecated)]
mod errors_impl {
    use crate::symbol::Symbol;

    error_chain! {
        links {
        }

        errors {
            GenericError(msg: String) {
                display("{}", msg)
            }

            /*ArgumentError(proc: String, args: Vec<String>) {
                display("Argument Error: {} {:?}", proc, args)
            }*/

            TooManyArguments

            MissingArguments(msg: String) {
                display("Missing Arguments: {}", msg)
            }

            TypeError(msg: String) {
                display("Error: {}", msg)
            }

            Undefined(symbol: Symbol) {
                display("Undefined symbol: {}", symbol.name())
            }

            UnexpectedToken(repr: String, expected: String) {
                display("Unexpected token: {:?} ... expected {} instead.", repr, expected)
            }

            UnexpectedEof

            UndelimitedString

            UnexpectedCharacter(expected: char, got: char) {
                display("Expected character '{}' but found '{}'", expected, got)
            }

            FileNotFoundError(filename: String) {
                display("File not found: {}", filename)
            }

            Utf8Error

            UndefinedExport(symbol: Symbol) {
                display("Undefined export symbol: {}", symbol.name())
            }
        }

        foreign_links {
            Io(::std::io::Error);
            ReadlineError(rustyline::error::ReadlineError);
        }
    }

}
*/
