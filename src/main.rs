#[macro_use]
extern crate error_chain;

mod errors;

mod lexer;

use error_chain::ChainedError;
use errors::*;
use lexer::{Lexer, Token};
use std::collections::HashMap;
use std::io::{self, Write};

#[derive(Clone)]
enum Expression {
    /// for now use a Vec... maybe change to linked list in the future?
    List(Vec<Expression>),
    Symbol(String),
    String(String),
    Integer(i64),
    Float(f64),
    True,
    False,
    Native(fn(Vec<Expression>) -> Result<Expression>),
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::List(l) => {
                let tmp: Vec<_> = l.iter().map(|item| format!("{}", item)).collect();
                write!(f, "({})", tmp.join(" "))
            }
            Expression::Symbol(s) => write!(f, "{}", s),
            Expression::String(s) => write!(f, "{:?}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Native(_) => write!(f, "<native>"),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::List(l) => {
                let tmp: Vec<_> = l.iter().map(|item| format!("{}", item)).collect();
                write!(f, "({})", tmp.join(" "))
            }
            Expression::Symbol(s) => write!(f, "{}", s),
            Expression::String(s) => write!(f, "{:?}", s),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(i) => write!(f, "{}", i),
            Expression::True => write!(f, "#t"),
            Expression::False => write!(f, "#f"),
            Expression::Native(_) => write!(f, "<native>"),
        }
    }
}

impl From<String> for Expression {
    fn from(s: String) -> Self {
        match s.as_str() {
            "#t" => return Expression::True,
            "#f" => return Expression::False,
            _ => {}
        }

        if let Ok(i) = s.parse() {
            return Expression::Integer(i);
        }

        if let Ok(f) = s.parse() {
            return Expression::Float(f);
        }

        return Expression::Symbol(s);
    }
}

impl std::ops::Add for Expression {
    type Output = Result<Expression>;
    fn add(self, other: Self) -> Self::Output {
        use Expression::*;
        match (self, other) {
            (Integer(a), Integer(b)) => Ok(Integer(a + b)),
            (Integer(a), Float(b)) => Ok(Float(a as f64 + b)),
            (Float(a), Integer(b)) => Ok(Float(a + b as f64)),
            (Float(a), Float(b)) => Ok(Float(a + b)),
            (a, b) => Err(ErrorKind::TypeError(format!("Cannot add {} and {}", a, b)).into()),
        }
    }
}

impl Expression {
    fn call(&self, args: Vec<Expression>) -> Result<Expression> {
        match self {
            Expression::Native(func) => func(args),
            _ => Err(ErrorKind::TypeError("not callable".to_string()).into()),
        }
    }
}

fn parse<R: io::BufRead>(input: &mut Lexer<R>) -> Result<Expression> {
    fn read_ahead<R: io::BufRead>(token: Token, input: &mut Lexer<R>) -> Result<Expression> {
        match token {
            Token::String(s) => Ok(Expression::String(s)),
            Token::Symbol(s) => Ok(s.into()),
            Token::ListOpen => {
                let mut list = Vec::new();
                loop {
                    let token = input.next_token()?;
                    match token {
                        Token::ListClose => return Ok(Expression::List(list)),
                        _ => list.push(read_ahead(token, input)?),
                    }
                }
            }
            Token::ListClose => Err(ErrorKind::UnexpectedToken(token.into()).into()),
        }
    }

    let token = input.next_token()?;
    read_ahead(token, input)
}

type Environment = HashMap<String, Expression>;

fn default_env() -> Environment {
    let mut env = HashMap::new();
    env.insert("+".to_string(), Expression::Native(native_add));
    env
}

fn native_add(args: Vec<Expression>) -> Result<Expression> {
    let mut args = args.into_iter();
    let mut a = args.next().ok_or(ErrorKind::ArgumentError)?;
    for b in args {
        a = (a + b)?;
    }
    Ok(a)
}

/// simple version without tail calls
fn eval(expr: Expression, env: &mut Environment) -> Result<Expression> {
    match expr {
        Expression::Symbol(s) => env
            .get(&s)
            .cloned()
            .ok_or_else(|| ErrorKind::Undefined(s).into()),
        Expression::Integer(_)
        | Expression::Float(_)
        | Expression::String(_)
        | Expression::True
        | Expression::False
        | Expression::Native(_) => Ok(expr),
        Expression::List(l) => match l.first() {
            None => panic!("empty list"),
            Some(_) => {
                let mut items = l.into_iter();
                let proc = eval(items.next().unwrap(), env)?;
                let args: Vec<Expression> =
                    items.map(|arg| eval(arg, env)).collect::<Result<_>>()?;
                proc.call(args)
            }
        },
    }
}

fn repl<R: io::BufRead>(input: &mut Lexer<R>, global: &mut Environment) -> Result<()> {
    print!(">> ");
    io::stdout().flush().unwrap();
    let expr = parse(input)?;
    let res = eval(expr, global)?;
    println!("{}", res);
    Ok(())
}

fn main() {
    let mut src = Lexer::new(io::BufReader::new(io::stdin()));
    let mut global = default_env();
    loop {
        match repl(&mut src, &mut global) {
            Ok(_) => {}
            Err(Error(ErrorKind::UnexpectedEof, _)) => {
                println!("EOF");
                break;
            }
            Err(e) => report_error(e),
        }
    }
}

fn report_error(e: Error) {
    eprintln!("{}", e);
    eprintln!("{}", e.display_chain().to_string());
}
