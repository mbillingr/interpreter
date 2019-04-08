use crate::errors::*;
use crate::expression::Expression;
use crate::lexer::Token;
use std::rc::Rc;

pub struct Parser {
    list_stack: Vec<Expression>,
}

impl Parser {
    pub fn new() -> Self {
        Parser { list_stack: vec![] }
    }

    pub fn push_token(&mut self, token: Token) -> Result<Option<Expression>> {
        self.parse_expression(token)
            .and_then(|o| o.map(|e| transform(&e)).transpose())
    }

    fn parse_expression(&mut self, token: Token) -> Result<Option<Expression>> {
        let expr = match token {
            Token::String(s) => Expression::String(s),
            Token::Symbol(s) => Expression::from_literal(s),
            Token::ListOpen => {
                self.list_stack.push(Expression::Nil);
                return Ok(None);
            }
            Token::ListClose => match self.list_stack.pop() {
                Some(list) => list,
                None => return Err(ErrorKind::UnexpectedToken(token.into()))?,
            },
        };

        match self.list_stack.pop() {
            Some(list) => {
                self.list_stack
                    .push(list.append(Expression::cons(expr, Expression::Nil))?);
                Ok(None)
            }
            None => Ok(Some(expr)),
        }
    }
}

// convert some syntactic forms, expand macros(?), check errors, ... (mostly to-do)
fn transform(expr: &Expression) -> Result<Expression> {
    use Expression::*;
    match expr {
        Pair(ref car, _) => match **car {
            Symbol(ref s) if s == "cond" => transform_cond(expr),
            Symbol(ref s) if s == "define" => transform_define(expr),
            Symbol(ref s) if s == "if" => transform_if(expr),
            Symbol(ref s) if s == "lambda" => transform_lambda(expr),
            Symbol(ref s) if s == "let" => transform_let(expr),
            _ => expr.map_list(|e| transform(&e)),
        },
        _ => Ok(expr.clone()),
    }
}

fn transform_define(list: &Expression) -> Result<Expression> {
    assert_eq!(&scheme!(define), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    if signature.is_symbol() {
        if body.cdr()? != &Expression::Nil {
            return Err(ErrorKind::ArgumentError)?;
        }
        let value = body.car()?;
        Ok(scheme!(define, @signature.clone(), @transform(&value)?))
    } else if signature.is_list() {
        let (name, signature) = signature.decons().map_err(|_| ErrorKind::ArgumentError)?;

        let lambda = scheme!(lambda, @signature.clone(), ...body.clone());
        let lambda = transform_lambda(&lambda)?;

        Ok(scheme!(define, @name.clone(), @lambda))
    } else {
        Err(ErrorKind::TypeError(format!("invalid signature: {:?}", signature)).into())
    }
}

fn transform_lambda(list: &Expression) -> Result<Expression> {
    assert_eq!(&scheme!(lambda), list.car()?);
    let (signature, body) = list.cdr()?.decons().map_err(|_| ErrorKind::ArgumentError)?;

    if body.cdr().unwrap() == &Expression::Nil {
        Ok(scheme!(lambda, @signature.clone(), @transform(body.car()?)?))
    } else {
        let body = Expression::cons(scheme!(begin), body.map_list(|e| transform(&e))?);
        Ok(scheme!(lambda, @signature.clone(), @body))
    }
}

fn transform_cond(list: &Expression) -> Result<Expression> {
    assert_eq!(&scheme!(cond), list.car()?);
    let body = list.cdr()?.map_list(|row| {
        let row = match row {
            Expression::Pair(car, cdr) => {
                let car = if let Expression::Symbol(s) = &**car {
                    if s == "else" {
                        Rc::new(Expression::True)
                    } else {
                        car.clone()
                    }
                } else {
                    car.clone()
                };
                Expression::Pair(car, cdr.clone())
            }
            row => row.clone(),
        };
        transform(&row)
    })?;
    Ok(Expression::cons(scheme!(cond), body))
}

fn transform_if(list: &Expression) -> Result<Expression> {
    let mut list = list.iter_list()?;

    assert_eq!(Some(&scheme!(if)), list.next_expr()?);
    let cond = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let if_ = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
    let else_ = list.next_expr()?.unwrap_or(&Expression::Undefined);

    Ok(scheme!(if, @transform(cond)?, @transform(if_)?, @transform(else_)?))
}

fn transform_let(list: &Expression) -> Result<Expression> {
    let mut list = list.iter_list()?;

    assert_eq!(Some(&scheme!(let)), list.next_expr()?);

    // need to get the tail first, because next_expr() advances the iterator into the tail
    let body = list.tail()?;
    let assignments = list.next_expr()?.ok_or(ErrorKind::ArgumentError)?;

    let mut vars = Expression::Nil;
    let mut exps = Expression::Nil;

    for vx in assignments.iter_list()? {
        let mut vx = vx?.iter_list()?;
        let var = vx.next_expr()?.ok_or(ErrorKind::ArgumentError)?;
        let exp = vx.next_expr()?.ok_or(ErrorKind::ArgumentError)?;

        vars = vars.append(Expression::cons(var.clone(), Expression::Nil))?;
        exps = exps.append(Expression::cons(exp.clone(), Expression::Nil))?;
    }

    let lambda_form = scheme!(lambda, @vars, ...body.clone());
    exps = Expression::cons(lambda_form, exps);
    transform(&exps)
}
