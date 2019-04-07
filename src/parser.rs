use crate::errors::*;
use crate::expression::Expression;
use crate::lexer::Token;

pub struct Parser {
    list_stack: Vec<Expression>,
}

impl Parser {
    pub fn new() -> Self {
        Parser { list_stack: vec![] }
    }

    pub fn push_token(&mut self, token: Token) -> Result<Option<Expression>> {
        self.parse_expression(token)
            .and_then(|o| o.map(transform).transpose())
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
                self.list_stack.push(list.append(Expression::cons(expr, Expression::Nil))?);
                Ok(None)
            }
            None => Ok(Some(expr)),
        }
    }
}

// convert some syntactic forms, expand macros(?), check errors, ... (mostly to-do)
fn transform(expr: Expression) -> Result<Expression> {
    use Expression::*;
    match expr {
        Pair(ref car, _) => {
            match **car {
                Symbol(ref s) if s == "cond" => transform_cond(expr),
                Symbol(ref s) if s == "define" => transform_define(expr),
                Symbol(ref s) if s == "if" => transform_if(expr),
                Symbol(ref s) if s == "lambda" => transform_lambda(expr),
                Symbol(ref s) if s == "let" => transform_let(expr),
                _ => expr.map(transform)
            }
        }
        _ => Ok(expr),
    }
}

fn transform_define(mut list: Expression) -> Result<Expression> {
    assert_eq!(Some(scheme!(define)), list.next()?);
    let mut signature = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let body = list;

    if signature.is_symbol() {
        if *body.cdr()? != Expression::Nil {
            return Err(ErrorKind::ArgumentError)?
        }
        let value = body.try_into_car()?;
        Ok(scheme!(define, @signature, @transform(value)?))
    } else if signature.is_list() {
        let name = signature.next()?.ok_or(ErrorKind::ArgumentError)?;

        let lambda = scheme!(lambda, @signature, ...body);
        let lambda = transform_lambda(lambda)?;

        Ok(scheme!(define, @name, @lambda))
    } else {
        Err(ErrorKind::TypeError(format!("invalid signature: {:?}", signature)).into())
    }
}

fn transform_lambda(mut list: Expression) -> Result<Expression> {
    assert_eq!(Some(scheme!(lambda)), list.next()?);
    let signature = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let body = list;

    if *body.cdr().unwrap() == Expression::Nil {
        Ok(scheme!(lambda, @signature, @transform(body.try_into_car()?)?))
    } else {
        let body = Expression::cons(scheme!(begin), body.map(transform)?);
        Ok(scheme!(lambda, @signature, @body))
    }
}

fn transform_cond(mut list: Expression) -> Result<Expression> {
    assert_eq!(Some(scheme!(cond)), list.next()?);
    
    let mut result = scheme!(cond,);
    let mut current = result.cdr_mut().unwrap();


    while let Some(mut item) = list.next()? {
        if let Expression::Symbol(s) = item.car_mut()? {
            if s == "else" {
                *item.car_mut()? = Expression::True;
            }
        }

        *current = Expression::cons(transform(item)?, Expression::Nil);
        current = current.cdr_mut().unwrap();
    }

    Ok(result)
}

fn transform_if(mut list: Expression) -> Result<Expression> {
    assert_eq!(Some(scheme!(if)), list.next()?);

    let cond = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let if_ = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let else_ = list.next()?.unwrap_or(Expression::Undefined);

    Ok(scheme!(if, @transform(cond)?, @transform(if_)?, @transform(else_)?))
}

fn transform_let(mut list: Expression) -> Result<Expression> {
    assert_eq!(Some(scheme!(let)), list.next()?);
    let assignments = list.next()?.ok_or(ErrorKind::ArgumentError)?;
    let body = list;

    let mut vars = Expression::Nil;
    let mut exps = Expression::Nil;

    for vx in assignments {
        let mut vx = vx?;
        let var = vx.next()?.ok_or(ErrorKind::ArgumentError)?;
        let exp = vx.next()?.ok_or(ErrorKind::ArgumentError)?;

        vars = vars.append(Expression::cons(var, Expression::Nil))?;
        exps = exps.append(Expression::cons(exp, Expression::Nil))?;
    }

    let lambda_form = scheme!(lambda, @vars, ...body);
    exps = Expression::cons(lambda_form, exps);
    transform(exps)
}
