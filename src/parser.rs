use crate::destructure::Destructure;
use crate::errors::*;
use crate::expression::{Expression, List};
use crate::lexer::Token;

pub struct Parser {
    list_stack: Vec<List>,
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
                self.list_stack.push(List::new());
                return Ok(None);
            }
            Token::ListClose => match self.list_stack.pop() {
                Some(list) => Expression::from_vec(list),
                None => return Err(ErrorKind::UnexpectedToken(token.into()))?,
            },
        };

        match self.list_stack.last_mut() {
            Some(list) => {
                list.push(expr);
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
        Pair(_, _) => {
            let l = expr.try_into_list()?;
            match l.first() {
                Some(Symbol(s)) if s == "cond" => transform_cond(l),
                Some(Symbol(s)) if s == "define" => transform_define(l),
                Some(Symbol(s)) if s == "if" => transform_if(l),
                Some(Symbol(s)) if s == "lambda" => transform_lambda(l),
                Some(Symbol(s)) if s == "let" => transform_let(l),
                _ => l
                    .into_iter()
                    .map(transform)
                    .collect::<Result<_>>()
                    .map(Expression::from_vec),
            }
        }
        _ => Ok(expr),
    }
}

fn transform_define(list: List) -> Result<Expression> {
    let ((_, signature), body): ((Expression, Expression), _) = list.tail_destructure()?;

    if signature.is_symbol() {
        let value = transform(body.destructure()?)?;
        Ok(scheme!(define, @signature, @value))
    } else if signature.is_list() {
        let (name, params): (Expression, _) =
            signature.try_into_list().unwrap().tail_destructure()?;

        let lambda = scheme!(lambda, @params, ...Expression::from_vec(body));
        let lambda = transform_lambda(lambda.try_into_list().unwrap())?;

        Ok(scheme!(define, @name, @lambda))
    } else {
        Err(ErrorKind::TypeError(format!("invalid signature: {:?}", signature)).into())
    }
}

fn transform_lambda(list: List) -> Result<Expression> {
    let ((_, signature), body): ((Expression, List), _) = list.tail_destructure()?;

    if body.len() == 1 {
        let body = transform(body.into_iter().next().unwrap())?;
        Ok(scheme!(lambda, @signature, @body))
    } else {
        let mut new_body = vec![scheme!(begin)];
        new_body.reserve(body.len());
        for expr in body {
            new_body.push(transform(expr)?);
        }

        Ok(scheme!(lambda, @signature, @new_body))
    }
}

fn transform_cond(list: List) -> Result<Expression> {
    let list = list
        .into_iter()
        .map(|item| {
            if let Expression::Pair(_, _) = item {
                let mut path = item.try_into_list().unwrap();
                if let Some(Expression::Symbol(f)) = path.first() {
                    if f == "else" {
                        path[0] = Expression::True;
                    }
                }
                transform(Expression::from_vec(path))
            } else {
                Ok(item)
            }
        })
        .collect::<Result<_>>()?;
    Ok(Expression::from_vec(list))
}

fn transform_if(list: List) -> Result<Expression> {
    if list.len() < 3 || list.len() > 4 {
        return Err(ErrorKind::ArgumentError.into());
    }

    let mut list = list
        .into_iter()
        .map(transform)
        .collect::<Result<Vec<_>>>()?;

    if list.len() == 3 {
        list.push(Expression::Undefined);
    }

    Ok(Expression::from_vec(list))
}

fn transform_let(list: List) -> Result<Expression> {
    let ((_, assignments), body): ((Expression, List), _) = list.tail_destructure()?;

    let mut lambda_form = List::new();
    lambda_form.push(Expression::Symbol("lambda".into()));

    let mut vars = List::with_capacity(assignments.len());
    let mut exps = List::with_capacity(assignments.len());

    for vx in assignments {
        let (var, expr) = vx.destructure()?;
        vars.push(var);
        exps.push(expr);
    }

    let lambda_form = scheme!(lambda, @vars, ...Expression::from_vec(body));
    exps.insert(0, lambda_form);
    transform(Expression::from_vec(exps))
}
