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
                Some(list) => Expression::List(list),
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
        List(l) => match l.first() {
            Some(Symbol(s)) if s == "cond" => transform_cond(l),
            Some(Symbol(s)) if s == "define" => transform_define(l),
            Some(Symbol(s)) if s == "if" => transform_if(l),
            Some(Symbol(s)) if s == "lambda" => transform_lambda(l),
            Some(Symbol(s)) if s == "let" => transform_let(l),
            _ => l
                .into_iter()
                .map(transform)
                .collect::<Result<_>>()
                .map(List),
        },
        _ => Ok(expr),
    }
}

fn transform_define(list: List) -> Result<Expression> {
    if list.len() < 3 {
        return Err(ErrorKind::ArgumentError.into());
    }

    let mut list: Vec<_> = list.into_iter().map(transform).collect::<Result<_>>()?;

    if list[1].is_symbol() {
        Ok(Expression::List(list))
    } else if list[1].is_list() {
        let mut new_body = vec![];
        while list.len() > 2 {
            new_body.push(list.pop().unwrap());
        }
        new_body.push(Expression::Symbol("begin".to_string()));
        new_body.reverse();
        list.push(Expression::List(new_body));
        Ok(Expression::List(list))
    } else {
        Err(ErrorKind::TypeError(format!("Cannot use {} as signature.", list[1])).into())
    }
}

fn transform_lambda(list: List) -> Result<Expression> {
    let ((_, signature), body): ((Expression, List), _) = list.tail_destructure()?;

    if body.len() == 1 {
        Ok(scheme!(lambda, @signature, @body.into_iter().next().unwrap()))
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
            if let Expression::List(mut path) = item {
                if let Some(Expression::Symbol(f)) = path.first() {
                    if f == "else" {
                        path[0] = Expression::True;
                    }
                }
                transform(Expression::List(path))
            } else {
                Ok(item)
            }
        })
        .collect::<Result<_>>()?;
    Ok(Expression::List(list))
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

    Ok(Expression::List(list))
}

fn transform_let(list: List) -> Result<Expression> {
    let (_, assignments, body): (Expression, List, Expression) = list.destructure()?;

    let mut lambda_form = List::new();
    lambda_form.push(Expression::Symbol("lambda".into()));

    let mut vars = List::with_capacity(assignments.len());
    let mut exps = List::with_capacity(assignments.len());

    for vx in assignments {
        let (var, expr) = vx.destructure()?;
        vars.push(var);
        exps.push(expr);
    }

    let lambda_form = scheme!(lambda, @vars, @transform(body)?);
    exps.insert(0, lambda_form);
    Ok(Expression::List(exps))
}
