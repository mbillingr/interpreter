use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::Expression;
use crate::expression::Expression::Symbol;

pub struct Debugger {
    env: EnvRef,
    expr: Result<Expression>,

    //stack: Vec<Debugger>,
    history: Vec<(Expression, std::result::Result<Expression, String>)>,
}

impl Debugger {
    pub fn new(expr: Expression, env: EnvRef) -> Self {
        Debugger {
            env,
            expr: Ok(expr),
            //stack: vec![],
            history: vec![],
        }
    }

    pub fn current_env(&self) -> &EnvRef {
        &self.env
    }

    pub fn current_expr(&self) -> &Result<Expression> {
        &self.expr
    }

    pub fn history(&self) -> &[(Expression, std::result::Result<Expression, String>)] {
        &self.history
    }

    // todo: how to get rid of code duplication with interpreter?
    pub fn eval(&mut self) {
        use Expression::*;
        match &self.expr {
            Err(_) => {}
            Ok(expr) => {
                let input = expr.clone();
                match expr {
                    Symbol(ref s) => {
                        self.expr = self
                            .env
                            .borrow()
                            .lookup(&s)
                            .ok_or_else(|| ErrorKind::Undefined(*s).into());
                    }
                    Undefined | Nil | Integer(_) | Float(_) | String(_) | Char(_) | True
                    | False | Procedure(_) | Macro(_) | Error(_) | Native(_)
                    | NativeIntrusive(_) => {
                        self.expr = Ok(expr.clone());
                    }
                    Pair(_) => unimplemented!(),
                }
                self.history.push((
                    input,
                    match &self.expr {
                        Ok(expr) => Ok(expr.clone()),
                        Err(e) => Err(e.to_string()),
                    },
                ))
            }
        }
    }
}
