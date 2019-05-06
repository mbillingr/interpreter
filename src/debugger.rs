use crate::environment::EnvRef;
use crate::expression::Expression;

struct DebugFrame {
    env: EnvRef,
    expr: Expression,
}

pub struct Debugger {
    frames: Vec<DebugFrame>,
}

impl Debugger {
    pub fn new(expr: Expression, env: EnvRef) -> Self {
        Debugger {
            frames: vec![DebugFrame { env, expr }],
        }
    }

    pub fn current_env(&self) -> &EnvRef {
        &self.frames.last().unwrap().env
    }
}
