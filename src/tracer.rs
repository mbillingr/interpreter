use crate::environment::EnvRef;
use crate::expression::Procedure;
use lazy_static::lazy_static;
use std::sync::Mutex;

pub trait Tracer: Send {
    fn trace_procedure_call(
        &mut self,
        proc: &Procedure<EnvRef>,
        inner_env: &EnvRef,
        outer_env: &EnvRef,
    );
}

lazy_static! {
    static ref TRACERS: Mutex<Vec<Box<dyn Tracer>>> = Mutex::new(vec![CallGraph::new()]);
}

pub fn trace_procedure_call(proc: &Procedure<EnvRef>, inner_env: &EnvRef, outer_env: &EnvRef) {
    for tracer in TRACERS.lock().unwrap().iter_mut() {
        tracer.trace_procedure_call(proc, inner_env, outer_env);
    }
}

struct CallGraph {}

impl Tracer for CallGraph {
    fn trace_procedure_call(
        &mut self,
        callee: &Procedure<EnvRef>,
        inner_env: &EnvRef,
        outer_env: &EnvRef,
    ) {
        let mut caller_scope = outer_env.borrow().get_scope();
        caller_scope.extend(outer_env.borrow().current_procedure().map(|p| p.name()));
        let caller_string = caller_scope
            .into_iter()
            .map(|s| format!("{}", s))
            .collect::<Vec<_>>()
            .join(".");

        let mut callee_scope = inner_env.borrow().get_scope();
        callee_scope.push(callee.name());
        let callee_string = callee_scope
            .into_iter()
            .map(|s| format!("{}", s))
            .collect::<Vec<_>>()
            .join(".");

        let params: Vec<_> = callee
            .params_ex()
            .try_to_vec()
            .unwrap()
            .into_iter()
            .map(|a| {
                let val = inner_env
                    .borrow()
                    .lookup(a.try_as_symbol().unwrap())
                    .unwrap();
                format!("{}: {}", a, val.short_repr())
            })
            .collect();

        println!(
            "{} -> {}({})",
            caller_string,
            callee_string,
            params.join(", ")
        );
    }
}

impl CallGraph {
    fn new() -> Box<Self> {
        Box::new(CallGraph {})
    }
}
