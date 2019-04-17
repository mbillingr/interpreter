use crate::environment::{EnvRef, EnvWeak};
use crate::expression::{Expression, Procedure};
use crate::symbol::Symbol;
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::sync::Mutex;

pub trait Tracer {
    fn trace_procedure_call(
        &mut self,
        proc: &Procedure<EnvRef>,
        inner_env: &EnvRef,
        outer_env: &EnvRef,
    );
}

thread_local! {
    pub static TRACERS: RefCell<Vec<Box<dyn Tracer>>> = RefCell::new(vec![CallGraph::new()]);
}

pub fn trace_procedure_call(proc: &Procedure<EnvRef>, inner_env: &EnvRef, outer_env: &EnvRef) {
    TRACERS.with(|tracers| {
        for tracer in tracers.borrow_mut().iter_mut() {
            tracer.trace_procedure_call(proc, inner_env, outer_env);
        }
    });
}

struct Call {
    caller: Option<Procedure<EnvWeak>>,
    callee: Procedure<EnvWeak>,
    params: Vec<(Symbol, Expression)>,
}

struct CallGraph {
    call_history: Vec<Call>,
}

impl Tracer for CallGraph {
    fn trace_procedure_call(
        &mut self,
        callee: &Procedure<EnvRef>,
        inner_env: &EnvRef,
        outer_env: &EnvRef,
    ) {
        let caller = outer_env.borrow().current_procedure().cloned();
        let mut caller_scope = outer_env.borrow().get_scope();
        caller_scope.extend(caller.as_ref().map(|p| p.name()));
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
                let param = a.try_as_symbol().unwrap();
                let arg = inner_env.borrow().lookup(param).unwrap();
                (*param, arg)
            })
            .collect();

        println!("{} -> {}", caller_string, callee_string);

        let call = Call {
            caller: caller,
            callee: callee.clone().into(),
            params,
        };

        self.call_history.push(call);
    }
}

impl CallGraph {
    fn new() -> Box<Self> {
        Box::new(CallGraph {
            call_history: Vec::new(),
        })
    }
}
