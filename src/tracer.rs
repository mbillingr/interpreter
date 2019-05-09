use crate::environment::{EnvRef, EnvWeak};
use crate::expression::{Expression, Procedure};
use crate::symbol::{self, Symbol};
use std::any::Any;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

pub trait Tracer {
    fn trace_procedure_call(
        &mut self,
        proc: &Procedure<EnvRef>,
        inner_env: &EnvRef,
        outer_env: &EnvRef,
    );

    fn as_any(&self) -> &dyn Any;
}

thread_local! {
    static TRACERS: RefCell<HashMap<usize, Box<dyn Tracer>>> = RefCell::new(HashMap::new());
    static NEXT_ID: Cell<usize> = Cell::new(0);
}

#[derive(Eq, PartialEq, Hash)]
pub struct TracerId(usize);

fn next_id() -> usize {
    NEXT_ID.with(|id| id.replace(id.get() + 1))
}

pub fn trace_procedure_call(proc: &Procedure<EnvRef>, inner_env: &EnvRef, outer_env: &EnvRef) {
    TRACERS.with(|tracers| {
        for tracer in tracers.borrow_mut().values_mut() {
            tracer.trace_procedure_call(proc, inner_env, outer_env);
        }
    });
}

pub fn install_tracer<T: Tracer + 'static>(tracer: T) -> TracerId {
    TRACERS.with(|tracers| {
        let mut tracers = tracers.borrow_mut();
        let id = next_id();
        tracers.insert(id, Box::new(tracer));
        TracerId(id)
    })
}

pub fn remove_tracer(id: TracerId) -> Box<dyn Tracer> {
    TRACERS.with(|tracers| {
        let mut tracers = tracers.borrow_mut();
        tracers.remove(&id.0).unwrap()
    })
}

#[derive(Debug)]
struct Call {
    caller: Procedure<EnvWeak>,
    callee: Procedure<EnvWeak>,
    params: Vec<(Symbol, Expression)>,
}

#[derive(Debug)]
pub struct CallGraph {
    call_history: Vec<Call>,
    call_counts: HashMap<Procedure<EnvWeak>, HashMap<Procedure<EnvWeak>, usize>>,
}

impl Tracer for CallGraph {
    fn trace_procedure_call(
        &mut self,
        callee: &Procedure<EnvRef>,
        inner_env: &EnvRef,
        outer_env: &EnvRef,
    ) {
        let caller = outer_env.borrow().current_procedure().clone();
        let mut caller_scope = outer_env.borrow().get_scope();
        caller_scope.push(caller.name());
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

        println!("{:?}", callee.params_ex());
        println!("{:?}", inner_env.borrow());

        let mut params = vec![];
        let mut param = callee.params_ex();
        loop {
            let p = match param {
                Expression::Nil => break,
                Expression::Symbol(s) => s,
                Expression::Pair(p) if p.car.is_symbol() => {
                    param = &p.cdr;
                    p.car.try_as_symbol().unwrap()
                }
                _ => unreachable!(), // there should never be any other type than symbols in the parameter list
            };

            if *p != symbol::DOT {
                let arg = inner_env.borrow().lookup(dbg!(p)).unwrap();
                params.push((*p, arg));
            }
        }

        println!("{} -> {}", caller_string, callee_string);

        let call = Call {
            caller: caller.clone(),
            callee: callee.clone().into(),
            params,
        };

        self.call_history.push(call);

        *self
            .call_counts
            .entry(caller)
            .or_default()
            .entry(callee.clone().into())
            .or_insert(0) += 1;
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl CallGraph {
    pub fn new() -> Self {
        CallGraph {
            call_history: Vec::new(),
            call_counts: HashMap::new(),
        }
    }
}
