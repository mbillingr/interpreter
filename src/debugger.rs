use crate::environment::EnvRef;
use crate::errors::*;
use crate::expression::Expression;
use crate::interpreter::{eval, is_special_form};
use std::cell::RefCell;
use std::sync::mpsc;
use std::thread;

thread_local! {
    static DEBUGGER: RefCell<Option<Requester<DebugRequest, ()>>> = RefCell::new(None);
}

pub fn install_thread_debugger(service: Requester<DebugRequest, ()>) {
    DEBUGGER.with(|debugger| {
        *debugger.borrow_mut() = Some(service);
    });
}

pub fn enter_eval(expr: &Expression, env: &EnvRef) {
    DEBUGGER.with(|debugger| {
        if let Some(service) = &*debugger.borrow() {
            service.req(DebugRequest::EnterEval(expr.clone(), env.clone()))
        }
    });
}

pub fn leave_eval(res: &Result<Expression>) {
    DEBUGGER.with(|debugger| {
        if let Some(service) = &*debugger.borrow() {
            let res = match res {
                Ok(expr) => Ok(expr.clone()),
                Err(e) => Err(ErrorKind::GenericError(e.to_string()).into()),
            };
            service.req(DebugRequest::LeaveEval(res))
        }
    });
}

pub fn predispatch(expr: &Expression, env: &EnvRef) {
    DEBUGGER.with(|debugger| {
        if let Some(service) = &*debugger.borrow() {
            service.req(DebugRequest::Predispatch(expr.clone(), env.clone()))
        }
    });
}

pub fn function_call(proc: &Expression, args: &Expression, expr: &Expression) {
    DEBUGGER.with(|debugger| {
        if let Some(service) = &*debugger.borrow() {
            service.req(DebugRequest::FunctionCall(
                proc.clone(),
                args.clone(),
                expr.clone(),
            ))
        }
    });
}

pub enum DebugRequest {
    EnterEval(Expression, EnvRef),
    LeaveEval(Result<Expression>),
    Predispatch(Expression, EnvRef),
    FunctionCall(Expression, Expression, Expression),
}

pub struct Debugger {
    service: Replier<DebugRequest, ()>,
    current_request: Option<DebugRequest>,

    stack: Vec<(Expression, EnvRef)>,
    //history: Vec<(Expression, std::result::Result<Expression, String>)>,
}

impl Debugger {
    pub fn new(expr: Expression, env: EnvRef) -> Self {
        let (req, rep) = make_request_reply();

        // todo: kill thread when debugger is dropped?
        thread::spawn(move || {
            install_thread_debugger(req);
            let _ = eval(&expr, env);
            println!("bye-bye debug thread")
        });

        Debugger {
            service: rep,
            current_request: None,
            stack: vec![],
            //history: vec![],
        }
    }

    pub fn poll(&mut self) {
        if self.current_request.is_none() {
            match self.service.poll() {
                Some(DebugRequest::EnterEval(expr, env)) => {
                    self.service.rep(());
                    self.stack.push((expr, env));
                }
                Some(DebugRequest::LeaveEval(_)) => {
                    self.service.rep(());
                    self.stack.pop().expect("stack underflow");
                }
                Some(DebugRequest::Predispatch(expr, env)) => {
                    if is_special_form(&expr) {
                        self.current_request =
                            Some(DebugRequest::Predispatch(expr.clone(), env.clone()));
                    } else {
                        self.service.rep(());
                    }
                    self.stack.pop().expect("stack underflow");
                    self.stack.push((expr, env));
                }
                fc @ Some(DebugRequest::FunctionCall(_, _, _)) => self.current_request = fc,
                None => {}
            }
        }
    }

    pub fn advance(&mut self) {
        if self.current_request.is_some() {
            self.service.rep(());
            self.current_request = None;
        }
    }

    pub fn current_request(&self) -> Option<&DebugRequest> {
        self.current_request.as_ref()
    }

    pub fn current_env(&self) -> Option<&EnvRef> {
        self.stack.last().map(|frame| &frame.1)
    }

    /*pub fn history(&self) -> &[(Expression, std::result::Result<Expression, String>)] {
        &self.history
    }*/
}

fn make_request_reply<Q, P>() -> (Requester<Q, P>, Replier<Q, P>) {
    let (req_send, req_recv) = mpsc::sync_channel(0);
    let (rep_send, rep_recv) = mpsc::sync_channel(0);

    let req = Requester {
        req: req_send,
        rep: rep_recv,
    };

    let rep = Replier {
        req: req_recv,
        rep: rep_send,
    };

    (req, rep)
}

pub struct Requester<Q, P> {
    req: mpsc::SyncSender<Q>,
    rep: mpsc::Receiver<P>,
}

impl<Q, P> Requester<Q, P> {
    pub fn req(&self, msg: Q) -> P {
        self.req.send(msg).expect("broken channel");
        self.rep.recv().expect("broken channel")
    }
}

pub struct Replier<Q, P> {
    req: mpsc::Receiver<Q>,
    rep: mpsc::SyncSender<P>,
}

impl<Q, P> Replier<Q, P> {
    pub fn poll(&self) -> Option<Q> {
        match self.req.try_recv() {
            Ok(msg) => Some(msg),
            Err(mpsc::TryRecvError::Empty) => None,
            Err(mpsc::TryRecvError::Disconnected) => panic!("broken channel"),
        }
    }

    pub fn rep(&self, msg: P) {
        self.rep.send(msg).expect("broken channel");
    }
}
