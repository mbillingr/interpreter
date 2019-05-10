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

pub enum DebugFrame {
    Frame(Expression, EnvRef),
    TailFrame(Expression, EnvRef),
}

impl DebugFrame {
    pub fn expr(&self) -> &Expression {
        match self {
            DebugFrame::Frame(x, _) => x,
            DebugFrame::TailFrame(x, _) => x,
        }
    }

    pub fn env(&self) -> &EnvRef {
        match self {
            DebugFrame::Frame(_, e) => e,
            DebugFrame::TailFrame(_, e) => e,
        }
    }

    pub fn is_tail(&self) -> bool {
        match self {
            DebugFrame::TailFrame(_, _) => true,
            _ => false,
        }
    }
}

pub struct Debugger {
    service: Replier<DebugRequest, ()>,
    current_request: Option<DebugRequest>,

    track_tailcalls: bool,

    stack: Vec<DebugFrame>,
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
            track_tailcalls: true,
            stack: vec![],
            //history: vec![],
        }
    }

    pub fn poll(&mut self) {
        if self.current_request.is_none() {
            self.current_request = self.service.poll();
            match &self.current_request {
                Some(DebugRequest::EnterEval(_, _)) => self.advance(),
                Some(DebugRequest::LeaveEval(e)) => {
                    if e.is_ok() {
                        self.advance()
                    }
                }
                Some(DebugRequest::Predispatch(expr, _)) => {
                    if !expr.is_pair() {
                        self.advance()
                    }
                }
                Some(DebugRequest::FunctionCall(_, _, _)) => {}
                None => {}
            }
        }
    }

    pub fn advance(&mut self) {
        match self.current_request.take() {
            None => return,
            Some(DebugRequest::EnterEval(expr, env)) => {
                self.stack.push(DebugFrame::Frame(expr, env))
            }
            Some(DebugRequest::LeaveEval(_)) => {
                if self.track_tailcalls {
                    while let Some(DebugFrame::TailFrame(_, _)) = self.stack.pop() {}
                } else {
                    self.stack.pop().expect("stack underflow");
                }
            }
            Some(DebugRequest::Predispatch(expr, env)) => {
                if !self.track_tailcalls {
                    self.stack.pop().expect("stack underflow");
                }
                self.stack.push(DebugFrame::TailFrame(expr, env));
            }
            Some(DebugRequest::FunctionCall(_, _, _)) => {}
        }
        self.service.rep(());
    }

    pub fn stack(&self) -> &[DebugFrame] {
        &self.stack
    }

    pub fn current_request(&self) -> Option<&DebugRequest> {
        self.current_request.as_ref()
    }

    pub fn current_env(&self) -> Option<&EnvRef> {
        self.stack.last().map(DebugFrame::env)
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
