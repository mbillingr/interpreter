use crate::debugger::{default_thread_debugger, ThreadDebugger};

thread_local! {
    static STATE: ThreadState = ThreadState::new();
}

pub struct ThreadState {
    debugger: ThreadDebugger,
}

impl ThreadState {
    fn new() -> Self {
        ThreadState {
            debugger: default_thread_debugger(),
        }
    }

    pub fn with_debugger(f: impl FnOnce(&ThreadDebugger)) {
        STATE.with(|state| f(&state.debugger))
    }
}
