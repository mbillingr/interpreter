#[cfg(feature = "debugging")]
use crate::debugger::{default_thread_debugger, ThreadDebugger};

thread_local! {
    static STATE: ThreadState = ThreadState::new();
}

pub struct ThreadState {
    #[cfg(feature = "debugging")]
    debugger: ThreadDebugger,
}

impl ThreadState {
    fn new() -> Self {
        ThreadState {
            #[cfg(feature = "debugging")]
            debugger: default_thread_debugger(),
        }
    }

    #[cfg(feature = "debugging")]
    pub fn with_debugger(f: impl FnOnce(&ThreadDebugger)) {
        STATE.with(|state| f(&state.debugger))
    }
}
