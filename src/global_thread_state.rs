use crate::debugger::{default_thread_debugger, ThreadDebugger};
use std::thread::ThreadId;
use std::path::{PathBuf, Path};
use std::cell::{RefCell};

thread_local! {
    static STATE: ThreadState = ThreadState::new();
}

pub struct ThreadState {
    debugger: ThreadDebugger,
    current_file: RefCell<Option<PathBuf>>,
}

impl ThreadState {
    fn new() -> Self {
        ThreadState {
            debugger: default_thread_debugger(),
            current_file: RefCell::new(None),
        }
    }

    pub fn with_debugger(f: impl FnOnce(&ThreadDebugger)) {
        STATE.with(|state| f(&state.debugger))
    }

    pub fn current_file() -> Option<PathBuf> {
        STATE.with(|state| state.current_file.borrow().clone())
    }

    pub fn set_current_file(path: Option<PathBuf>) {
        STATE.with(|state| *state.current_file.borrow_mut() = path)
    }
}
