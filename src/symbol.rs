use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::pin::Pin;

// Define some static symbols that the interpreter needs in any case.
// IMPORTANT: When adding a new symbol here, make sure Symbol::new() checks against it.
pub static BEGIN: Symbol = Symbol { name: "begin" };
pub static COND: Symbol = Symbol { name: "cond" };
pub static DEFINE: Symbol = Symbol { name: "define" };
pub static LAMBDA: Symbol = Symbol { name: "lambda" };
pub static IF: Symbol = Symbol { name: "if" };
pub static LET: Symbol = Symbol { name: "let" };
pub static ELSE: Symbol = Symbol { name: "else" };

thread_local! {
    static STATIC_NAMES: RefCell<Vec<Pin<Box<String>>>> = RefCell::new(vec![]);
}

fn static_name<T: AsRef<str> + ToString>(name: T) -> &'static str {
    STATIC_NAMES.with(|symbols| {
        match symbols
            .borrow()
            .iter()
            .map(|entry| -> &str { (**entry).as_ref() })
            .find(|&entry| entry == name.as_ref())
        {
            Some(s) => unsafe {
                // We transmute from &str to &'static str.
                // This should be safe if
                //  1. The string data is never moved in memory (hence the pinned box)
                //  2. The string data is never deallocated. Thus, **never** remove a string from STATIC_NAMES
                return std::mem::transmute(s);
            },
            None => {}
        }

        let entry = Box::pin(name.to_string());
        symbols.borrow_mut().push(entry);

        // Recurse. Now the name should be defined.
        // This is certainly less performant than returning the latest element
        // but we don't need another unsafe block...
        static_name(name)
    })
}

#[derive(Debug, Copy, Clone)]
pub struct Symbol {
    name: &'static str,
}

impl Symbol {
    pub fn new<T: AsRef<str> + ToString>(name: T) -> Self {
        match name.as_ref() {
            n if n == BEGIN.name() => BEGIN,
            n if n == COND.name() => COND,
            n if n == DEFINE.name() => DEFINE,
            n if n == LAMBDA.name() => LAMBDA,
            n if n == IF.name() => IF,
            n if n == LET.name() => LET,
            n if n == ELSE.name() => ELSE,
            _ => Symbol {
                name: static_name(name),
            },
        }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }
}

impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        Symbol::new(s)
    }
}

impl PartialEq<Symbol> for Symbol {
    fn eq(&self, s: &Symbol) -> bool {
        self.name as *const _ == s.name as *const _
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let id = self.name as *const _ as *const u8 as usize;
        id.hash(state);
    }
}
