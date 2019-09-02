use lazy_static::lazy_static;
use std::hash::{Hash, Hasher};
use std::pin::Pin;
use std::sync::Mutex;

// Define some static symbols that the interpreter needs in any case.
// IMPORTANT: When adding a new symbol here, make sure Symbol::new() checks against it.
pub static AND: Symbol = Symbol { name: "and" };
pub static APPLY: Symbol = Symbol { name: "apply" };
pub static BEGIN: Symbol = Symbol { name: "begin" };
pub static CASE: Symbol = Symbol { name: "case" };
pub static COND: Symbol = Symbol { name: "cond" };
pub static COND_APPLY: Symbol = Symbol { name: "=>" };
pub static DEFINE: Symbol = Symbol { name: "define" };
pub static DEFINE_LIBRARY: Symbol = Symbol {
    name: "define-library",
};
pub static DEFINE_SYNTAX: Symbol = Symbol {
    name: "define-syntax",
};
pub static DOT: Symbol = Symbol { name: "." };
pub static ELLIPSIS: Symbol = Symbol { name: "..." };
pub static ELSE: Symbol = Symbol { name: "else" };
pub static EVAL: Symbol = Symbol { name: "eval" };
pub static IS_EQV: Symbol = Symbol { name: "eqv?" };
pub static EXPORT: Symbol = Symbol { name: "export" };
pub static IF: Symbol = Symbol { name: "if" };
pub static IMPORT: Symbol = Symbol { name: "import" };
pub static INCLUDE: Symbol = Symbol { name: "include" };
pub static LAMBDA: Symbol = Symbol { name: "lambda" };
pub static LET: Symbol = Symbol { name: "let" };
pub static OR: Symbol = Symbol { name: "or" };
pub static QUASIQUOTE: Symbol = Symbol { name: "quasiquote" };
pub static QUOTE: Symbol = Symbol { name: "quote" };
pub static RENAME: Symbol = Symbol { name: "rename" };
pub static SETVAR: Symbol = Symbol { name: "set!" };
pub static SYNTAX_RULES: Symbol = Symbol {
    name: "syntax-rules",
};
pub static UNDERSCORE: Symbol = Symbol { name: "_" };
pub static UNQUOTE: Symbol = Symbol { name: "unquote" };

pub static GREEK_LAMBDA: Symbol = Symbol { name: "\u{03BB}" };

lazy_static! {
    static ref STATIC_NAMES: Mutex<Vec<Pin<Box<String>>>> = Mutex::new(vec![]);
}

fn static_name<T: AsRef<str> + ToString>(name: T) -> &'static str {
    let mut container = STATIC_NAMES.lock().unwrap();
    let s = match container
        .iter()
        .map(|entry| entry.as_str())
        .find(|&entry| entry == name.as_ref())
    {
        Some(s) => s,
        None => {
            let entry = Box::pin(name.to_string());
            container.push(entry);
            container.last().unwrap()
        }
    };

    unsafe {
        // We transmute from &str to &'static str.
        // This should be safe if
        //  1. The string data is never moved in memory (hence the pinned box)
        //  2. The string data is never deallocated. Thus, **never** remove a string from STATIC_NAMES
        std::mem::transmute(s)
    }
}

#[derive(Copy, Clone)]
pub struct Symbol {
    name: &'static str,
}

impl Symbol {
    pub fn new<T: AsRef<str> + ToString>(name: T) -> Self {
        // Clippy complains about all the branching in this function, but this
        // is what we actually want here, I guess...
        #![allow(clippy::cognitive_complexity)]
        match name.as_ref() {
            n if n == AND.name() => AND,
            n if n == APPLY.name() => APPLY,
            n if n == BEGIN.name() => BEGIN,
            n if n == CASE.name() => CASE,
            n if n == COND.name() => COND,
            n if n == COND_APPLY.name() => COND_APPLY,
            n if n == DEFINE.name() => DEFINE,
            n if n == DEFINE_LIBRARY.name() => DEFINE_LIBRARY,
            n if n == DEFINE_SYNTAX.name() => DEFINE_SYNTAX,
            n if n == DOT.name() => DOT,
            n if n == ELLIPSIS.name() => ELLIPSIS,
            n if n == ELSE.name() => ELSE,
            n if n == EVAL.name() => EVAL,
            n if n == IS_EQV.name() => IS_EQV,
            n if n == EXPORT.name() => EXPORT,
            n if n == IF.name() => IF,
            n if n == IMPORT.name() => IMPORT,
            n if n == INCLUDE.name() => INCLUDE,
            n if n == LAMBDA.name() => LAMBDA,
            n if n == LET.name() => LET,
            n if n == OR.name() => OR,
            n if n == QUASIQUOTE.name() => QUASIQUOTE,
            n if n == QUOTE.name() => QUOTE,
            n if n == RENAME.name() => RENAME,
            n if n == SETVAR.name() => SETVAR,
            n if n == SYNTAX_RULES.name() => SYNTAX_RULES,
            n if n == UNDERSCORE.name() => UNDERSCORE,
            n if n == UNQUOTE.name() => UNQUOTE,
            n if n == GREEK_LAMBDA.name() => GREEK_LAMBDA,
            _ => Symbol {
                name: static_name(name),
            },
        }
    }

    pub fn new_uninterned<T: AsRef<str> + ToString>(name: T) -> Self {
        let string_data = Box::new(name.to_string());
        Symbol {
            name: Box::leak(string_data),
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

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.name
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

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::cmp::PartialOrd for Symbol {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(rhs.name)
    }
}
