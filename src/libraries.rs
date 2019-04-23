use crate::environment::{default_env, EnvRef, Environment};
use crate::errors::*;
use crate::expression::Expression;
use crate::interpreter::eval;
use crate::parser::parse_file;
use crate::symbol::{self, Symbol};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;

thread_local! {
    static LIBRARIES: RefCell<HashMap<Vec<Symbol>, Library>> = {
        let mut map = HashMap::new();
        map.insert(vec!["builtin".into(), "core".into()], Library{private_env: Environment::new(None).into(), export_env: default_env().into()});
        RefCell::new(map)
    };
}

#[derive(Clone)]
pub struct Library {
    private_env: EnvRef,
    export_env: EnvRef,
}

impl Library {
    fn import(&self, target_env: &EnvRef) {
        let mut te = target_env.borrow_mut();
        for (&k, entry) in self.export_env.borrow().items() {
            te.insert_entry(k, entry.clone());
        }
    }
}

/// Load library from file -- cached.
///
/// This brain-dead implementation simply evals the file, then checks the cache again.
fn get_library(name: &[Symbol]) -> Result<Library> {
    if let Some(lib) = LIBRARIES.with(|libs| libs.borrow().get(name).cloned()) {
        return Ok(lib);
    }

    let lib_expr = parse_file(resolve_lib(name))?;
    assert_eq!(Expression::Nil, *lib_expr.cdr()?);
    eval(lib_expr.car()?, Environment::new(None).into())?;

    LIBRARIES
        .with(|libs| libs.borrow().get(name).cloned())
        .ok_or_else(|| ErrorKind::GenericError(format!("Unknown library: {:?}", name)).into())
}

pub fn store_library(name: &Expression, lib: Library) -> Result<()> {
    LIBRARIES.with(|libs| {
        libs.borrow_mut().insert(libname(name)?, lib);
        Ok(())
    })
}

pub fn import_library(import_sets: &Expression, env: &EnvRef) -> Result<()> {
    for imp in import_sets.iter_list() {
        let imp = imp?;
        match imp.car()? {
            _ => get_library(&libname(imp)?)?.import(env),
        }
    }
    Ok(())
}

pub fn define_library(declarations: &Expression) -> Result<Library> {
    let private_env: EnvRef = Environment::new(None).into();

    let mut exports = HashMap::new();

    for decl in declarations.iter_list() {
        let decl = decl?;
        match decl.car()? {
            Expression::Symbol(s) if *s == symbol::EXPORT => {
                for export in decl.cdr()?.iter_list() {
                    let export = export?;
                    if !export.is_pair() {
                        let name = *export.try_as_symbol()?;
                        exports.insert(name, name);
                    } else {
                        let keyword = *export.car()?.try_as_symbol()?;
                        if keyword != symbol::RENAME {
                            Err(ErrorKind::GenericError(
                                "Expected symbol \"rename\".".to_string(),
                            ))?
                        }
                        let original = *export.cdr()?.car()?.try_as_symbol()?;
                        let export_name = *export.cdr()?.cdr()?.car()?.try_as_symbol()?;
                        exports.insert(original, export_name);
                    }
                }
            }
            Expression::Symbol(s)
                if *s == symbol::IMPORT || *s == symbol::BEGIN || *s == symbol::INCLUDE =>
            {
                eval(decl, private_env.clone())?;
            }
            x => Err(ErrorKind::GenericError(format!(
                "Invalid library declaration: {}",
                x
            )))?,
        }
    }

    let export_env = private_env.borrow().export(&exports).into();
    Ok(Library {
        private_env,
        export_env,
    })
}

fn libname(expr: &Expression) -> Result<Vec<Symbol>> {
    expr.iter_list()
        .map(|x| x.and_then(Expression::try_as_symbol).map(|s| *s))
        .collect()
}

/// convert library name (list of strings) into a path
fn resolve_lib(name: &[Symbol]) -> PathBuf {
    let mut path: PathBuf = name.iter().map(Symbol::name).collect();
    path.set_extension("scm");
    path
}
