use crate::debugger::{DebugRequest, Debugger};
use crate::environment::{EnvRef, Environment};
use crate::symbol::Symbol;
use imgui::*;

mod support_gfx;

const CLEAR_COLOR: [f32; 4] = [0.6, 0.7, 0.8, 1.0];

pub fn run(mut debugger: Debugger) {
    support_gfx::run(
        "Scheme Debugger".to_owned(),
        Some("debug_imgui.ini"),
        CLEAR_COLOR,
        |ui| {
            debugger.poll();

            hello_world(ui);
            ui.show_demo_window(&mut true);
            if let Some(env) = debugger.current_env() {
                env_window(ui, env);
            }
            dbg_window(ui, &mut debugger);
            true
        },
    );
}

fn hello_world<'a>(ui: &Ui<'a>) -> bool {
    ui.window(im_str!("Hello world"))
        .size((300.0, 100.0), ImGuiCond::FirstUseEver)
        .build(|| {
            ui.text(im_str!("Hello world!"));
            ui.text(im_str!("こんにちは世界！"));
            ui.text(im_str!("λλλλλλλλλλ"));
            ui.text(im_str!("This...is...imgui-rs!"));
            ui.separator();
            let mouse_pos = ui.imgui().mouse_pos();
            ui.text(im_str!(
                "Mouse Position: ({:.1},{:.1})",
                mouse_pos.0,
                mouse_pos.1
            ));
        });

    true
}

fn dbg_window(ui: &Ui, debugger: &mut Debugger) {
    ui.window(im_str!("Current Expression"))
        .size((300.0, 100.0), ImGuiCond::FirstUseEver)
        .build(|| {
            match debugger.current_request() {
                Some(DebugRequest::FunctionCall(func, args)) => {
                    ui.text(format!("gonna call {} with {} ...", func, args));
                    if ui.small_button(im_str!("advance")) {
                        debugger.advance();
                    }
                }
                _ => {}
            }
            /*match debugger.current_expr() {
                Ok(expr) => ui.text(expr.short_repr()),
                Err(e) => ui.text(format!("{}", e)),
            }
            if ui.small_button(im_str!("eval")) {
                debugger.eval();
            }
            for (x, y) in debugger.history().iter().rev() {
                ui.text(x.short_repr());
                match y {
                    Ok(expr) => ui.text(format!(" => {}", expr.short_repr())),
                    Err(e) => ui.text(format!(" => {}", e)),
                }
            }*/
        });
}

fn env_window(ui: &Ui, env: &EnvRef) {
    ui.window(im_str!("Current Environment"))
        .size((300.0, 100.0), ImGuiCond::FirstUseEver)
        .build(|| {
            env_header(ui, &*env.borrow());
        });
}

fn env_header(ui: &Ui, env: &Environment) {
    let env_name: ImString = env.name().into();
    if let Some(parent) = env.parent() {
        env_header(ui, &*parent.borrow());
    }
    if ui.collapsing_header(&env_name).build() {
        ui.text(im_str!("Items"));
        let mut entries: Vec<String> = env
            .items()
            .map(|(key, value)| {
                format!("{}: {}", key, value.short_repr())
                    .replace('λ', "lambda")
                    .into()
            })
            .collect();
        entries.sort();
        for entry in entries.into_iter().map(|e| -> ImString { e.into() }) {
            ui.bullet_text(&entry);
        }
    }
}

impl From<Symbol> for ImString {
    fn from(symbol: Symbol) -> Self {
        ImString::new(symbol.name())
    }
}
