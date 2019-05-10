use crate::debugger::{DebugRequest, Debugger};
use crate::environment::{EnvRef, Environment};
use crate::expression::Expression;
use crate::symbol::Symbol;
use imgui::*;
use imgui_sys;

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
        .build(|| match debugger.current_request() {
            Some(DebugRequest::FunctionCall(func, args, expr)) => {
                ui.text("reduction:");
                ui.text(Expression::cons(func.clone(), args.clone()).short_repr());
                let advance = ui.small_button(im_str!("advance"));
                source_view(ui, &expr);
                if advance {
                    debugger.advance();
                }
            }
            Some(DebugRequest::Predispatch(expr, _)) => {
                ui.text("reduction:");
                ui.text(expr.short_repr());
                let advance = ui.small_button(im_str!("advance"));
                source_view(ui, &expr);
                if advance {
                    debugger.advance();
                }
            }
            Some(DebugRequest::LeaveEval(Err(e))) => {
                ui.text("Call Stack:");
                for (x, _) in debugger.stack() {
                    ui.text(format!("{}", x));
                }
                ui.text_colored([1.0, 0.0, 0.0, 1.0], &ImString::from(format!("{}", e)));
            }
            _ => {}
        });
}

fn source_view(ui: &Ui, expr: &Expression) {
    ui.child_frame(im_str!("source view"), ui.get_content_region_avail())
        .scrollbar_horizontal(true)
        .build(|| match expr.get_source() {
            None => ui.text(format!("<{}>", expr)),
            Some(src) => {
                for line in src.pre_span().split('\n') {
                    ui.text(line);
                }
                ui.same_line(0.0);
                unsafe {
                    imgui_sys::igSetScrollHereY(0.25);
                }
                for line in src.in_span().split('\n') {
                    ui.text_colored([1.0, 0.0, 0.0, 1.0], &ImString::from(line.to_owned()));
                }
                ui.same_line(0.0);
                for line in src.post_span().split('\n') {
                    ui.text(line);
                }
            }
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
