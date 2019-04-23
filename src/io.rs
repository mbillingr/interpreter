use crate::completer::EnvHelper;
use crate::environment::EnvWeak;
use crate::errors::Result;
use rustyline::Editor;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub trait LineReader {
    fn read_line(&mut self) -> Result<String>;
    fn is_eof(&self) -> bool;
    fn set_prompt(&mut self, _prompt: &'static str) {}
}

pub struct ReplInput {
    rl: Editor<EnvHelper>,
    prompt: &'static str,
}

impl ReplInput {
    pub fn new(prompt: &'static str) -> Self {
        ReplInput {
            rl: Editor::new(),
            prompt,
        }
    }

    pub fn set_env(&mut self, env: EnvWeak) {
        self.rl.set_helper(Some(EnvHelper::new(env)));
    }
}

impl LineReader for ReplInput {
    fn read_line(&mut self) -> Result<String> {
        // todo: stdout.flush() does not seem to work correctly, and without flushing we don't
        //       see the result of (display ...) commands.
        //println!();
        //std::io::stdout().flush();
        let line = self.rl.readline(self.prompt)?;
        self.rl.add_history_entry(line.as_str());
        Ok(line + "\n")
    }

    fn is_eof(&self) -> bool {
        false
    }

    fn set_prompt(&mut self, prompt: &'static str) {
        self.prompt = prompt;
    }
}

pub struct FileInput {
    file: BufReader<File>,
    eof: bool,
}

impl FileInput {
    pub fn new(filename: impl AsRef<Path>) -> Result<Self> {
        Ok(FileInput {
            file: BufReader::new(File::open(filename)?),
            eof: false,
        })
    }
}

impl LineReader for FileInput {
    fn read_line(&mut self) -> Result<String> {
        let mut buf = String::new();
        if self.file.read_line(&mut buf)? == 0 {
            self.eof = true;
        }
        Ok(buf)
    }

    fn is_eof(&self) -> bool {
        self.eof
    }
}
