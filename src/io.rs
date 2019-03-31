use crate::errors::Result;
use rustyline::Editor;
use std::fs::File;
use std::io::{BufRead, BufReader};

pub trait LineReader {
    fn read_line(&mut self) -> Result<String>;
    fn is_eof(&self) -> bool;
}

pub struct ReplInput {
    rl: Editor<()>,
}

impl ReplInput {
    pub fn new() -> Self {
        ReplInput { rl: Editor::new() }
    }
}

impl LineReader for ReplInput {
    fn read_line(&mut self) -> Result<String> {
        // todo: stdout.flush() does not seem to work correctly, and without flushing we don't
        //       see the result of (display ...) commands.
        println!();
        let line = self.rl.readline(">> ")?;
        self.rl.add_history_entry(line.as_str());
        Ok(line + "\n")
    }

    fn is_eof(&self) -> bool {
        false
    }
}

pub struct FileInput {
    file: BufReader<File>,
    eof: bool,
}

impl FileInput {
    pub fn new(filename: &str) -> Result<Self> {
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
