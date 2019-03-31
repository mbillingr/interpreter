use crate::errors::Result;
use rustyline::Editor;

pub trait LineReader {
    fn read_line(&mut self) -> Result<String>;
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
        let line = self.rl.readline(">> ")?;
        self.rl.add_history_entry(line.as_str());
        Ok(line + "\n")
    }
}

/*pub struct ReplInput {
    stdin: BufReader<io::Stdin>,
}

impl ReplInput {
    pub fn new() -> Self {
        ReplInput {
            stdin: BufReader::new(io::stdin()),
        }
    }
}

impl LineReader for ReplInput {
    fn read_line(&mut self) -> Result<String> {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut buf = String::new();
        self.stdin.read_line(&mut buf)?;
        Ok(buf)
    }
}*/
