use std::fmt;

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedCharacter,
    InvalidNumber,
    UnexpectedToken,
    DuplicateId,
    IdNotFound,
    WrongParamsNum,
    WrongSymbolType,
}

#[derive(Debug)]
pub enum ErrorCategory {
    Lexer,
    Parser,
    Semantic,
}

pub struct Error {
    message: String,
    kind: ErrorKind,
    line: Option<i32>,
    col: Option<i32>,
    current_string: String,
    category: ErrorCategory,
}

impl Error {
    pub fn new(
        message: String,
        kind: ErrorKind,
        line: Option<i32>,
        col: Option<i32>,
        current_string: String,
        category: ErrorCategory,
    ) -> Self {
        Error {
            message,
            kind,
            line,
            col,
            current_string,
            category,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let line = match self.line {
            Some(matched_line) => matched_line.to_string(),
            None => "No line".to_string(),
        };
        let col = match self.col {
            Some(matched_col) => matched_col.to_string(),
            None => "No Col".to_string(),
        };
        write!(
            f,
            "{:?} error on '{}' line: {} column: {} kind: {:?} message: {}",
            self.category, self.current_string, line, col, self.kind, self.message
        )
    }
}
