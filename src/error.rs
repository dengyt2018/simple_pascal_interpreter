#![allow(non_camel_case_types, dead_code, unused)]
use crate::token::Token;

#[rustfmt::skip]
#[derive(Debug)]
pub enum PascalResult {
    ParseError { token: Token, message: String },
    SystemError { token: Token, message: String },
    RuntimeError { token: Token, message: String },
    Error { lineno: usize, column: usize, message: String },
    Fail,
}

impl PascalResult {
    pub fn fail() -> Self {
        Self::Fail
    }

    pub fn error<S: AsRef<str>>(token: &Token, message: S) -> Self {
        let err = Self::Error {
            lineno: token.lineno,
            column: token.column,
            message: message.as_ref().into(),
        };
        err.report("");
        err
    }

    pub fn parse_error<S: AsRef<str>>(token: &Token, message: S) -> Self {
        let err = Self::ParseError {
            token: token.clone(),
            message: message.as_ref().into(),
        };
        err.report("");
        err
    }

    pub fn runtime_error<S: AsRef<str>>(token: &Token, message: S) -> Self {
        let err = Self::RuntimeError {
            token: token.clone(),
            message: message.as_ref().into(),
        };
        err.report("");
        err
    }

    pub fn system_error<S: AsRef<str>>(token: &Token, message: S) -> Self {
        let err = Self::SystemError {
            token: token.clone(),
            message: message.as_ref().into(),
        };
        err.report("");
        err
    }

    fn report<S: AsRef<str>>(&self, loc: S) {
        match self {
            PascalResult::ParseError { token, message } => {
                log::error!(
                    "[line: {}, column: {}] Error at '{}': {}",
                    token.lineno,
                    token.column,
                    token.token_value,
                    message
                )
            }
            PascalResult::RuntimeError { token, message } => {
                log::error!(
                    "[line: {}, column: {}] Error at '{}': {}",
                    token.lineno,
                    token.column,
                    token.token_value,
                    message
                )
            }
            PascalResult::Error {
                lineno,
                column,
                message,
            } => {
                log::error!("[line: {}, column: {}] : {}", lineno, column, message)
            }

            PascalResult::SystemError { token, message } => {
                log::error!(
                    "[line: {}, column: {}] Error at '{}': {}",
                    token.lineno,
                    token.column,
                    token.token_value,
                    message
                )
            }
            PascalResult::Fail => {
                unreachable!();
            }
        }
    }
}

pub mod init_log {
    use env_logger::Builder;
    use log::LevelFilter;
    use std::io::Write;

    pub fn init_log() {
        Builder::new()
            .format(|buf, record| writeln!(buf, "{}", record.args()))
            .filter(None, LevelFilter::Info)
            .init();
    }
}
