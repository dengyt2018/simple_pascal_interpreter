#![allow(non_camel_case_types, dead_code, unused)]
use crate::lexer::Lexer;
use crate::object::Object;
use std::str::FromStr;

#[rustfmt::skip]
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenType {
    Integer, Real, IntegerConst, RealConst, String,
    Procedure, Id, Program, Var,
    Plus, Minus, Mul, IntegerDiv, FloatDiv, Modulo,
    Lbrack, Rbrack,
    Assign, Begin, End,
    Semi, Dot, Colon, Comma,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) token_value: String,
    pub(crate) literal: Option<Object>,
    pub(crate) lineno: usize,
    pub(crate) column: usize,
}

impl Token {
    pub fn new<S: AsRef<str>>(token_type: TokenType, token_value: S) -> Self {
        Self {
            token_type,
            token_value: token_value.as_ref().into(),
            literal: None,
            lineno: 1,
            column: 1,
        }
    }

    #[inline]
    pub fn token_type(&self) -> TokenType {
        self.token_type
    }

    #[inline]
    pub fn get_value(&self) -> String {
        self.token_value.clone()
    }

    #[inline]
    pub fn set_pos(self, lexer: &Lexer) -> Self {
        self.pos(lexer.lineno, lexer.column)
    }

    #[inline]
    pub fn pos(mut self, lineno: usize, column: usize) -> Self {
        self.lineno = lineno;
        self.column = column;
        self
    }

    #[inline]
    pub fn set_object(mut self, object: Option<Object>) -> Self {
        if let Some(ob) = object {
            self.literal = Some(ob);
        }
        self
    }

    #[inline]
    pub fn set_token<S: AsRef<str>>(mut self, token_type: TokenType, token_value: S) -> Self {
        self.token_type = token_type;
        self.token_value = token_value.as_ref().into();
        self
    }

    #[inline]
    pub(crate) fn error(&self, code: ErrorCode) -> String {
        match code {
            ErrorCode::UnexpectedToken => "Unexpected token".to_string(),
            ErrorCode::DuplicateId => "Duplicate id found".to_string(),
            ErrorCode::IdNotFound => "Symbol(identifier) not found".to_string(),
            ErrorCode::WrongParamsNum => "Wrong number of arguments".to_string(),
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            token_type: TokenType::Eof,
            token_value: "None".to_string(),
            literal: None,
            lineno: 1,
            column: 1,
        }
    }
}

pub enum ErrorCode {
    UnexpectedToken,
    IdNotFound,
    DuplicateId,
    WrongParamsNum,
}

#[derive(Debug, PartialEq)]
pub enum ReservedKeywords {
    Procedure,
    Program,
    Integer,
    String,
    Begin,
    Real,
    End,
    Var,
    Div,
}

impl FromStr for ReservedKeywords {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "PROCEDURE" => Ok(ReservedKeywords::Procedure),
            "PROGRAM" => Ok(ReservedKeywords::Program),
            "INTEGER" => Ok(ReservedKeywords::Integer),
            "STRING" => Ok(ReservedKeywords::String),
            "BEGIN" => Ok(ReservedKeywords::Begin),
            "REAL" => Ok(ReservedKeywords::Real),
            "END" => Ok(ReservedKeywords::End),
            "VAR" => Ok(ReservedKeywords::Var),
            "DIV" => Ok(ReservedKeywords::Div),
            _ => Err(format!("Unsupported Reserved Keywords. {:#?}", s)),
        }
    }
}

impl From<TokenType> for ReservedKeywords {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Procedure => ReservedKeywords::Procedure,
            TokenType::Program => ReservedKeywords::Program,
            TokenType::Integer => ReservedKeywords::Integer,
            TokenType::IntegerDiv => ReservedKeywords::Div,
            TokenType::String => ReservedKeywords::String,
            TokenType::Begin => ReservedKeywords::Begin,
            TokenType::Real => ReservedKeywords::Real,
            TokenType::End => ReservedKeywords::End,
            TokenType::Var => ReservedKeywords::Var,
            _ => {
                panic!("Unsupported Reserved Keywords. {:#?}", value)
            }
        }
    }
}

#[macro_export]
macro_rules! set_token {
    ($token_type:expr, $token_value: expr) => {
        Token::new($token_type, $token_value)
    };
    ($token_type:expr, $token_value: expr, $lexer: expr) => {
        set_token!($token_type, $token_value).set_pos($lexer)
    };
    ($token_type:expr, $token_value: expr, $lineno: expr, $column: expr) => {
        set_token!($token_type, $token_value).pos($lineno, $column)
    };
}

#[test]
fn test_macro() {
    let t1 = set_token!(TokenType::Var, "var").pos(15, 12);
    let t2 = set_token!(TokenType::Var, "var", 15, 12);
    let mut lexer = Lexer::new("var");
    lexer.lineno = 15;
    lexer.column = 12;
    let t3 = set_token!(TokenType::Var, "var", &lexer);

    assert_eq!(t1, t2);
    assert_eq!(t2, t3);
}
