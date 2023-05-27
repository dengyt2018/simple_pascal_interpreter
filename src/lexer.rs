#![allow(non_camel_case_types, dead_code, unused)]
use crate::object::Object;
use crate::set_token;
use crate::token::{ReservedKeywords, Token, TokenType};
use std::str::FromStr;

#[derive(Default)]
pub struct Lexer {
    tokens: Vec<Token>,
    text: Vec<char>,
    pos: usize,
    pub(crate) current_char: char,
    pub(crate) lineno: usize,
    pub(crate) column: usize,
}

impl Lexer {
    pub fn new<S: AsRef<str>>(text: S) -> Self {
        let text = text
            .as_ref()
            .as_bytes()
            .iter()
            .map(|c| *c as char)
            .collect::<Vec<_>>();
        let current_char = text[0];
        Self {
            tokens: vec![],
            text,
            pos: 0,
            current_char,
            lineno: 1,
            column: 1,
        }
    }

    fn skip_comment(&mut self) {
        while self.current_char != '}' {
            self.advance();
        }
        // the closing curly brace
        self.advance();
    }

    /**
       Advance the `pos` pointer and set the `current_char` variable.
    */
    fn advance(&mut self) {
        if self.current_char == '\n' {
            self.lineno += 1;
            self.column = 0;
        }

        self.pos += 1;

        if self.pos > (self.text.len() - 1) {
            self.current_char = '\0'; // Indicates end of input
        } else {
            self.current_char = self.text[self.pos];
            self.column += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.current_char == '\0'
    }

    fn peek(&self) -> char {
        let peek_pos = &self.pos + 1;
        if peek_pos > self.text.len() - 1 {
            '\0'
        } else {
            self.text[peek_pos]
        }
    }

    fn integer(&mut self, result: &mut Vec<char>) {
        while !self.is_at_end() && self.current_char.is_ascii_digit() {
            result.push(self.current_char);
            self.advance();
        }
    }

    /**
       Return a (multidigit) integer or float consumed from the input.
    */
    fn number(&mut self) -> Token {
        let mut result = vec![];
        self.integer(&mut result);

        if self.current_char == '.' {
            result.push(self.current_char);
            self.advance();

            self.integer(&mut result);

            if let Ok(r) = result.iter().collect::<String>().parse::<f64>() {
                set_token!(
                    TokenType::RealConst,
                    result.iter().collect::<String>(),
                    self
                )
                .set_object(Some(Object::RealConst(r)))
            } else {
                panic!("")
            }
        } else if let Ok(r) = result.iter().collect::<String>().parse::<u64>() {
            set_token!(
                TokenType::IntegerConst,
                result.iter().collect::<String>(),
                self
            )
            .set_object(Some(Object::IntegerConst(r)))
        } else {
            panic!("")
        }
    }

    /**
       Handle identifiers and reserved keywords
    */
    fn _id(&mut self) -> Token {
        let mut buffer = Vec::new();
        while !self.current_char.is_whitespace() && self.current_char.is_alphanumeric() {
            buffer.push(self.current_char);
            self.advance();
        }
        let token_name = buffer.iter().collect::<String>();

        if let Ok(key) = ReservedKeywords::from_str(&token_name) {
            match key {
                ReservedKeywords::Procedure => set_token!(TokenType::Procedure, token_name, self),
                ReservedKeywords::Program => set_token!(TokenType::Program, token_name, self),
                ReservedKeywords::Integer => set_token!(TokenType::Integer, token_name, self),
                ReservedKeywords::String => set_token!(TokenType::String, token_name, self),
                ReservedKeywords::Begin => set_token!(TokenType::Begin, token_name, self),
                ReservedKeywords::Real => set_token!(TokenType::Real, token_name, self),
                ReservedKeywords::End => set_token!(TokenType::End, token_name, self),
                ReservedKeywords::Var => set_token!(TokenType::Var, token_name, self),
                ReservedKeywords::Div => set_token!(TokenType::IntegerDiv, token_name, self),
            }
        } else {
            set_token!(TokenType::Id, token_name, self)
        }
    }

    fn get_string(&mut self) -> Token {
        let mut string = vec![];

        // the first ".
        self.advance();

        loop {
            string.push(self.current_char);
            self.advance();

            if self.current_char == '"' || self.is_at_end() {
                break;
            }
        }

        if self.current_char == '\0' {
            panic!("Unterminated string. line: {}", self.lineno);
        }

        // the closing ".
        self.advance();

        let str = string.iter().collect::<String>();

        set_token!(TokenType::String, "STRING", self).set_object(Some(Object::Identifier(str)))
    }

    /**
       Lexical analyzer (also known as scanner or tokenizer)

       This method is responsible for breaking a sentence
       apart into tokens. One token at a time.
    */
    pub fn get_next_token(&mut self) -> Token {
        while !self.is_at_end() {
            match self.current_char {
                '*' => {
                    self.advance();
                    return set_token!(TokenType::Mul, "*", self);
                }
                '/' => {
                    self.advance();
                    return set_token!(TokenType::FloatDiv, "/", self);
                }
                '%' => {
                    self.advance();
                    return set_token!(TokenType::Modulo, "%", self);
                }
                '+' => {
                    self.advance();
                    return set_token!(TokenType::Plus, "+", self);
                }
                '-' => {
                    self.advance();
                    return set_token!(TokenType::Minus, "-", self);
                }
                '(' => {
                    self.advance();
                    return set_token!(TokenType::Lbrack, "(", self);
                }
                ')' => {
                    self.advance();
                    return set_token!(TokenType::Rbrack, ")", self);
                }
                ';' => {
                    self.advance();
                    return set_token!(TokenType::Semi, ";", self);
                }
                '.' => {
                    self.advance();
                    return set_token!(TokenType::Dot, ".", self);
                }
                ',' => {
                    self.advance();
                    return set_token!(TokenType::Comma, ",", self);
                }
                '{' => {
                    self.advance();
                    self.skip_comment();
                    continue;
                }
                ':' if self.peek() == '=' => {
                    self.advance();
                    self.advance();
                    return set_token!(TokenType::Assign, ":=", self);
                }
                ':' => {
                    self.advance();
                    return set_token!(TokenType::Colon, ":", self);
                }
                '"' => {
                    return self.get_string();
                }
                _ => {
                    if self.current_char.is_whitespace() {
                        self.advance();
                        continue;
                    }
                    if self.current_char.is_ascii_digit() {
                        return self.number();
                    }
                    if self.current_char.is_alphabetic() {
                        return self._id();
                    }
                }
            }
        }
        set_token!(TokenType::Eof, "EOF", self)
    }

    pub fn get_tokens(&mut self) -> Vec<Token> {
        self._get_tokens();

        self.tokens.clone()
    }

    fn _get_tokens(&mut self) {
        loop {
            let token = self.get_next_token();
            match token.token_type() {
                TokenType::Eof => {
                    self.tokens.push(token);
                    break;
                }
                _ => self.tokens.push(token),
            }
        }
    }
}

#[test]
fn test_lexer() {
    use crate::token::TokenType::*;
    let str = r#"PROGRAM Part12;
VAR
    a, c : INTEGER;

PROCEDURE P1;
VAR
    a, c : REAL;
    k: INTEGER;
    PROCEDURE P2(a : INTEGER);
        VAR
            a, z : INTEGER;
        BEGIN {P2}
            z := 777;
        END; {P2}

BEGIN {P1}

END; {P1}
BEGIN {Part12}
    a := 10;
    a := "string abc";
    a := 10.52;
END. {Part12}"#
        .to_string();

    let mut lexer = Lexer::new(str);

    let tokens = vec![
        set_token!(TokenType::Program, "PROGRAM", 1, 8),
        set_token!(TokenType::Id, "Part12", 1, 15),
        set_token!(TokenType::Semi, ";", 1, 16),
        set_token!(TokenType::Var, "VAR", 2, 4),
        set_token!(TokenType::Id, "a", 3, 6),
        set_token!(TokenType::Comma, ",", 3, 7),
        set_token!(TokenType::Id, "c", 3, 9),
        set_token!(TokenType::Colon, ":", 3, 11),
        set_token!(TokenType::Integer, "INTEGER", 3, 19),
        set_token!(TokenType::Semi, ";", 3, 20),
        set_token!(TokenType::Procedure, "PROCEDURE", 5, 10),
        set_token!(TokenType::Id, "P1", 5, 13),
        set_token!(TokenType::Semi, ";", 5, 14),
        set_token!(TokenType::Var, "VAR", 6, 4),
        set_token!(TokenType::Id, "a", 7, 6),
        set_token!(TokenType::Comma, ",", 7, 7),
        set_token!(TokenType::Id, "c", 7, 9),
        set_token!(TokenType::Colon, ":", 7, 11),
        set_token!(TokenType::Real, "REAL", 7, 16),
        set_token!(TokenType::Semi, ";", 7, 17),
        set_token!(TokenType::Id, "k", 8, 6),
        set_token!(TokenType::Colon, ":", 8, 7),
        set_token!(TokenType::Integer, "INTEGER", 8, 15),
        set_token!(TokenType::Semi, ";", 8, 16),
        set_token!(TokenType::Procedure, "PROCEDURE", 9, 14),
        set_token!(TokenType::Id, "P2", 9, 17),
        set_token!(TokenType::Lbrack, "(", 9, 18),
        set_token!(TokenType::Id, "a", 9, 19),
        set_token!(TokenType::Colon, ":", 9, 21),
        set_token!(TokenType::Integer, "INTEGER", 9, 29),
        set_token!(TokenType::Rbrack, ")", 9, 30),
        set_token!(TokenType::Semi, ";", 9, 31),
        set_token!(TokenType::Var, "VAR", 10, 12),
        set_token!(TokenType::Id, "a", 11, 14),
        set_token!(TokenType::Comma, ",", 11, 15),
        set_token!(TokenType::Id, "z", 11, 17),
        set_token!(TokenType::Colon, ":", 11, 19),
        set_token!(TokenType::Integer, "INTEGER", 11, 27),
        set_token!(TokenType::Semi, ";", 11, 28),
        set_token!(TokenType::Begin, "BEGIN", 12, 14),
        set_token!(TokenType::Id, "z", 13, 14),
        set_token!(TokenType::Assign, ":=", 13, 17),
        set_token!(TokenType::IntegerConst, "777", 13, 21)
            .set_object(Some(Object::IntegerConst(777))),
        set_token!(TokenType::Semi, ";", 13, 22),
        set_token!(TokenType::End, "END", 14, 12),
        set_token!(TokenType::Semi, ";", 14, 13),
        set_token!(TokenType::Begin, "BEGIN", 16, 6),
        set_token!(TokenType::End, "END", 18, 4),
        set_token!(TokenType::Semi, ";", 18, 5),
        set_token!(TokenType::Begin, "BEGIN", 19, 6),
        set_token!(TokenType::Id, "a", 20, 6),
        set_token!(TokenType::Assign, ":=", 20, 9),
        set_token!(TokenType::IntegerConst, "10", 20, 12)
            .set_object(Some(Object::IntegerConst(10))),
        set_token!(TokenType::Semi, ";", 20, 13),
        set_token!(TokenType::Id, "a", 21, 6),
        set_token!(TokenType::Assign, ":=", 21, 9),
        set_token!(TokenType::String, "STRING", 21, 22)
            .set_object(Some(Object::Identifier("string abc".to_string()))),
        set_token!(TokenType::Semi, ";", 21, 23),
        set_token!(TokenType::Id, "a", 22, 6),
        set_token!(TokenType::Assign, ":=", 22, 9),
        set_token!(TokenType::RealConst, "10.52", 22, 15)
            .set_object(Some(Object::RealConst(10.52))),
        set_token!(TokenType::Semi, ";", 22, 16),
        set_token!(TokenType::End, "END", 23, 4),
        set_token!(TokenType::Dot, ".", 23, 5),
        set_token!(TokenType::Eof, "EOF", 23, 13),
    ];

    lexer
        .get_tokens()
        .iter()
        .zip(tokens)
        .for_each(|(x, y)| assert_eq!(*x, y));
}
