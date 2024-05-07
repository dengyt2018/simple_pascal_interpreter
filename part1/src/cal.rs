#[allow(dead_code, unused, unused_variables)]
pub mod learn_parser {
    use std::fmt::{Display, Formatter};

    use TokenType::{Eof, Integer, Plus};

    #[derive(Debug, Eq, PartialEq)]
    pub enum TokenType {
        Integer,
        Plus,
        Eof,
    }

    pub struct Token {
        token_type: TokenType,
        token_value: String,
    }

    impl Token {
        pub fn new<S: AsRef<str>>(token_type: TokenType, token_value: S) -> Self {
            Self {
                token_type,
                token_value: token_value.as_ref().into(),
            }
        }
    }

    impl Display for TokenType {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Integer => {
                    write!(f, "Integer")
                }
                Eof => {
                    write!(f, "Eof")
                }
                Plus => {
                    write!(f, "+")
                }
            }
        }
    }

    impl Display for Token {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "Token({},{})", self.token_type, self.token_value)
        }
    }

    pub struct Interpreter {
        text: String,
        pos: usize,
        current_token: Token,
    }

    impl Interpreter {
        pub fn new<S: AsRef<str>>(text: S) -> Self {
            Self {
                text: text.as_ref().into(),
                pos: 0,
                current_token: Token::new(Eof, "None"),
            }
        }

        pub fn get_next_token(&mut self) -> Token {
            if self.pos > (self.text.len() - 1) {
                return Token::new(Eof, "None");
            }

            let c_text = self.text.as_bytes()[self.pos] as char;

            if c_text.is_ascii_digit() {
                self.pos += 1;
                return Token::new(Integer, c_text.to_string());
            }

            if c_text == '+' {
                self.pos += 1;
                return Token::new(Plus, "+");
            }

            panic!("Error parsing input");
        }

        fn eat(&mut self, token_type: TokenType) {
            if self.current_token.token_type == token_type {
                self.current_token = self.get_next_token();
            }
        }

        pub fn expr(&mut self) -> i64 {
            self.current_token = self.get_next_token();

            let left = self
                .current_token
                .token_value
                .clone()
                .parse::<i64>()
                .unwrap();
            self.eat(Integer);

            self.eat(Plus);

            let right = self
                .current_token
                .token_value
                .clone()
                .parse::<i64>()
                .unwrap();

            left + right
        }
    }
}

#[cfg(test)]
mod tests {
    use super::learn_parser::*;

    #[test]
    fn test_1() {
        let interpreter = |s| -> i64 { Interpreter::new(s).expr() };

        assert_eq!(10, interpreter("3+7"));
        assert_eq!(7, interpreter("3+4"));
        assert_eq!(12, interpreter("3+9"));
    }
}
