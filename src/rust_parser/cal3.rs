#[allow(dead_code, unused, unused_variables)]
pub mod learn_parser {
    use std::fmt::{Display, Formatter};

    use TokenType::{Eof, Integer, Minus, Plus};

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    pub enum TokenType {
        Integer,
        Plus,
        Minus,
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
                Minus => {
                    write!(f, "-")
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
        text: Vec<char>,
        pos: usize,
        current_token: Token,
        current_char: char,
    }

    impl Interpreter {
        pub fn new<S: AsRef<str>>(text: S) -> Self {
            let text = text
                .as_ref()
                .as_bytes()
                .iter()
                .map(|c| *c as char)
                .collect::<Vec<_>>();

            let current_char = text[0];

            Self {
                text,
                pos: 0,
                current_token: Token::new(Eof, "None"),
                current_char,
            }
        }

        fn advance(&mut self) {
            self.pos += 1;
            if self.pos > (self.text.len() - 1) {
                self.current_char = '\0';
            } else {
                self.current_char = self.text[self.pos]
            }
        }

        fn get_current_token_type(&self) -> TokenType {
            self.current_token.token_type
        }

        fn integer(&mut self) -> String {
            let mut result = vec![];
            while self.current_char != '\0' && self.current_char.is_ascii_digit() {
                result.push(self.current_char);
                self.advance();
            }
            result.iter().collect::<String>()
        }

        fn current_token_to_integer(&self) -> i64 {
            self.current_token
                .token_value
                .as_bytes()
                .iter()
                .map(|c| *c as char)
                .collect::<String>()
                .parse::<i64>()
                .expect("Error parser input")
        }

        pub fn get_next_token(&mut self) -> Token {
            while self.current_char != '\0' {
                match self.current_char {
                    '+' => {
                        self.advance();
                        return Token::new(Plus, "+");
                    }
                    '-' => {
                        self.advance();
                        return Token::new(Minus, "-");
                    }
                    _ => {
                        if self.current_char.is_whitespace() {
                            self.advance();
                            continue;
                        }
                        if self.current_char.is_ascii_digit() {
                            return Token::new(Integer, self.integer());
                        }
                    }
                }
            }
            Token::new(Eof, "None")
        }

        fn eat(&mut self, token_type: TokenType) {
            if self.current_token.token_type == token_type {
                self.current_token = self.get_next_token();
            } else {
                panic!("Error parsing input");
            }
        }

        fn term(&mut self) -> i64 {
            let i = self.current_token_to_integer();
            self.eat(Integer);
            i
        }

        pub fn expr(&mut self) -> i64 {
            self.current_token = self.get_next_token();

            let mut result = self.term();

            while self.get_current_token_type() == Plus || self.get_current_token_type() == Minus {
                if self.get_current_token_type() == Plus {
                    self.eat(Plus);
                    result += self.term();
                } else if self.get_current_token_type() == Minus {
                    self.eat(Minus);
                    result -= self.term();
                }
            }
            result
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::rust_parser::cal3::learn_parser::Interpreter;

    #[test]
    fn test_3() {
        let interpreter = |s| -> i64 { Interpreter::new(s).expr() };

        assert_eq!(15, interpreter("15"));
        assert_eq!(3, interpreter("7 - 4 "));
        assert_eq!(5, interpreter("7 - 3 + 2 -1"));
        assert_eq!(5, interpreter("10 +1 +2 -3 + 4 + 6 -15"));
        assert_eq!(15, interpreter("15+0000"));
    }
}
