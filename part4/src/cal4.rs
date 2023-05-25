#[allow(dead_code, unused, unused_variables)]
pub mod learn_parser {
    use std::fmt::{Display, Formatter};

    use TokenType::*;

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    pub enum TokenType {
        Integer,
        Mul,
        Div,
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
                Mul => {
                    write!(f, "*")
                }
                Div => {
                    write!(f, "/")
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
                    '*' => {
                        self.advance();
                        return Token::new(Mul, "*");
                    }
                    '/' => {
                        self.advance();
                        return Token::new(Div, "/");
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

        fn factor(&mut self) -> i64 {
            let i = self.current_token_to_integer();
            self.eat(Integer);
            i
        }

        pub fn expr(&mut self) -> i64 {
            self.current_token = self.get_next_token();

            let mut result = self.factor();

            while self.get_current_token_type() == Mul || self.get_current_token_type() == Div {
                if self.get_current_token_type() == Mul {
                    self.eat(Mul);
                    result *= self.factor();
                } else if self.get_current_token_type() == Div {
                    self.eat(Div);
                    result /= self.factor();
                }
            }
            result
        }
    }
}

#[cfg(test)]
mod tests {
    use super::learn_parser::*;

    #[test]
    fn test_4() {
        let interpreter = |s| -> i64 { Interpreter::new(s).expr() };

        assert_eq!(14, interpreter("7 * 4 /2"));
        assert_eq!(42, interpreter("7 * 4 / 2 * 3 "));
        assert_eq!(30, interpreter("10 * 4 * 2 *3 / 8"));
    }
}
