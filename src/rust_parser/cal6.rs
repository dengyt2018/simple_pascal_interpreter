#[allow(dead_code, unused, unused_variables)]
pub mod learn_parser {
    use std::fmt::{Display, Formatter};

    use TokenType::{Div, Eof, Integer, Lbrack, Minus, Mul, Plus, Rbrack};

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    pub enum TokenType {
        Integer,
        Mul,
        Div,
        Minus,
        Plus,
        Lbrack,
        Rbrack,
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
                Minus => {
                    write!(f, "-")
                }
                Plus => {
                    write!(f, "+")
                }
                Lbrack => {
                    write!(f, "(")
                }
                Rbrack => {
                    write!(f, ")")
                }
            }
        }
    }

    impl Display for Token {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "Token<{},{}>", self.token_type, self.token_value)
        }
    }

    pub struct Lexer {
        text: Vec<char>,
        pos: usize,
        current_char: char,
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
                text,
                pos: 0,
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

        fn integer(&mut self) -> String {
            let mut result = vec![];
            while self.current_char != '\0' && self.current_char.is_ascii_digit() {
                result.push(self.current_char);
                self.advance();
            }
            result.iter().collect::<String>()
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
                    '+' => {
                        self.advance();
                        return Token::new(Plus, "+");
                    }
                    '-' => {
                        self.advance();
                        return Token::new(Minus, "-");
                    }
                    '(' => {
                        self.advance();
                        return Token::new(Lbrack, "(");
                    }
                    ')' => {
                        self.advance();
                        return Token::new(Rbrack, ")");
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
    }

    pub struct Interpreter {
        lexer: Lexer,
        current_token: Token,
    }

    impl Interpreter {
        pub fn new(mut lexer: Lexer) -> Self {
            let current_token = lexer.get_next_token();
            Self {
                lexer,
                current_token,
            }
        }

        fn token_type(&self) -> TokenType {
            self.current_token.token_type
        }

        fn to_integer(&self) -> i64 {
            self.current_token
                .token_value
                .as_bytes()
                .iter()
                .map(|c| *c as char)
                .collect::<String>()
                .parse::<i64>()
                .expect("Error parser input current_token_to_integer")
        }

        fn eat(&mut self, token_type: TokenType) {
            if self.token_type() == token_type {
                self.current_token = self.lexer.get_next_token();
            } else {
                panic!("Error parsing input eat");
            }
        }

        fn factor(&mut self) -> i64 {
            let mut result = 0;
            if self.token_type() == Integer {
                result = self.to_integer();
                self.eat(Integer);
            }
            if self.token_type() == Lbrack {
                self.eat(Lbrack);
                result = self.expr();
                self.eat(Rbrack);
            }
            result
        }

        fn term(&mut self) -> i64 {
            let mut result = self.factor();

            while self.token_type() == Mul || self.token_type() == Div {
                if self.token_type() == Mul {
                    self.eat(Mul);
                    result *= self.factor();
                } else if self.token_type() == Div {
                    self.eat(Div);
                    result /= self.factor();
                }
            }
            result
        }

        pub fn expr(&mut self) -> i64 {
            let mut result = self.term();
            while self.token_type() == Plus || self.token_type() == Minus {
                if self.token_type() == Plus {
                    self.eat(Plus);
                    result += self.term();
                } else if self.token_type() == Minus {
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
    use crate::rust_parser::cal6::learn_parser::{Interpreter, Lexer};

    #[test]
    fn test_6() {
        let interpreter = |s: &str| -> i64 { Interpreter::new(Lexer::new(s)).expr() };

        assert_eq!(3, interpreter("3"));
        assert_eq!(26, interpreter("7*4-2"));
        assert_eq!(5, interpreter("7 - 8 / 4"));
        assert_eq!(17, interpreter("14 +2 * 3 -6 / 2"));
        assert_eq!(22, interpreter("7 + 3 * (10 / (12 / (3 + 1) - 1))"));
        assert_eq!(
            10,
            interpreter("7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)")
        );
        assert_eq!(12, interpreter("7+ (((3 + 2)))"));
        assert_ne!(5, interpreter("1 + ( 1 + 1) +1)+ 1"));
    }
}
