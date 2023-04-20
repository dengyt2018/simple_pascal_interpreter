#[allow(dead_code)]

pub mod parser {
    #[derive(Debug, Eq, PartialEq)]
    pub enum TokenType {
        Eof,
        Name,
        Comma,
        Assign,
        LBracket,
        RBracket,
    }

    #[derive(Debug, Eq, PartialEq)]
    pub struct Token {
        pub token_type: TokenType,
        pub token_text: String,
    }

    impl Token {
        pub fn new<S: AsRef<str>>(token_type: TokenType, token_text: S) -> Token {
            Token {
                token_type,
                token_text: token_text.as_ref().into(),
            }
        }

        pub fn string() -> String {
            "".to_string()
        }
    }

    pub struct Lexer {
        input: String,
        p: usize,
        c: char,
        eof: bool,
    }

    impl Lexer {
        pub fn new<S: AsRef<str>>(input: S) -> Self {
            let s: String = input.as_ref().into();
            let c = s.as_bytes()[0] as char;
            Self {
                input: s,
                p: 0,
                c,
                eof: false,
            }
        }

        fn consume(&mut self) {
            self.p += 1;
            if self.p >= self.input.len() {
                self.eof = true;
            } else {
                self.c = self.input.as_bytes()[self.p] as char;
            }
        }

        fn match_lexer(&mut self, x: char) {
            if self.c == x {
                self.consume();
            } else {
                panic!(
                    "expecting: {} found: {}",
                    String::from(x),
                    String::from(self.c)
                );
            }
        }

        fn is_letter(&self) -> bool {
            (self.c >= 'a' && self.c <= 'z') || (self.c >= 'A' && self.c <= 'Z')
        }

        fn ws(&mut self) {
            match self.c {
                ' ' | '\t' | '\n' | '\r' => {
                    self.consume();
                }
                _ => {}
            }
        }

        fn name(&mut self) -> Token {
            let mut buffer = Vec::new();
            while self.is_letter() && !self.eof {
                buffer.push(self.c);
                self.consume();
            }

            Token::new(TokenType::Name, buffer.into_iter().collect::<String>())
        }

        pub fn next_token(&mut self) -> Token {
            while !self.eof {
                match self.c {
                    ' ' | '\t' | '\r' | '\n' => {
                        self.ws();
                        continue;
                    }
                    ',' => {
                        self.consume();
                        return Token::new(TokenType::Comma, ",");
                    }
                    '[' => {
                        self.consume();
                        return Token::new(TokenType::LBracket, "[");
                    }
                    ']' => {
                        self.consume();
                        return Token::new(TokenType::RBracket, "]");
                    }
                    '=' => {
                        self.consume();
                        return Token::new(TokenType::Assign, "=");
                    }
                    _ => {
                        if self.is_letter() {
                            return self.name();
                        } else {
                            panic!("invalid char: {}", String::from(self.c));
                        }
                    }
                }
            }

            Token::new(TokenType::Eof, "<EOF>")
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::libs::backup::ch2::parser::{Lexer, TokenType};

    #[test]
    fn test_lexer() {
        let input = "[ hello, world \n \r ]";
        let mut lexer = Lexer::new(input);

        loop {
            let t = lexer.next_token();
            println!("{:?}", t);
            if t.token_type == TokenType::Eof {
                break;
            }
        }
    }
}
