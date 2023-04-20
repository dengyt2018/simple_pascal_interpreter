#[allow(dead_code)]

pub mod parser {
    use crate::libs::backup::ch2::parser::{Lexer, Token, TokenType};

    pub struct ListParser {
        input: Lexer,
        lookahead: Token,
    }

    impl ListParser {
        pub fn new(mut input: Lexer) -> Self {
            let lookahead = input.next_token();
            Self { input, lookahead }
        }

        fn consume(&mut self) {
            self.lookahead = self.input.next_token();
        }

        fn match_token_type(&mut self, token_type: TokenType) {
            if self.lookahead.token_type == token_type {
                self.consume();
            } else {
                panic!(
                    "expecting: {:?}  found: {:?}",
                    token_type, self.lookahead.token_type
                );
            }
        }

        // list : '[' elements ']'
        pub fn list(&mut self) {
            self.match_token_type(TokenType::LBracket);
            self.elements();
            self.match_token_type(TokenType::RBracket);
            println!("--list ok");
        }

        // elements : element (',' element)*
        fn elements(&mut self) {
            self.element();
            while self.lookahead.token_type == TokenType::Comma {
                self.match_token_type(TokenType::Comma);
                self.element();
            }
        }

        // element : Name | list
        fn element(&mut self) {
            if self.lookahead.token_type == TokenType::Name {
                self.match_token_type(TokenType::Name);
            } else if self.lookahead.token_type == TokenType::LBracket {
                self.list();
            } else {
                panic!("expecting name or list; found: {:?}", self.lookahead);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::libs::backup::ch2::parser::Lexer;
    use crate::libs::backup::ch3::parser::ListParser;

    #[test]
    fn test_ch3() {
        let input = "[hello, [ab,c],c]";

        let lexer = Lexer::new(input);
        let mut parser = ListParser::new(lexer);

        parser.list();
    }
}
