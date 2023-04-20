#[allow(dead_code)]

pub mod parser {
    use crate::libs::backup::ch2::parser::{Lexer, Token, TokenType};

    pub struct ListParser {
        input: Lexer,
        lookahead: Vec<Token>, // 向前看的Token
        k: usize,              // 向前长k个Token ll(k)
        p: usize,
    }

    impl ListParser {
        pub fn new(mut input: Lexer, k: usize) -> Self {
            let mut lookahead = vec![];
            (0..k).for_each(|_| lookahead.push(input.next_token()));

            Self {
                input,
                lookahead,
                k,
                p: 0,
            }
        }

        fn consume(&mut self) {
            self.lookahead[self.p] = self.input.next_token();
            self.p = (self.p + 1) % self.k;
        }

        // 向前看第i个Token
        fn lt(&mut self, i: usize) -> &Token {
            let p = (self.p + i - 1) % self.k;
            &self.lookahead[p]
        }

        // 向前看第i个Token的类型
        fn la(&mut self, i: usize) -> &TokenType {
            &self.lt(i).token_type
        }

        fn match_token_type(&mut self, token_type: &TokenType) {
            if self.la(1) == token_type {
                self.consume();
            } else {
                eprintln!("--expecting: {:?}, but found: {:?}", token_type, self.lt(1));
                panic!("match error");
            }
        }

        // list : '[' elements ']'
        pub fn list(&mut self) {
            self.match_token_type(&TokenType::LBracket);
            self.elements();
            self.match_token_type(&TokenType::RBracket);
            println!("--list ok");
        }

        // elements : element (',' element)*
        fn elements(&mut self) {
            self.element();
            while self.la(1) == &TokenType::Comma {
                self.match_token_type(&TokenType::Comma);
                self.element();
            }
        }

        // element : Name | Name '=' Name | list
        fn element(&mut self) {
            if self.la(1) == &TokenType::Name && self.la(2) == &TokenType::Assign {
                self.match_token_type(&TokenType::Name);
                self.match_token_type(&TokenType::Assign);
                self.match_token_type(&TokenType::Name);
            } else if self.la(1) == &TokenType::Name {
                self.match_token_type(&TokenType::Name);
            } else if self.la(1) == &TokenType::LBracket {
                self.list();
            } else {
                panic!("expecting name or list; found: {:?}", self.lt(1));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::libs::backup::ch2::parser::Lexer;
    use crate::libs::backup::ch4::parser::ListParser;

    #[test]
    fn test_ch4() {
        let input = "[a,b=c,[b,c]]], b=c]";

        let lexer = Lexer::new(input);
        let mut parser = ListParser::new(lexer, 2);

        parser.list();
    }
}
