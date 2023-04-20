#[allow(dead_code, unused, unused_variables)]
pub mod parser {
    use crate::libs::backup::ch2::parser::{Lexer, Token, TokenType};

    pub struct BacktrackParser {
        input: Lexer,
        markers: Vec<usize>,
        // 缓存解析结果
        lookahead: Vec<Token>,
        // 向前看的Token
        p: usize,
        backtrack_flag: bool, // backtrack flag
    }

    impl BacktrackParser {
        pub fn new(mut input: Lexer) -> Self {
            let lookahead = vec![input.next_token()];

            Self {
                input,
                markers: vec![],
                lookahead,
                p: 0,
                backtrack_flag: false,
            }
        }
        fn sync(&mut self, i: usize) {
            let s = self.p + i - 1;
            let l = self.lookahead.len() - 1;
            if s > l {
                self.fill(s - l);
            }
        }
        fn fill(&mut self, n: usize) {
            (0..n).for_each(|_| self.lookahead.push(self.input.next_token()));
        }

        fn lt(&mut self, i: usize) -> &Token {
            self.sync(i);
            &self.lookahead[self.p + i - 1]
        }

        fn la(&mut self, i: usize) -> &TokenType {
            &self.lt(i).token_type
        }

        fn consume(&mut self) {
            self.p += 1;
            if self.p == self.lookahead.len() && !self.is_speculating() {
                self.p = 0;
                self.lookahead.clear();
            }
            self.sync(1);
        }

        fn mark(&mut self) -> usize {
            self.markers.push(self.p);
            self.p
        }

        fn release(&mut self) {
            let marker = self.markers[self.markers.len() - 1];
            self.markers.pop();
            self.seek(marker);
        }

        fn seek(&mut self, index: usize) {
            self.p = index;
        }

        fn is_speculating(&self) -> bool {
            !self.markers.is_empty()
        }

        fn match_token_type(&mut self, token_type: &TokenType) {
            let la = self.la(1);
            if self.la(1) == token_type {
                self.consume();
                self.backtrack_flag = true;
            } else {
                self.backtrack_flag = false;
            }
        }

        // stat: list EOF | assign EOF
        // stat: list EOF | list '=' list EOF
        pub fn stat(&mut self) {
            if self.stat_alt1() {
                self.list();
                self.match_token_type(&TokenType::Eof);
            } else if self.stat_alt2() {
                self.assign();
                self.match_token_type(&TokenType::Eof);
            } else {
                panic!("stat error.");
            }
        }

        // list EOF
        fn stat_alt1(&mut self) -> bool {
            self.mark();

            self.list();
            self.match_token_type(&TokenType::Eof);

            self.release();

            self.backtrack_flag
        }

        // assign EOF
        fn stat_alt2(&mut self) -> bool {
            self.mark();

            self.assign();
            self.match_token_type(&TokenType::Eof);

            self.release();

            self.backtrack_flag
        }

        fn assign(&mut self) {
            self.list();
            self.match_token_type(&TokenType::Assign);
            self.list();
        }

        // list : '[' elements ']'
        fn list(&mut self) {
            self.match_token_type(&TokenType::LBracket);

            self.elements();

            self.match_token_type(&TokenType::RBracket);
            eprintln!("--- list parser.");
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
            self.backtrack_flag = true;

            if self.la(1) == &TokenType::Name && self.la(2) == &TokenType::Assign {
                self.match_token_type(&TokenType::Name);
                self.match_token_type(&TokenType::Assign);
                self.match_token_type(&TokenType::Name);
            } else if self.la(1) == &TokenType::Name {
                self.match_token_type(&TokenType::Name);
            } else if self.la(1) == &TokenType::LBracket {
                self.list();
            } else {
                self.backtrack_flag = false;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::libs::backup::ch2::parser::Lexer;
    use crate::libs::backup::ch5::parser::BacktrackParser;

    #[test]
    fn test_ch5() {
        let _input = "[a,b] = [c,d]";

        let input = "[a]=[c,d,e]";

        let lexer = Lexer::new(input);
        let mut parser = BacktrackParser::new(lexer);

        parser.stat();
    }
}
