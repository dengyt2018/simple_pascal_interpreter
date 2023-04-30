#[allow(dead_code, unused, unused_variables)]
pub mod parser {
    use std::collections::HashMap;
    use std::fmt::{Display, Formatter};

    const EOF: char = '\0';
    pub const EOF_TYPE: u32 = 1;
    pub const NAME: u32 = 2;
    pub const COMMA: u32 = 3;
    pub const LBRACK: u32 = 4;
    pub const RBRACK: u32 = 5;
    pub const EQUALS: u32 = 6;
    const TOKEN_NAMES: [&str; 7] = [
        "n/a", "<EOF>", "NAME", "COMMA", "LBRACK", "RBRACK", "EQUALS",
    ];

    pub fn get_token_name(x: usize) -> &'static str {
        TOKEN_NAMES[x]
    }

    #[derive(Debug, Eq, PartialEq)]
    pub struct Token {
        pub token_type: u32,
        pub token_text: String,
    }

    impl Token {
        pub fn new<S: AsRef<str>>(token_type: u32, token_text: S) -> Self {
            Self {
                token_type,
                token_text: token_text.as_ref().into(),
            }
        }
    }

    impl Display for Token {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let tname = get_token_name(self.token_type as usize);
            write!(f, "<'{}',{}>", self.token_text, tname)
        }
    }

    pub struct BacktrackParserMemo {
        // from where do we get tokens?
        input: ListLexer,
        // stack of index markers into lookahead buffer
        markers: Vec<usize>,
        // dynamically-sized lookahead buffer
        lookahead: Vec<Token>,
        // index of current lookahead token; LT(1) returns lookahead[p]
        p: usize,
        // element match fails flags
        flags: bool,

        /** Map input position to FAILED or previous stop token index.
         *  null implies we've not parsed this rule at that index.
         */
        list_memo: HashMap<usize, usize>,
    }

    impl BacktrackParserMemo {
        // parsing failed on last attempt
        const FAILED: usize = usize::MAX;

        pub(crate) fn new(mut input: ListLexer) -> Self {
            // make lookahead buffer
            // prime buffer with at least 1 token
            let lookahead = vec![input.next_token()];
            Self {
                input,
                markers: vec![], // make marker stack
                lookahead,
                p: 0,
                flags: true,
                list_memo: Default::default(),
            }
        }

        fn consume(&mut self) {
            self.p += 1;

            // have we hit end of buffer when not backtracking?
            if self.p == self.lookahead.len() && self.markers.is_empty() {
                // if so, it's an opportunity to start filling at index 0 again
                self.p = 0;
                // size goes to 0, but retains memory
                self.lookahead.clear();
                self.clear_memo();
            }
            // get another to replace consumed token
            self.sync(1);
        }

        /** Make sure we have i tokens from current position p */
        fn sync(&mut self, i: usize) {
            let p = self.p + i - 1;
            let l = self.lookahead.len() - 1;

            // out of tokens?
            if p > l {
                // get n tokens
                let n = p - l;
                // add n tokens
                (0..n).for_each(|_| self.lookahead.push(self.input.next_token()));
            }
        }

        pub fn lt(&mut self, i: usize) -> &Token {
            self.sync(i);
            &self.lookahead[self.p + i - 1]
        }

        pub fn la(&mut self, i: usize) -> u32 {
            self.lt(i).token_type
        }

        pub fn match_token(&mut self, x: u32) {
            if self.la(1) == x {
                self.consume();
                self.flags = true;
            } else {
                self.flags = false;
            }
        }

        fn seek(&mut self, marker: usize) {
            self.p = marker;
        }

        // pushes the current token index onto a stack
        pub fn mark(&mut self) -> usize {
            self.markers.push(self.p);
            self.p
        }

        // pops the index back off the stack and rewinds
        // p to that position. We need a stack of markers to handle nested backtracking.
        pub fn release(&mut self) {
            let marker = self.markers[self.markers.len() - 1];
            self.markers.pop();
            self.seek(marker);
        }

        /** Have we parsed a particular rule before at this input position?
         *  If no memoization value, we've never parsed here before.
         *  If memoization value is FAILED, we parsed and failed before.
         *  If value >= 0, it is an index into the token buffer.  It indicates
         *  a previous successful parse.  This method has a side effect:
         *  it seeks ahead in the token buffer to avoid reparsing.
         */
        pub fn already_parsed_rule(&mut self) -> bool {
            if let Some(memo_i) = self.list_memo.get(self.index()) {
                println!(
                    "parsed list before at index {}; skip ahead to token index {}: {}",
                    self.index(),
                    memo_i,
                    self.lookahead[*memo_i].to_string()
                );
                if *memo_i == BacktrackParserMemo::FAILED {
                    panic!("already error.");
                } else {
                    self.seek(*memo_i);
                    true
                }
            } else {
                false
            }
        }

        /** While backtracking, record partial parsing results.
         *  If invoking rule method failed, record that fact.
         *  If it succeeded, record the token position we should skip to
         *  next time we attempt this rule for this input position.
         */
        pub fn memoize(&mut self, start_token_index: usize, failed: bool) {
            // record token just after last in rule if success
            match failed {
                true => {
                    self.list_memo
                        .insert(start_token_index, BacktrackParserMemo::FAILED);
                }
                false => {
                    self.list_memo.insert(start_token_index, *self.index());
                }
            }
        }

        fn index(&self) -> &usize {
            &self.p // return current input position
        }
    }

    pub trait Memo {
        fn stat(&mut self);
        fn speculate_stat_alt1(&mut self) -> bool;
        fn speculate_stat_alt2(&mut self) -> bool;
        fn assign(&mut self);
        fn list(&mut self);
        fn _list(&mut self);
        fn elements(&mut self);
        fn element(&mut self);
        fn clear_memo(&mut self);
    }

    impl Memo for BacktrackParserMemo {
        fn stat(&mut self) {
            // attempt alternative 1: list EOF
            if self.speculate_stat_alt1() {
                self.list();
                self.match_token(EOF_TYPE);

                // attempt alternative 2: assign EOF
            } else if self.speculate_stat_alt2() {
                println!("predict alternative 2");
                self.assign();
                self.match_token(EOF_TYPE)
            } else {
                self.flags = false;
                // must be an error; neither matched; LT(1) is lookahead token 1
                panic!("\nExpecting stat found: {}", self.lt(1).to_string());
            }
        }

        fn speculate_stat_alt1(&mut self) -> bool {
            println!("attempt alternative 1");
            // mark this spot in input so we can rewind
            self.mark();

            self.list();
            self.match_token(EOF_TYPE);

            // either way, rewind to where we were
            self.release();

            self.flags
        }

        fn speculate_stat_alt2(&mut self) -> bool {
            println!("attempt alternative 2");
            // mark this spot in input so we can rewind
            self.mark();

            self.assign();
            self.match_token(EOF_TYPE);

            // either way, rewind to where we were
            self.release();

            self.flags
        }

        fn assign(&mut self) {
            self.list();
            self.match_token(EQUALS);
            self.list();
        }

        fn list(&mut self) {
            let start_token_index = *self.index();
            if self.markers.is_empty() && self.already_parsed_rule() {
                return;
            }

            self._list();

            if self.markers.is_empty() {
                self.memoize(start_token_index, self.flags);
            }
        }

        fn _list(&mut self) {
            println!("parse list rule at token index: {}", self.index());
            self.match_token(LBRACK);
            self.elements();
            self.match_token(RBRACK);
        }

        /** elements : element (',' element)* ; // match comma-separated list */
        fn elements(&mut self) {
            self.element();
            while self.la(1) == COMMA {
                self.match_token(COMMA);
                self.element();
            }
        }

        /** element : name '=' NAME | NAME | list ; // assignment, name or list */
        fn element(&mut self) {
            let la1 = self.la(1);
            let la2 = self.la(2);
            self.flags = true;
            if la1 == NAME && la2 == EQUALS {
                self.match_token(NAME);
                self.match_token(EQUALS);
                self.match_token(NAME);
            } else if la1 == NAME {
                self.match_token(NAME);
            } else if la1 == LBRACK {
                self.list();
            } else {
                self.flags = false;
            }
        }

        fn clear_memo(&mut self) {
            self.list_memo.clear();
        }
    }

    pub struct BacktrackParser {
        // from where do we get tokens?
        input: ListLexer,
        // stack of index markers into lookahead buffer
        markers: Vec<usize>,
        // dynamically-sized lookahead buffer
        lookahead: Vec<Token>,
        // index of current lookahead token; LT(1) returns lookahead[p]
        p: usize,
        // element match fails flags
        flags: bool,
    }

    impl BacktrackParser {
        pub(crate) fn new(mut input: ListLexer) -> Self {
            // make lookahead buffer
            // prime buffer with at least 1 token
            let lookahead = vec![input.next_token()];
            Self {
                input,
                markers: vec![], // make marker stack
                lookahead,
                p: 0,
                flags: true,
            }
        }

        fn consume(&mut self) {
            self.p += 1;

            // have we hit end of buffer when not backtracking?
            if self.p == self.lookahead.len() && self.markers.is_empty() {
                // if so, it's an opportunity to start filling at index 0 again
                self.p = 0;
                // size goes to 0, but retains memory
                self.lookahead.clear();
            }
            // get another to replace consumed token
            self.sync(1);
        }

        /** Make sure we have i tokens from current position p */
        fn sync(&mut self, i: usize) {
            let p = self.p + i - 1;
            let l = self.lookahead.len() - 1;

            // out of tokens?
            if p > l {
                // get n tokens
                let n = p - l;
                // add n tokens
                (0..n).for_each(|_| self.lookahead.push(self.input.next_token()));
            }
        }

        pub fn lt(&mut self, i: usize) -> &Token {
            self.sync(i);
            &self.lookahead[self.p + i - 1]
        }

        pub fn la(&mut self, i: usize) -> u32 {
            self.lt(i).token_type
        }

        pub fn match_token(&mut self, x: u32) {
            if self.la(1) == x {
                self.consume();
                self.flags = true;
            } else {
                self.flags = false;
            }
        }

        fn seek(&mut self, marker: usize) {
            self.p = marker;
        }

        // pushes the current token index onto a stack
        pub fn mark(&mut self) -> usize {
            self.markers.push(self.p);
            self.p
        }

        // pops the index back off the stack and rewinds
        // p to that position. We need a stack of markers to handle nested backtracking.
        pub fn release(&mut self) {
            let marker = self.markers[self.markers.len() - 1];
            self.markers.pop();
            self.seek(marker);
        }
    }

    pub trait Stat {
        fn stat(&mut self);
        fn speculate_stat_alt1(&mut self) -> bool;
        fn speculate_stat_alt2(&mut self) -> bool;
        fn assign(&mut self);
        fn list(&mut self);
        fn elements(&mut self);
        fn element(&mut self);
    }

    impl Stat for BacktrackParser {
        /** stat : list EOF | assign EOF ; */
        fn stat(&mut self) {
            // attempt alternative 1: list EOF
            if self.speculate_stat_alt1() {
                self.list();
                self.match_token(EOF_TYPE);

                // attempt alternative 2: assign EOF
            } else if self.speculate_stat_alt2() {
                self.assign();
                self.match_token(EOF_TYPE)
            } else {
                self.flags = false;
                // must be an error; neither matched; LT(1) is lookahead token 1
                panic!("\nExpecting stat found: {}", self.lt(1).to_string());
            }
        }

        fn speculate_stat_alt1(&mut self) -> bool {
            // mark this spot in input so we can rewind
            self.mark();

            self.list();
            self.match_token(EOF_TYPE);

            // either way, rewind to where we were
            self.release();

            self.flags
        }

        fn speculate_stat_alt2(&mut self) -> bool {
            // mark this spot in input so we can rewind
            self.mark();

            self.assign();
            self.match_token(EOF_TYPE);

            // either way, rewind to where we were
            self.release();

            self.flags
        }

        /** assign : list '=' list ; // parallel assignment */
        fn assign(&mut self) {
            self.list();
            self.match_token(EQUALS);
            self.list();
        }

        /** list : '[' elements ']' ; // match bracketed list */
        fn list(&mut self) {
            self.match_token(LBRACK);
            self.elements();
            self.match_token(RBRACK);

            eprintln!("---list ok!");
        }

        /** elements : element (',' element)* ; // match comma-separated list */
        fn elements(&mut self) {
            self.element();
            while self.la(1) == COMMA {
                self.match_token(COMMA);
                self.element();
            }
        }

        /** element : name '=' NAME | NAME | list ; // assignment, name or list */
        fn element(&mut self) {
            let la1 = self.la(1);
            let la2 = self.la(2);
            self.flags = true;
            if la1 == NAME && la2 == EQUALS {
                self.match_token(NAME);
                self.match_token(EQUALS);
                self.match_token(NAME);
            } else if la1 == NAME {
                self.match_token(NAME);
            } else if la1 == LBRACK {
                self.list();
            } else {
                self.flags = false;
            }
        }
    }

    pub struct LookaheadParser {
        // from where do we get tokens?
        input: ListLexer,
        // circular lookahead buffer
        lookahead: Vec<Token>,
        // how many lookahead symbols
        k: usize,
        // circular index of next token position to fill
        p: usize,
    }

    /** LL(k) Recursive-Descent Parser */
    impl LookaheadParser {
        pub fn new(mut input: ListLexer, k: usize) -> Self {
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

        pub fn lt(&mut self, i: usize) -> &Token {
            // circular fetch
            &self.lookahead[(self.p + i - 1) % self.k]
        }

        pub fn la(&mut self, i: usize) -> u32 {
            self.lt(i).token_type
        }

        pub fn match_token(&mut self, x: u32) {
            if self.la(1) == x {
                self.consume();
            } else {
                eprintln!(
                    "\nExpecting {}; found: {}",
                    get_token_name(x as usize),
                    self.lt(1).to_string()
                );
                panic!()
            }
        }
    }

    pub trait List {
        fn list(&mut self);
        fn elements(&mut self);
        fn element(&mut self) -> bool;
    }

    impl List for LookaheadParser {
        /** list : '[' elements ']' ; // match bracketed list */
        fn list(&mut self) {
            self.match_token(LBRACK);
            self.elements();
            self.match_token(RBRACK);
        }

        /** elements : element (',' element)* ; // match comma-separated list */
        fn elements(&mut self) {
            self.element();
            while self.la(1) == COMMA {
                self.match_token(COMMA);
                self.element();
            }
        }

        /** element : NAME '=' NAME | NAME | list ; assignment, NAME or list */
        fn element(&mut self) -> bool {
            let success_match = true;
            let l1 = self.la(1);
            let l2 = self.la(2);
            if l1 == NAME && l2 == EQUALS {
                self.match_token(NAME);
                self.match_token(EQUALS);
                self.match_token(NAME)
            } else if l1 == NAME {
                self.match_token(NAME);
            } else if l1 == LBRACK {
                self.list();
            } else {
                eprintln!("\nExpecting name or list; found {}", self.lt(1).to_string());
                return !success_match;
            }
            success_match
        }
    }

    pub struct ListLexer {
        input: String,
        p: usize,
        c: char,
    }

    /** LL(1) Recursive-Descent Lexer */
    impl ListLexer {
        pub fn new<S: AsRef<str>>(input: S) -> Self {
            let s: String = input.as_ref().into();
            let c = s.as_bytes()[0] as char;
            Self { input: s, p: 0, c }
        }

        fn consume(&mut self) {
            self.p += 1;
            if self.p >= self.input.len() {
                self.c = EOF;
            } else {
                self.c = self.input.as_bytes()[self.p] as char;
            }
        }

        pub fn next_token(&mut self) -> Token {
            while self.c != EOF {
                match self.c {
                    ' ' | '\t' | '\r' | '\n' => {
                        self.ws();
                        continue;
                    }
                    ',' => {
                        self.consume();
                        return Token::new(COMMA, ",");
                    }
                    '[' => {
                        self.consume();
                        return Token::new(LBRACK, "[");
                    }
                    ']' => {
                        self.consume();
                        return Token::new(RBRACK, "]");
                    }
                    '=' => {
                        self.consume();
                        return Token::new(EQUALS, "=");
                    }
                    _ => {
                        if self.is_letter() {
                            return self.name();
                        } else {
                            panic!("invalid character: {}", self.c);
                        }
                    }
                }
            }
            Token::new(EOF_TYPE, "<EOF>")
        }

        fn name(&mut self) -> Token {
            let mut buf = vec![];
            while self.is_letter() {
                buf.push(self.c);
                self.consume();
            }

            Token::new(NAME, buf.into_iter().collect::<String>())
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
    }

    pub struct ListParser {
        // from where do we get tokens?
        input: ListLexer,
        // the current lookahead token
        lookahead: Token,
    }

    /** LL(1) Recursive-Descent Parser */
    impl ListParser {
        pub fn new(mut input: ListLexer) -> Self {
            let lookahead = input.next_token();
            Self { input, lookahead }
        }

        fn match_token(&mut self, x: u32) {
            if self.lookahead.token_type == x {
                self.consume();
            }
        }

        fn consume(&mut self) {
            self.lookahead = self.input.next_token();
        }

        /** list : '[' elements ']' ; // match bracketed list */
        pub fn list(&mut self) {
            self.match_token(LBRACK);
            self.elements();
            self.match_token(RBRACK);
        }

        /** elements : element (',' element)* ; */
        fn elements(&mut self) {
            self.element();
            while self.lookahead.token_type == COMMA {
                self.match_token(COMMA);
                self.element();
            }
        }

        /** element : name | list ; // element is name or nested list */
        fn element(&mut self) {
            if self.lookahead.token_type == NAME {
                self.match_token(NAME);
            } else if self.lookahead.token_type == LBRACK {
                self.list();
            } else {
                eprintln!(
                    "\nExpecting name or list; found: {}",
                    self.lookahead.to_string()
                );
                panic!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::libs::llparser::parser::{
        BacktrackParser, BacktrackParserMemo, List, ListLexer, ListParser, LookaheadParser, Memo,
        Stat, Token, COMMA, EOF_TYPE, LBRACK, NAME, RBRACK,
    };

    #[test]
    fn test_list_lexer() {
        let input = "[ a , b ]";
        let mut lexer = ListLexer::new(input);

        assert_eq!(Token::new(LBRACK, "["), lexer.next_token());
        assert_eq!(Token::new(NAME, "a"), lexer.next_token());
        assert_eq!(Token::new(COMMA, ","), lexer.next_token());
        assert_eq!(Token::new(NAME, "b"), lexer.next_token());
        assert_eq!(Token::new(RBRACK, "]"), lexer.next_token());
        assert_eq!(Token::new(EOF_TYPE, "<EOF>"), lexer.next_token());
    }

    #[test]
    fn test_list_parse() {
        let input = "[a, b, b]";
        let lexer = ListLexer::new(input);
        let mut parser = ListParser::new(lexer);

        parser.list();
    }

    #[test]
    fn test_lookahead_parser() {
        let input = "[a,b=c,,[d,e]]";
        let lexer = ListLexer::new(input);
        let mut parser = LookaheadParser::new(lexer, 2);

        parser.list();
    }

    #[test]
    fn test_backtrack_parser() {
        let input = "[a] = [b]";
        let lexer = ListLexer::new(input);
        let mut parser = BacktrackParser::new(lexer);

        parser.stat();
    }

    #[test]
    fn test_backtrack_memo_parser() {
        let input = "[a,b] = [c,d]";
        let lexer = ListLexer::new(input);
        let mut parser = BacktrackParserMemo::new(lexer);

        parser.stat();
    }
}
