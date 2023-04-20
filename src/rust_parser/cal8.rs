#[allow(dead_code, unused, unused_variables, unused_imports)]
pub mod learn_parser {
    use std::cell::RefCell;
    use std::fmt::{Display, Formatter};
    use std::rc::Rc;

    use TokenType::{Div, Eof, Integer, Lbrack, Minus, Modulo, Mul, Plus, Rbrack};

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    pub enum TokenType {
        Integer,
        Mul,
        Div,
        Modulo,
        Minus,
        Plus,
        Lbrack,
        Rbrack,
        Eof,
    }

    #[derive(Debug, Clone)]
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

    impl Default for Token {
        fn default() -> Self {
            Self {
                token_type: TokenType::Eof,
                token_value: "None".to_string(),
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
                Modulo => {
                    write!(f, "%")
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
                    '%' => {
                        self.advance();
                        return Token::new(Modulo, "%");
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

    #[derive(Debug, Clone)]
    pub enum Operation {
        Num(Token),
        Unary(Token),
        Binop(Token),
    }

    impl Operation {
        pub fn get_op(&self) -> &Token {
            match self {
                Operation::Num(t) => t,
                Operation::Unary(t) => t,
                Operation::Binop(t) => t,
            }
        }
    }

    #[derive(Debug)]
    pub struct ASTTree {
        left: Option<Rc<RefCell<ASTTree>>>,
        op: Operation,
        right: Option<Rc<RefCell<ASTTree>>>,
        expr: Option<Rc<RefCell<ASTTree>>>,
    }

    impl ASTTree {
        pub fn new(
            left: Rc<RefCell<ASTTree>>,
            op: Operation,
            right: Rc<RefCell<ASTTree>>,
        ) -> Rc<RefCell<Self>> {
            let tree = Self {
                left: Option::Some(left),
                op,
                right: Option::Some(right),
                expr: None,
            };
            let p = Rc::new(RefCell::new(tree));

            Rc::clone(&p)
        }

        pub fn set_token(&mut self, op: Operation) {
            self.op = op;
        }

        pub fn set_expr(&mut self, expr: Option<ASTTree>) {
            if let Some(e) = expr {
                let p = Rc::new(RefCell::new(e));
                self.expr = Option::Some(Rc::clone(&p));
            } else {
                self.expr = None;
            }
        }
    }

    impl Default for ASTTree {
        fn default() -> Self {
            let op = Operation::Binop(Token::new(Eof, "None"));
            Self {
                left: None,
                op,
                right: None,
                expr: None,
            }
        }
    }

    pub struct Parser {
        lexer: Lexer,
        current_token: Token,
    }

    impl Parser {
        pub fn new(mut lexer: Lexer) -> Self {
            let current_token = lexer.get_next_token();
            Self {
                lexer,
                current_token,
            }
        }

        fn eat(&mut self, token_type: TokenType) {
            if self.current_token.token_type == token_type {
                self.current_token = self.lexer.get_next_token();
            }
        }

        fn factor(&mut self) -> ASTTree {
            let token = self.current_token.clone();
            let mut node = ASTTree::default();

            match token.token_type {
                Integer => {
                    self.eat(Integer);
                    node.set_token(Operation::Num(token));
                    node
                }
                Lbrack => {
                    self.eat(Lbrack);
                    let node = self.expr();
                    self.eat(Rbrack);
                    node
                }
                Plus => {
                    self.eat(Plus);
                    node.set_token(Operation::Unary(token));
                    node.set_expr(Option::Some(self.factor()));
                    node
                }
                Minus => {
                    self.eat(Minus);
                    node.set_token(Operation::Unary(token));
                    node.set_expr(Option::Some(self.factor()));
                    node
                }

                _ => panic!(
                    "factor token type why are you here? found {}",
                    token.token_type.to_string()
                ),
            }
        }

        fn term(&mut self) -> ASTTree {
            let mut node = self.factor();

            while self.current_token.token_type == Mul || self.current_token.token_type == Div {
                let token = self.current_token.clone();
                if token.token_type == Mul {
                    self.eat(Mul)
                } else if token.token_type == Div {
                    self.eat(Div);
                } else if token.token_type == Modulo {
                    self.eat(Modulo);
                }
                let left = Rc::new(RefCell::new(node));
                let right = Rc::new(RefCell::new(self.factor()));
                node = ASTTree::new(Rc::clone(&left), Operation::Binop(token), Rc::clone(&right))
                    .take();
            }

            node
        }

        fn expr(&mut self) -> ASTTree {
            let mut node = self.term();

            while self.current_token.token_type == Plus || self.current_token.token_type == Minus {
                let token = self.current_token.clone();
                if token.token_type == Plus {
                    self.eat(Plus);
                } else if token.token_type == Minus {
                    self.eat(Minus);
                }
                let left = Rc::new(RefCell::new(node));
                let right = Rc::new(RefCell::new(self.term()));
                node = ASTTree::new(Rc::clone(&left), Operation::Binop(token), Rc::clone(&right))
                    .take();
            }

            node
        }

        pub fn parser(&mut self) -> ASTTree {
            self.expr()
        }
    }

    pub struct Interpreter {
        parser: Parser,
    }

    impl Interpreter {
        pub fn new(parser: Parser) -> Self {
            Self { parser }
        }

        fn visit(&mut self, node: ASTTree) -> i64 {
            let op = node.op.clone();
            match op {
                Operation::Num(_) => self.visit_num(node),
                Operation::Unary(_) => self.visit_unary(node),
                Operation::Binop(_) => self.visit_binop(node),
            }
        }

        fn visit_binop(&mut self, node: ASTTree) -> i64 {
            let left = self.visit(node.left.unwrap().take());
            let right = self.visit(node.right.unwrap().take());
            let op = node.op.get_op().clone();

            match op.token_type {
                Mul => left * right,
                Div => left / right,
                Minus => left - right,
                Plus => left + right,
                _ => todo!(),
            }
        }

        fn visit_num(&mut self, node: ASTTree) -> i64 {
            let token = node.op.get_op().clone();
            token
                .token_value
                .parse::<i64>()
                .unwrap_or_else(|_| panic!("Parser token value error: {}", token.to_string()))
        }

        fn visit_unary(&mut self, node: ASTTree) -> i64 {
            let op = node.op.get_op().clone();

            match op.token_type {
                Minus => -self.visit(node.expr.unwrap().take()),
                Plus => self.visit(node.expr.unwrap().take()),
                _ => todo!(),
            }
        }

        pub fn interpret(&mut self) -> i64 {
            let tree = self.parser.parser();
            self.visit(tree)
        }
    }
}

#[allow(dead_code, unused, unused_variables, unused_imports)]
#[cfg(test)]
mod tests {
    use crate::rust_parser::cal8::learn_parser::{Interpreter, Lexer, Parser};

    #[test]
    fn test_8() {
        let result = |s: &str| -> i64 { Interpreter::new(Parser::new(Lexer::new(s))).interpret() };

        assert_eq!(-1, result("-1"));
        assert_eq!(0, result("-(--1 -1)"));
        assert_eq!(-1, result("-(--1 -1)-1"));
        assert_eq!(-1, result("-(-++-1 ++-1)---1"));
        assert_eq!(22, result("7 + 3 * (10 / (12 / (3 + 1) - 1))"));
        assert_eq!(12, result("7 + (((3 + 2)))"));
        assert_eq!(13, result("5+2*(3+1)"));
    }
}
