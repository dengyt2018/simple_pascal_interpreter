#[allow(dead_code, unused, unused_variables, unused_imports)]
pub mod pascal_parser {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::fmt::{Display, Formatter};
    use std::rc::Rc;
    use std::str::FromStr;

    use TokenType::{
        Assign, Begin, Div, Dot, End, Eof, Id, Integer, Lbrack, Minus, Modulo, Mul, Plus, Rbrack,
        Semi,
    };

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
        Id,
        Assign,
        Begin,
        End,
        Semi,
        Dot,
        Eof,
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
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

        pub fn token_type(&self) -> TokenType {
            self.token_type
        }
    }

    impl Default for Token {
        fn default() -> Self {
            Self {
                token_type: Eof,
                token_value: "None".to_string(),
            }
        }
    }

    impl Display for TokenType {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Integer => {
                    write!(f, "INTEGER")
                }
                Eof => {
                    write!(f, "EOF")
                }
                Mul => {
                    write!(f, "MUL")
                }
                Div => {
                    write!(f, "DIV")
                }
                Modulo => {
                    write!(f, "MODULO")
                }
                Minus => {
                    write!(f, "MINUS")
                }
                Plus => {
                    write!(f, "PLUS")
                }
                Lbrack => {
                    write!(f, "LBRACK")
                }
                Rbrack => {
                    write!(f, "RBRACK")
                }
                Id => {
                    write!(f, "ID")
                }
                Assign => {
                    write!(f, "ASSIGN")
                }
                Begin => {
                    write!(f, "BEGIN")
                }
                End => {
                    write!(f, "END")
                }
                Semi => {
                    write!(f, "SEMI")
                }
                Dot => {
                    write!(f, "DOT")
                }
            }
        }
    }

    impl Display for Token {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "Token<{}, {}>", self.token_type, self.token_value)
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum ReservedKeywords {
        Begin,
        End,
    }

    impl FromStr for ReservedKeywords {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "BEGIN" => Ok(ReservedKeywords::Begin),
                "END" => Ok(ReservedKeywords::End),
                _ => Err(()),
            }
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

        /**
        Advance the `pos` pointer and set the `current_char` variable.
         */
        fn advance(&mut self) {
            self.pos += 1;
            if self.pos > (self.text.len() - 1) {
                self.current_char = '\0'; // Indicates end of input
            } else {
                self.current_char = self.text[self.pos]
            }
        }

        fn peek(&self) -> Option<char> {
            let peek_pos = self.pos + 1;
            if peek_pos > self.text.len() - 1 {
                None
            } else {
                Some(self.text[peek_pos])
            }
        }

        /**
        Return a (multidigit) integer consumed from the input.
         */
        fn integer(&mut self) -> String {
            let mut result = vec![];
            while self.current_char != '\0' && self.current_char.is_ascii_digit() {
                result.push(self.current_char);
                self.advance();
            }
            result.iter().collect::<String>()
        }

        /**
        Handle identifiers and reserved keywords
         */
        fn _id(&mut self) -> Token {
            let mut buffer = Vec::new();
            while !self.current_char.is_whitespace() && self.current_char.is_alphanumeric() {
                buffer.push(self.current_char);
                self.advance();
            }
            let token = buffer.iter().collect::<String>();
            if let Ok(key) = ReservedKeywords::from_str(&token) {
                match key {
                    ReservedKeywords::Begin => Token::new(Begin, token),
                    ReservedKeywords::End => Token::new(End, token),
                }
            } else {
                Token::new(Id, token)
            }
        }

        /**
        Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
         */
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
                    ';' => {
                        self.advance();
                        return Token::new(Semi, ";");
                    }
                    '.' => {
                        self.advance();
                        return Token::new(Dot, ".");
                    }
                    ':' => {
                        if let Some(e) = self.peek() {
                            if e == '=' {
                                self.advance();
                                self.advance();
                                return Token::new(Assign, ":=");
                            }
                        }
                        panic!("parser assign error");
                    }
                    _ => {
                        if self.current_char.is_whitespace() {
                            self.advance();
                            continue;
                        }
                        if self.current_char.is_ascii_digit() {
                            return Token::new(Integer, self.integer());
                        }
                        if self.current_char.is_alphabetic() {
                            return self._id();
                        }
                    }
                }
            }
            Token::new(Eof, "EOF")
        }
    }

    #[derive(Debug, Clone)]
    pub enum Statements {
        Token(Token),
        Binop(Token),
        Assign(Token),
        Num {
            value: String,
        },
        UnaryOp {
            token: Token,
            expr: Rc<RefCell<ASTTree>>,
        },
        Compound {
            children: Rc<RefCell<Vec<Rc<RefCell<ASTTree>>>>>,
        },

        NoOp,
        Var {
            value: String,
        },
    }

    impl Display for Statements {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Statements::Token(t) | Statements::Binop(t) | Statements::Assign(t) => {
                    write!(f, "{}", t)
                }
                Statements::Num { value } => {
                    write!(f, "Num: {}", value)
                }
                Statements::UnaryOp { token, expr } => {
                    write!(f, "{},{:#?}", token, expr.take())
                }
                Statements::Compound { children } => {
                    write!(f, "{:#?}", children.take())
                }
                Statements::NoOp => {
                    write!(f, "NoOp")
                }
                Statements::Var { value } => {
                    write!(f, "Var: {}", value)
                }
            }
        }
    }

    impl Statements {
        pub fn get_num(&self) -> String {
            match self {
                Statements::Num { value } => value.clone(),
                _ => {
                    panic!("this method ony for Num");
                }
            }
        }

        pub fn get_varname(&self) -> String {
            match self {
                Statements::Var { value } => value.clone(),
                _ => {
                    panic!("this method ony for Var");
                }
            }
        }

        pub fn get_unary(&self) -> (Token, Rc<RefCell<ASTTree>>) {
            match self {
                Statements::UnaryOp { token, expr } => (token.clone(), expr.clone()),
                _ => {
                    panic!("this method only for Unary");
                }
            }
        }

        pub fn get_token(&self) -> Token {
            match self {
                Statements::Token(t) | Statements::Assign(t) | Statements::Binop(t) => t.clone(),
                _ => {
                    panic!("wrong statements");
                }
            }
        }

        pub fn get_compound(&self) -> Rc<RefCell<Vec<Rc<RefCell<ASTTree>>>>> {
            if let Statements::Compound { children } = self {
                Rc::clone(children)
            } else {
                panic!("compound can not be empty.")
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ASTTree {
        left: Option<Rc<RefCell<ASTTree>>>,
        stat: Statements,
        right: Option<Rc<RefCell<ASTTree>>>,
    }

    impl ASTTree {
        pub fn new(left: ASTTree, stat: Statements, right: ASTTree) -> Rc<RefCell<Self>> {
            let p = Rc::new(RefCell::new(Self {
                left: Some(Rc::clone(&Rc::new(RefCell::new(left)))),
                stat,
                right: Some(Rc::clone(&Rc::new(RefCell::new(right)))),
            }));
            Rc::clone(&p)
        }

        pub fn set_stat(&mut self, stat: Statements) {
            self.stat = stat;
        }
    }

    impl Default for ASTTree {
        fn default() -> Self {
            Self {
                left: None,
                stat: Statements::NoOp,
                right: None,
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

        /**
        compare the current token type with the passed token
        type and if they match then "eat" the current token
        and assign the next token to the self.current_token,
        otherwise raise an exception.
         */
        fn eat(&mut self, token_type: TokenType) {
            if self.current_token.token_type == token_type {
                self.current_token = self.lexer.get_next_token();
            }
        }

        /**
        ```
            program : compound_statement DOT
        ```
         */
        fn program(&mut self) -> ASTTree {
            let node = self.compound_statement();
            self.eat(Dot);
            node
        }

        /**
        ```
            compound_statement: BEGIN statement_list END
        ```
         */
        fn compound_statement(&mut self) -> ASTTree {
            self.eat(Begin);
            let nodes = self.statement_list();
            self.eat(End);

            let mut root = Rc::new(RefCell::new(vec![]));

            nodes.iter().for_each(|node| {
                let child = Rc::new(RefCell::new(node.clone()));
                root.borrow_mut().push(Rc::clone(&child));
            });

            let mut node = ASTTree::default();
            node.set_stat(Statements::Compound {
                children: Rc::clone(&root),
            });

            node
        }

        /**
        ```
            statement_list : statement
                        | statement SEMI statement_list
        ```
         */
        fn statement_list(&mut self) -> Vec<ASTTree> {
            let node = self.statement();

            let mut results = vec![node];

            while self.current_token.token_type == Semi {
                self.eat(Semi);
                results.push(self.statement())
            }
            if self.current_token.token_type == Id {
                panic!("")
            }

            results
        }

        /**
        ```
            statement : compound_statement
                    | assignment_statement
                    | empty
        ```
         */

        fn statement(&mut self) -> ASTTree {
            if self.current_token.token_type == Begin {
                self.compound_statement()
            } else if self.current_token.token_type == Id {
                self.assignment_statement()
            } else {
                self.empty()
            }
        }

        /**
        ```
            assignment_statement : variable ASSIGN expr
        ```
         */
        fn assignment_statement(&mut self) -> ASTTree {
            let left = self.variable();
            let token = self.current_token.clone();
            self.eat(Assign);
            let right = self.expr();

            let assign = Statements::Assign(token);

            ASTTree::new(left, assign, right).take()
        }

        /**
        ```
            variable : ID
        ```
         */
        fn variable(&mut self) -> ASTTree {
            let value = self.current_token.token_value.clone();
            let mut node = ASTTree::default();
            self.eat(Id);
            node.set_stat(Statements::Var { value });
            node
        }

        /**
        ```
            An empty production
        ```
         */
        fn empty(&self) -> ASTTree {
            ASTTree::default()
        }

        /**
        ```
            factor : PLUS factor
                    | MINUS factor
                    | INTEGER
                    | LPAREN expr RPAREN
                    | variable
        ```
         */
        fn factor(&mut self) -> ASTTree {
            let token = self.current_token.clone();
            let mut node = ASTTree::default();

            match token.token_type {
                Integer => {
                    self.eat(Integer);
                    let value = token.token_value;
                    node.set_stat(Statements::Num { value });
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
                    node.set_stat(Statements::UnaryOp {
                        token,
                        expr: Rc::new(RefCell::new(self.factor())),
                    });

                    node
                }
                Minus => {
                    self.eat(Minus);
                    node.set_stat(Statements::UnaryOp {
                        token,
                        expr: Rc::new(RefCell::new(self.factor())),
                    });

                    node
                }

                _ => self.variable(),
            }
        }

        /**
        ```
            term : factor ((MUL | DIV) factor)*
        ```
         */
        fn term(&mut self) -> ASTTree {
            let mut node = self.factor();
            while self.current_token.token_type == Mul
                || self.current_token.token_type == Div
                || self.current_token.token_type == Modulo
            {
                let token = self.current_token.clone();
                if token.token_type == Mul {
                    self.eat(Mul);
                } else if token.token_type == Div {
                    self.eat(Div)
                } else if token.token_type == Modulo {
                    self.eat(Modulo)
                }

                node = ASTTree::new(node, Statements::Binop(token), self.factor()).take()
            }
            node
        }

        /**
        ```
            expr : term ((PLUS | MINUS) term)*
        ```
         */
        fn expr(&mut self) -> ASTTree {
            let mut node = self.term();

            while self.current_token.token_type == Plus || self.current_token.token_type == Minus {
                let token = self.current_token.clone();
                if token.token_type == Plus {
                    self.eat(Plus);
                } else if token.token_type == Minus {
                    self.eat(Minus);
                }
                node = ASTTree::new(node, Statements::Binop(token), self.term()).take()
            }
            node
        }

        /**
        ```
            program : compound_statement DOT

            compound_statement : BEGIN statement_list END

            statement_list : statement
                            | statement SEMI statement_list

            statement : compound_statement
                        | assignment_statement
                        | empty

            assignment_statement : variable ASSIGN expr

            empty :

            expr: term ((PLUS | MINUS) term)*

            term: factor ((MUL | DIV) factor)*

            factor : PLUS factor
                    | MINUS factor
                    | INTEGER
                    | LPAREN expr RPAREN
                    | variable

            variable: ID
        ```
         */
        pub fn parser(&mut self) -> ASTTree {
            let node = self.program();
            if self.current_token.token_type != Eof {
                panic!("parser error...");
            }

            node
        }
    }

    pub struct Interpreter {
        parser: Parser,
        global_scope: Rc<RefCell<HashMap<String, i64>>>,
    }

    impl Interpreter {
        pub fn new(parser: Parser) -> Self {
            Self {
                parser,
                global_scope: Default::default(),
            }
        }

        fn set_global_scope<S: AsRef<str>>(&mut self, var_name: S, var: i64) {
            let var_name = var_name.as_ref().into();
            self.global_scope.borrow_mut().insert(var_name, var);
        }

        fn visit(&mut self, node: ASTTree) -> Option<i64> {
            match node.stat {
                Statements::Binop(_) => Some(self.visit_binop(node)),
                Statements::Assign(_) => {
                    self.visit_assign(node);
                    None
                }
                Statements::Num { .. } => Some(self.visit_num(node)),
                Statements::UnaryOp { .. } => Some(self.visit_unary(node)),
                Statements::Compound { .. } => {
                    self.visit_compound(node);
                    None
                }
                Statements::NoOp => {
                    self.visit_noop(node);
                    None
                }
                Statements::Var { .. } => Some(self.visit_var(node)),
                _ => None,
            }
        }

        fn visit_binop(&mut self, node: ASTTree) -> i64 {
            let left = self.visit(node.left.unwrap().take()).unwrap();
            let right = self.visit(node.right.unwrap().take()).unwrap();

            let token = node.stat.get_token();
            if token.token_type == Plus {
                left + right
            } else if token.token_type == Minus {
                left - right
            } else if token.token_type == Mul {
                left * right
            } else if token.token_type == Div {
                left / right
            } else if token.token_type == Modulo {
                left % right
            } else {
                panic!("visit binop get wrong type.")
            }
        }

        fn visit_num(&mut self, node: ASTTree) -> i64 {
            node.stat
                .get_num()
                .parse::<i64>()
                .unwrap_or_else(|_| panic!("visit_num parse error. found {}", node.stat))
        }

        fn visit_unary(&mut self, node: ASTTree) -> i64 {
            let (token, node) = node.stat.get_unary();
            match token.token_type {
                Minus => -self.visit(node.take()).unwrap(),
                Plus => self.visit(node.take()).unwrap(),
                _ => {
                    panic!("visit unary get wrong type")
                }
            }
        }

        fn visit_compound(&mut self, node: ASTTree) {
            let children = node.stat.get_compound();
            children.borrow().iter().for_each(|child| {
                self.visit(child.take());
            });
        }

        fn visit_assign(&mut self, node: ASTTree) {
            if let Some(var_name) = node.left {
                let var_name = var_name.take().stat.get_varname();
                let var = self.visit(node.right.unwrap().take()).unwrap();
                self.set_global_scope(var_name, var);
            }
        }

        fn visit_var(&mut self, node: ASTTree) -> i64 {
            let var_name = node.stat.get_varname();
            let hash = self.global_scope.borrow();
            let var = hash.get(&var_name);
            match var {
                None => {
                    panic!("visit var can not find var")
                }
                Some(v) => *v,
            }
        }

        fn visit_noop(&mut self, node: ASTTree) {
            if let Statements::NoOp = node.stat {
                // pass
            } else {
                panic!("visit noop get wrong token.")
            }
        }

        fn _interpret(&mut self) -> Option<i64> {
            let tree = self.parser.parser();
            self.visit(tree)
        }

        pub fn interpret(&mut self) -> HashMap<String, i64> {
            self._interpret();
            self.global_scope.take()
        }
    }
}

#[allow(dead_code, unused, unused_variables, unused_imports)]
#[cfg(test)]
mod tests {
    use super::pascal_parser::*;

    #[test]
    fn test_9_lexer() {
        let mut lexer = Lexer::new("BEGIN a := 125; END.");
        let tokens = vec![
            Token::new(TokenType::Begin, "BEGIN"),
            Token::new(TokenType::Id, "a"),
            Token::new(TokenType::Assign, ":="),
            Token::new(TokenType::Integer, "125"),
            Token::new(TokenType::Semi, ";"),
            Token::new(TokenType::End, "END"),
            Token::new(TokenType::Dot, "."),
            Token::new(TokenType::Eof, "EOF"),
        ];

        tokens
            .iter()
            .for_each(|token| assert_eq!(token, &lexer.get_next_token()));
    }

    #[test]
    fn test_ast_tree() {
        let k = Parser::new(Lexer::new("BEGIN a := 125+12; END.")).parser();

        eprintln!("{k:#?}");
    }

    #[test]
    fn test_parser() {
        let case = vec![
            ("33", 33),
            ("2 + 7 * 4", 30),
            ("7 - 8 / 4", 5),
            ("14 + 2 * 3 - 6 / 2", 17),
            ("7 + 3 * (10 / (12 / (3 + 1) - 1))", 22),
            (
                "7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)",
                10,
            ),
            ("7 + (((3 + 2)))", 12),
            ("- 3", -3),
            ("+ 3", 3),
            ("5 - - - + - 3", 8),
            ("5 - - - + - (3 + 4) - +2", 10),
        ];

        let interpreter = |x: &str| {
            let s = format!("BEGIN a := {}; END.", x);
            Interpreter::new(Parser::new(Lexer::new(s))).interpret()
        };

        case.iter().for_each(|(s, i)| {
            let k = *interpreter(s).get("a").unwrap();
            assert_eq!(k, *i);
        });
    }

    #[test]
    fn test_statements() {
        let text = "
BEGIN

    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number / 4;
        c := a - - b
    END;

    x := 11;
END.
"
            .to_string();
        let interpreter = |x: &str| Interpreter::new(Parser::new(Lexer::new(x))).interpret();

        let hash = interpreter(text.as_str());
        assert_eq!(5, hash.len());
        assert_eq!(2, *hash.get("number").unwrap());
        assert_eq!(2, *hash.get("a").unwrap());
        assert_eq!(25, *hash.get("b").unwrap());
        assert_eq!(27, *hash.get("c").unwrap());
        assert_eq!(11, *hash.get("x").unwrap());
    }
}
