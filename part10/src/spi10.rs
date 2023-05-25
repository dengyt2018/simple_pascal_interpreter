#[allow(dead_code, unused, unused_variables, unused_imports)]
pub mod pascal_parser {
    use num_traits::{Float, Num};
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::fmt::{write, Display, Formatter};
    use std::rc::Rc;
    use std::str::FromStr;

    use TokenType::*;

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    pub enum TokenType {
        Integer,
        Real,
        IntegerConst,
        RealConst,
        Plus,
        Minus,
        Mul,
        IntegerDiv,
        FloatDiv,
        Modulo,
        Lbrack,
        Rbrack,
        Id,
        Assign,
        Begin,
        End,
        Semi,
        Dot,
        Program,
        Var,
        Colon,
        Comma,
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

        pub fn get_value(&self) -> String {
            self.token_value.clone()
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
                Real => {
                    write!(f, "REAL")
                }
                IntegerConst => {
                    write!(f, "INTEGER-CONST")
                }
                RealConst => {
                    write!(f, "REAL-CONST")
                }
                Plus => {
                    write!(f, "PLUS")
                }
                Minus => {
                    write!(f, "MINUS")
                }
                Mul => {
                    write!(f, "MUL")
                }
                IntegerDiv => {
                    write!(f, "INTEGER-DIV")
                }
                FloatDiv => {
                    write!(f, "FLOAT-DIV")
                }
                Modulo => {
                    write!(f, "MODULO")
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
                Program => {
                    write!(f, "PROGRAM")
                }
                Var => {
                    write!(f, "VAR")
                }
                Colon => {
                    write!(f, "COLON")
                }
                Comma => {
                    write!(f, "COMMA")
                }
                Eof => {
                    write!(f, "EOF")
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
        Program,
        Integer,
        Begin,
        Real,
        End,
        Var,
        Div,
    }

    impl FromStr for ReservedKeywords {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s.to_uppercase().as_str() {
                "PROGRAM" => Ok(ReservedKeywords::Program),
                "INTEGER" => Ok(ReservedKeywords::Integer),
                "BEGIN" => Ok(ReservedKeywords::Begin),
                "REAL" => Ok(ReservedKeywords::Real),
                "END" => Ok(ReservedKeywords::End),
                "VAR" => Ok(ReservedKeywords::Var),
                "DIV" => Ok(ReservedKeywords::Div),
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

        fn skip_comment(&mut self) {
            while self.current_char != '}' {
                self.advance();
            }
            // the closing curly brace
            self.advance();
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

        fn peek(&self) -> char {
            let peek_pos = self.pos + 1;
            if peek_pos > self.text.len() - 1 {
                '\0'
            } else {
                self.text[peek_pos]
            }
        }

        fn integer(&mut self, mut result: &mut Vec<char>) {
            while self.current_char != '\0' && self.current_char.is_ascii_digit() {
                result.push(self.current_char);
                self.advance();
            }
        }

        /**
        Return a (multidigit) integer or float consumed from the input.
         */
        fn number(&mut self) -> Token {
            let mut result = vec![];
            self.integer(&mut result);

            if self.current_char == '.' {
                result.push(self.current_char);
                self.advance();

                self.integer(&mut result);
                Token::new(RealConst, result.iter().collect::<String>())
            } else {
                Token::new(IntegerConst, result.iter().collect::<String>())
            }
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
                    ReservedKeywords::Program => Token::new(Program, token),
                    ReservedKeywords::Integer => Token::new(Integer, token),
                    ReservedKeywords::Begin => Token::new(Begin, token),
                    ReservedKeywords::Real => Token::new(Real, token),
                    ReservedKeywords::End => Token::new(End, token),
                    ReservedKeywords::Var => Token::new(Var, token),
                    ReservedKeywords::Div => Token::new(IntegerDiv, token),
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
                        return Token::new(FloatDiv, "/");
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
                    ',' => {
                        self.advance();
                        return Token::new(Comma, ",");
                    }
                    '{' => {
                        self.advance();
                        self.skip_comment();
                        continue;
                    }
                    ':' if self.peek() == '=' => {
                        self.advance();
                        self.advance();
                        return Token::new(Assign, ":=");
                    }
                    ':' => {
                        self.advance();
                        return Token::new(Colon, ":");
                    }
                    _ => {
                        if self.current_char.is_whitespace() {
                            self.advance();
                            continue;
                        }
                        if self.current_char.is_ascii_digit() {
                            return self.number();
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
        Type(Token),
        Token(Token),
        Binop(Token),
        Assign(Token),
        Var(Token),
        NoOp,
        Num {
            value: String,
        },

        UnaryOp {
            token: Token,
            expr: Rc<RefCell<ASTTree>>,
        },
        Program {
            name: String,
            block: Rc<RefCell<ASTTree>>,
        },
        Compound {
            children: Rc<RefCell<Vec<Rc<RefCell<ASTTree>>>>>,
        },
        Block {
            declaration: Rc<RefCell<Vec<Rc<RefCell<ASTTree>>>>>,
            compound_statement: Rc<RefCell<ASTTree>>,
        },

        VarDecl {
            var_node: Rc<RefCell<ASTTree>>,
            type_node: Rc<RefCell<ASTTree>>,
        },
    }

    impl Display for Statements {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Statements::Token(t)
                | Statements::Binop(t)
                | Statements::Var(t)
                | Statements::Assign(t)
                | Statements::Type(t) => {
                    write!(f, "{}", t.to_string())
                }
                Statements::Num { value } => {
                    write!(f, "Num: {}", value)
                }
                Statements::UnaryOp { token, expr } => {
                    write!(
                        f,
                        "unaryOp\ntoken:{}\nexpr:\n{:#?}",
                        token.to_string(),
                        expr.take()
                    )
                }
                Statements::Compound { children } => {
                    write!(f, "Compound:\n{:#?}", children.take())
                }
                Statements::NoOp => {
                    write!(f, "NoOp")
                }
                Statements::Program { name, block } => {
                    write!(f, "Program\nname:{}\nblock:{:#?}", name, block.take())
                }
                Statements::Block {
                    declaration,
                    compound_statement,
                } => {
                    write!(
                        f,
                        "Block\ndeclaration:{:#?}\ncompound_statement:{:#?}",
                        declaration.take(),
                        compound_statement.take()
                    )
                }

                Statements::VarDecl {
                    var_node,
                    type_node,
                } => {
                    write!(
                        f,
                        "VarDecl:\nvar_node:{:#?}\ntype_node:{:#?}",
                        var_node.take(),
                        type_node.take()
                    )
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
                Statements::Type(t)
                | Statements::Token(t)
                | Statements::Var(t)
                | Statements::Binop(t)
                | Statements::Assign(t) => t.clone(),
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

        pub fn get_program_block(&self) -> ASTTree {
            if let Statements::Program { name, block } = self {
                block.take()
            } else {
                panic!("program block can not be empty.")
            }
        }

        pub fn get_block(&self) -> (Vec<Rc<RefCell<ASTTree>>>, ASTTree) {
            if let Statements::Block {
                declaration,
                compound_statement,
            } = self
            {
                (declaration.take(), compound_statement.take())
            } else {
                panic!("block can not be empty.")
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

        pub fn num<S: AsRef<str>>(s: S) -> Self {
            let mut node = ASTTree::default();
            node.set_stat(Statements::Num {
                value: s.as_ref().into(),
            });
            node
        }

        pub fn var(token: Token) -> Self {
            let mut node = ASTTree::default();
            node.set_stat(Statements::Var(token));
            node
        }

        pub fn unary(token: Token, expr: ASTTree) -> Self {
            let mut node = ASTTree::default();
            node.set_stat(Statements::UnaryOp {
                token,
                expr: Rc::new(RefCell::new(expr)),
            });
            node
        }

        pub fn program<S: AsRef<str>>(name: S, block: ASTTree) -> Self {
            let mut node = ASTTree::default();
            node.set_stat(Statements::Program {
                name: name.as_ref().into(),
                block: Rc::new(RefCell::new(block)),
            });
            node
        }

        pub fn compound(children: Vec<Rc<RefCell<ASTTree>>>) -> Self {
            let mut node = ASTTree::default();
            node.set_stat(Statements::Compound {
                children: Rc::new(RefCell::new(children)),
            });
            node
        }

        pub fn block(declaration: Vec<Rc<RefCell<ASTTree>>>, compound_statement: ASTTree) -> Self {
            let mut node = ASTTree::default();
            node.set_stat(Statements::Block {
                declaration: Rc::new(RefCell::new(declaration)),
                compound_statement: Rc::new(RefCell::new(compound_statement)),
            });
            node
        }

        pub fn var_decl(var_node: ASTTree, type_node: ASTTree) -> Rc<RefCell<Self>> {
            let mut node = ASTTree::default();
            node.set_stat(Statements::VarDecl {
                var_node: Rc::new(RefCell::new(var_node)),
                type_node: Rc::new(RefCell::new(type_node)),
            });
            Rc::clone(&Rc::new(RefCell::new(node)))
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
            if self.current_token.token_type() == token_type {
                self.current_token = self.lexer.get_next_token();
            }
        }

        /**
        ```
            program : PROGRAM variable SEMI block DOT
        ```
         */
        fn program(&mut self) -> ASTTree {
            self.eat(Program);
            let var_node = self.variable().stat.get_token().get_value();
            self.eat(Semi);
            let block_node = self.block();
            self.eat(Dot);
            ASTTree::program(var_node, block_node)
        }

        /**
        ```
            block : declarations compound_statement
        ```
         */
        fn block(&mut self) -> ASTTree {
            ASTTree::block(self.declarations(), self.compound_statement())
        }

        /**
        ```
            declarations : VAR (variable_declaration SEMI)+
                        | empty
        ```
         */
        fn declarations(&mut self) -> Vec<Rc<RefCell<ASTTree>>> {
            let mut declarations = vec![];

            if self.current_token.token_type == Var {
                self.eat(Var);
                while self.current_token.token_type == Id {
                    let var_decl = self.variable_declaration();

                    declarations.extend(var_decl);

                    self.eat(Semi);
                }
            }
            //eprintln!("{:#?}", declarations);
            declarations
        }

        /**
        ```
            variable_declaration : ID (COMMA ID)* COLON type_spec
        ```
         */
        fn variable_declaration(&mut self) -> Vec<Rc<RefCell<ASTTree>>> {
            // first ID
            let mut var_nodes = vec![ASTTree::var(self.current_token.clone())];
            self.eat(Id);

            while self.current_token.token_type == Comma {
                self.eat(Comma);

                var_nodes.push(ASTTree::var(self.current_token.clone()));

                self.eat(Id);
            }

            self.eat(Colon);

            let type_node = self.type_spec();
            let mut var_declarations = vec![];

            var_nodes.iter().for_each(|var_node| {
                let var_node = var_node.clone();
                let var_decl = ASTTree::var_decl(var_node, type_node.clone());

                var_declarations.push(var_decl);
            });
            //eprintln!("{:#?}", var_declarations);
            var_declarations
        }

        /**
        ```
            type_spec : INTEGER
                    | REAL
        ```
         */
        fn type_spec(&mut self) -> ASTTree {
            let token = self.current_token.clone();
            if token.token_type == Integer {
                self.eat(Integer);
            } else if token.token_type == Real {
                self.eat(Real);
            }

            let mut node = ASTTree::default();
            node.set_stat(Statements::Type(token));

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

            let mut root = vec![];

            nodes.iter().for_each(|node| {
                let child = Rc::new(RefCell::new(node.clone()));
                root.push(Rc::clone(&child));
            });

            ASTTree::compound(root)
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

            ASTTree::new(left, Statements::Assign(token), right).take()
        }

        /**
        ```
            variable : ID
        ```
         */
        fn variable(&mut self) -> ASTTree {
            let var_name = self.current_token.clone();
            self.eat(Id);

            ASTTree::var(var_name)
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

            match token.token_type {
                Plus => {
                    self.eat(Plus);
                    ASTTree::unary(token, self.factor())
                }
                Minus => {
                    self.eat(Minus);
                    ASTTree::unary(token, self.factor())
                }
                IntegerConst => {
                    self.eat(IntegerConst);
                    ASTTree::num(token.token_value)
                }
                RealConst => {
                    self.eat(RealConst);
                    ASTTree::num(token.token_value)
                }
                Lbrack => {
                    self.eat(Lbrack);
                    let node = self.expr();
                    self.eat(Rbrack);
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
                || self.current_token.token_type == IntegerDiv
                || self.current_token.token_type == FloatDiv
                || self.current_token.token_type == Modulo
            {
                let token = self.current_token.clone();
                if token.token_type == Mul {
                    self.eat(Mul);
                } else if token.token_type == IntegerDiv {
                    self.eat(IntegerDiv)
                } else if token.token_type == FloatDiv {
                    self.eat(FloatDiv)
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
        program : PROGRAM variable SEMI block DOT

        block : declarations compound_statement

        declarations : VAR (variable_declaration SEMI)+
                    | empty

        variable_declaration : ID (COMMA ID)* COLON type_spec

        type_spec : INTEGER | REAL

        compound_statement : BEGIN statement_list END

        statement_list : statement
                    | statement SEMI statement_list

        statement : compound_statement
                | assignment_statement
                | empty

        assignment_statement : variable ASSIGN expr

        empty :

        expr : term ((PLUS | MINUS) term)*

        term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

        factor : PLUS factor
            | MINUS factor
            | INTEGER_CONST
            | REAL_CONST
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

    pub struct Interpreter<T>
    where
        T: num_traits::Float + num_traits::Num + num_traits::NumCast,
    {
        parser: Parser,
        global_scope: Rc<RefCell<HashMap<String, T>>>,
    }

    impl<T> Interpreter<T>
    where
        T: num_traits::Float + num_traits::Num + num_traits::NumCast,
    {
        pub fn new(parser: Parser) -> Self {
            Self {
                parser,
                global_scope: Default::default(),
            }
        }

        fn visit_program(&mut self, node: ASTTree) {
            self.visit(node.stat.get_program_block());
        }

        fn visit_block(&mut self, node: ASTTree) {
            let (declarations, compound_statement) = node.stat.get_block();
            declarations.iter().for_each(|n| {
                self.visit(n.take());
            });
            self.visit(compound_statement);
        }

        fn visit_type(&self, node: ASTTree) {
            {
                // pass
            }
        }

        fn visit_var_decl(&self, node: ASTTree) {
            {
                //pass
            }
        }

        fn visit(&mut self, node: ASTTree) -> Option<T> {
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
                Statements::Program { .. } => {
                    self.visit_program(node);
                    None
                }
                Statements::Block { .. } => {
                    self.visit_block(node);
                    None
                }
                _ => None,
            }
        }

        fn visit_binop(&mut self, node: ASTTree) -> T {
            let left = self.visit(node.left.unwrap().take()).unwrap();
            let right = self.visit(node.right.unwrap().take()).unwrap();

            let token = node.stat.get_token();
            if token.token_type == Plus {
                left + right
            } else if token.token_type == Minus {
                left - right
            } else if token.token_type == Mul {
                left * right
            } else if token.token_type == IntegerDiv {
                let r = left / right;
                T::from(r.to_i64().unwrap()).unwrap()
            } else if token.token_type == FloatDiv {
                left.div(right)
            } else if token.token_type == Modulo {
                left % right
            } else {
                panic!("visit binop get wrong type.")
            }
        }

        fn visit_num(&mut self, node: ASTTree) -> T {
            let value = node
                .stat
                .get_num()
                .parse::<f64>()
                .unwrap_or_else(|_| panic!("visit_num parse error. found {}", node.stat));
            T::from(value).unwrap()
        }

        fn visit_unary(&mut self, node: ASTTree) -> T {
            let (token, node) = node.stat.get_unary();
            match token.token_type {
                Minus => self.visit(node.take()).unwrap().neg(),
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
                let var_name = var_name.take().stat.get_token().token_value;
                let var = self.visit(node.right.unwrap().take()).unwrap();
                self.set_global_scope(var_name, T::from(var).unwrap());
            }
        }

        fn set_global_scope<S: AsRef<str>>(&mut self, var_name: S, var: T) {
            let var_name = var_name.as_ref().into();
            self.global_scope.borrow_mut().insert(var_name, var);
        }

        fn visit_var(&mut self, node: ASTTree) -> T {
            let var_name = node.stat.get_token().token_value;
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

        fn _interpret(&mut self) {
            let tree = self.parser.parser();
            self.visit(tree);
        }

        pub fn interpret(&mut self) -> HashMap<String, T> {
            self._interpret();
            self.global_scope.take()
        }
    }
}
