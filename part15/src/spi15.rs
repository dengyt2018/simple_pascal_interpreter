#[allow(dead_code, unused, unused_variables, unused_imports)]
pub mod pascal_parser {
    use core::fmt;
    use std::cell::RefCell;
    use std::collections::{HashMap, LinkedList};
    use std::fmt::{write, Display, Formatter};
    use std::rc::Rc;
    use std::str::FromStr;

    use num_traits::{Float, Num};

    use TokenType::{
        Assign, Begin, Colon, Comma, Dot, End, Eof, FloatDiv, Id, Integer, IntegerConst,
        IntegerDiv, Lbrack, Minus, Modulo, Mul, Plus, Procedure, Program, Rbrack, Real, RealConst,
        Semi, Var,
    };

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    pub enum TokenType {
        Integer,
        Real,
        IntegerConst,
        RealConst,
        Procedure,
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
        lineno: usize,
        column: usize,
    }

    impl Token {
        pub fn new<S: AsRef<str>>(token_type: TokenType, token_value: S) -> Self {
            Self {
                token_type,
                token_value: token_value.as_ref().into(),
                lineno: 1,
                column: 1,
            }
        }

        pub fn token_type(&self) -> TokenType {
            self.token_type
        }

        pub fn get_value(&self) -> String {
            self.token_value.clone()
        }

        pub fn set_pos(mut self, lexer: &Lexer) -> Self {
            self.lineno = lexer.lineno;
            self.column = lexer.column;
            self
        }

        pub fn set_lineno_column(mut self, lineno: usize, column: usize) -> Self {
            self.lineno = lineno;
            self.column = column;
            self
        }

        pub fn set_token<S: AsRef<str>>(mut self, token_type: TokenType, token_value: S) -> Self {
            self.token_type = token_type;
            self.token_value = token_value.as_ref().into();
            self
        }

        fn error(&self, code: ErrorCode) -> String {
            let mut error_code = String::new();
            match code {
                ErrorCode::UnexpectedToken => {
                    error_code = "Unexpected token".to_string();
                }
                ErrorCode::DuplicateId => {
                    error_code = "Symbol(identifier) not found".to_string();
                }
                ErrorCode::IdNotFound => {
                    error_code = "Duplicate id found".to_string();
                }
            }
            format!(
                "Error {} '{}', on line: {} column: {}.",
                error_code, self.token_value, self.lineno, self.column
            )
        }
    }

    impl Default for Token {
        fn default() -> Self {
            Self {
                token_type: Eof,
                token_value: "None".to_string(),
                lineno: 1,
                column: 1,
            }
        }
    }

    pub enum ErrorCode {
        UnexpectedToken,
        IdNotFound,
        DuplicateId,
    }

    #[derive(Debug, PartialEq)]
    pub enum ReservedKeywords {
        Procedure,
        Program,
        Integer,
        Begin,
        Real,
        End,
        Var,
        Div,
    }

    impl FromStr for ReservedKeywords {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s.to_uppercase().as_str() {
                "PROCEDURE" => Ok(ReservedKeywords::Procedure),
                "PROGRAM" => Ok(ReservedKeywords::Program),
                "INTEGER" => Ok(ReservedKeywords::Integer),
                "BEGIN" => Ok(ReservedKeywords::Begin),
                "REAL" => Ok(ReservedKeywords::Real),
                "END" => Ok(ReservedKeywords::End),
                "VAR" => Ok(ReservedKeywords::Var),
                "DIV" => Ok(ReservedKeywords::Div),
                _ => Err(format!("Unsupported Reserved Keywords. {:#?}", s)),
            }
        }
    }

    impl From<TokenType> for ReservedKeywords {
        fn from(value: TokenType) -> Self {
            match value {
                Procedure => ReservedKeywords::Procedure,
                Program => ReservedKeywords::Program,
                Integer => ReservedKeywords::Integer,
                IntegerDiv => ReservedKeywords::Div,
                Begin => ReservedKeywords::Begin,
                Real => ReservedKeywords::Real,
                End => ReservedKeywords::End,
                Var => ReservedKeywords::Var,
                _ => {
                    panic!("Unsupported Reserved Keywords. {:#?}", value)
                }
            }
        }
    }

    pub struct Lexer {
        text: Vec<char>,
        pos: usize,
        current_char: char,
        lineno: usize,
        column: usize,
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
                lineno: 1,
                column: 1,
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
            if self.current_char == '\n' {
                self.lineno += 1;
                self.column = 0;
            }

            self.pos += 1;

            if self.pos > (self.text.len() - 1) {
                self.current_char = '\0'; // Indicates end of input
            } else {
                self.current_char = self.text[self.pos];
                self.column += 1;
            }
        }

        fn peek(&self) -> char {
            let peek_pos = &self.pos + 1;
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
            let mut token = Token::default().set_pos(self);

            if self.current_char == '.' {
                result.push(self.current_char);
                self.advance();

                self.integer(&mut result);

                token
                    .set_token(RealConst, result.iter().collect::<String>())
                    .set_pos(self)
            } else {
                token.set_token(IntegerConst, result.iter().collect::<String>())
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
            let token_name = buffer.iter().collect::<String>();

            let mut token = Token::default().set_pos(self);

            if let Ok(key) = ReservedKeywords::from_str(&token_name) {
                match key {
                    ReservedKeywords::Procedure => token.set_token(Procedure, token_name),
                    ReservedKeywords::Program => token.set_token(Program, token_name),
                    ReservedKeywords::Integer => token.set_token(Integer, token_name),
                    ReservedKeywords::Begin => token.set_token(Begin, token_name),
                    ReservedKeywords::Real => token.set_token(Real, token_name),
                    ReservedKeywords::End => token.set_token(End, token_name),
                    ReservedKeywords::Var => token.set_token(Var, token_name),
                    ReservedKeywords::Div => token.set_token(IntegerDiv, token_name),
                }
            } else {
                token.set_token(Id, token_name)
            }
        }

        /**
            Lexical analyzer (also known as scanner or tokenizer)

            This method is responsible for breaking a sentence
            apart into tokens. One token at a time.
        */
        pub fn get_next_token(&mut self) -> Token {
            let mut token = Token::default();
            while self.current_char != '\0' {
                match self.current_char {
                    '*' => {
                        self.advance();
                        return token.set_token(Mul, "*").set_pos(self);
                    }
                    '/' => {
                        self.advance();
                        return token.set_token(FloatDiv, "/").set_pos(self);
                    }
                    '%' => {
                        self.advance();
                        return token.set_token(Modulo, "%").set_pos(self);
                    }
                    '+' => {
                        self.advance();
                        return token.set_token(Plus, "+").set_pos(self);
                    }
                    '-' => {
                        self.advance();
                        return token.set_token(Minus, "-").set_pos(self);
                    }
                    '(' => {
                        self.advance();
                        return token.set_token(Lbrack, "(").set_pos(self);
                    }
                    ')' => {
                        self.advance();
                        return token.set_token(Rbrack, ")").set_pos(self);
                    }
                    ';' => {
                        self.advance();
                        return token.set_token(Semi, ";").set_pos(self);
                    }
                    '.' => {
                        self.advance();
                        return token.set_token(Dot, ".").set_pos(self);
                    }
                    ',' => {
                        self.advance();
                        return token.set_token(Comma, ",").set_pos(self);
                    }
                    '{' => {
                        self.advance();
                        self.skip_comment();
                        continue;
                    }
                    ':' if self.peek() == '=' => {
                        self.advance();
                        self.advance();
                        return token.set_token(Assign, ":=").set_pos(self);
                    }
                    ':' => {
                        self.advance();
                        return token.set_token(Colon, ":").set_pos(self);
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
            token.set_token(Eof, "EOF").set_pos(self)
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
        Param {
            var_name: Rc<RefCell<ASTTree>>,
            var_type: Rc<RefCell<ASTTree>>,
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
        ProcedureDecl {
            proc_name: Token,
            params: Vec<Rc<RefCell<ASTTree>>>,
            block_node: Rc<RefCell<ASTTree>>,
        },
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
                Statements::Var(t)
                | Statements::Type(t)
                | Statements::Token(t)
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

        pub fn get_params(&self) -> (ASTTree, ASTTree) {
            if let Statements::Param {
                var_name: var_node,
                var_type: type_node,
            } = self
            {
                (var_node.take(), type_node.take())
            } else {
                panic!("params can not be empty.")
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

        pub fn get_var_decl(&self) -> (ASTTree, ASTTree) {
            if let Statements::VarDecl {
                var_node,
                type_node,
            } = self
            {
                (var_node.take(), type_node.take())
            } else {
                panic!("var decl can not be empty.")
            }
        }

        pub fn get_procedure_decl(&self) -> (Token, Vec<Rc<RefCell<ASTTree>>>, ASTTree) {
            if let Statements::ProcedureDecl {
                proc_name,
                params,
                block_node,
            } = self
            {
                (proc_name.clone(), params.clone(), block_node.take())
            } else {
                panic!("procedure decl can not be empty.")
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ASTTree {
        pub left: Option<Rc<RefCell<ASTTree>>>,
        pub(crate) stat: Statements,
        pub right: Option<Rc<RefCell<ASTTree>>>,
        // for ASTVisualizer
        pub _num: usize,
    }

    impl ASTTree {
        pub fn new(left: ASTTree, stat: Statements, right: ASTTree) -> Rc<RefCell<Self>> {
            let p = Rc::new(RefCell::new(Self {
                left: Some(Rc::clone(&Rc::new(RefCell::new(left)))),
                stat,
                right: Some(Rc::clone(&Rc::new(RefCell::new(right)))),
                _num: 0,
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

        pub fn param(var_node: ASTTree, type_node: ASTTree) -> Rc<RefCell<Self>> {
            let mut node = ASTTree::default();
            node.set_stat(Statements::Param {
                var_name: Rc::new(RefCell::new(var_node)),
                var_type: Rc::new(RefCell::new(type_node)),
            });
            Rc::clone(&Rc::new(RefCell::new(node)))
        }

        pub fn procedure_decl(
            proc_name: Token,
            params: Vec<Rc<RefCell<ASTTree>>>,
            block_node: ASTTree,
        ) -> Rc<RefCell<Self>> {
            let mut node = ASTTree::default();
            node.set_stat(Statements::ProcedureDecl {
                proc_name,
                params,
                block_node: Rc::new(RefCell::new(block_node)),
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
                _num: 0,
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
            } else {
                panic!(
                    "Parser {} Expected: {:#?}",
                    self.current_token.error(ErrorCode::UnexpectedToken),
                    token_type
                );
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
            declarations : (VAR (variable_declaration SEMI)+)*
                | (PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI)*
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

            loop {
                match self.current_token.token_type {
                    Var => {
                        self.eat(Var);
                        while self.current_token.token_type == Id {
                            let var_decl = self.variable_declaration();
                            declarations.extend(var_decl);
                            self.eat(Semi);
                        }
                    }
                    Procedure => {
                        self.eat(Procedure);
                        let proc_name = self.current_token.clone();
                        self.eat(Id);
                        let mut params = vec![];

                        if self.current_token.token_type == Lbrack {
                            self.eat(Lbrack);
                            if let Some(parm) = self.formal_parameter_list() {
                                params.extend(parm);
                            }
                            self.eat(Rbrack);
                        }

                        self.eat(Semi);
                        let block_node = self.block();
                        let proc_decl = ASTTree::procedure_decl(proc_name, params, block_node);
                        declarations.push(proc_decl);
                        self.eat(Semi);
                    }
                    _ => break,
                }
            }

            declarations
        }

        /**
        ```
            formal_parameters : ID (COMMA ID)* COLON type_spec
        ```
        */
        fn formal_parameters(&mut self) -> Vec<Rc<RefCell<ASTTree>>> {
            let mut param_nodes = vec![];

            let mut param_tokens = vec![ASTTree::var(self.current_token.clone())];
            self.eat(Id);
            while self.current_token.token_type == Comma {
                self.eat(Comma);
                param_tokens.push(ASTTree::var(self.current_token.clone()));
                self.eat(Id);
            }

            self.eat(Colon);
            let var_type = self.type_spec();
            param_tokens.iter().for_each(|var_name| {
                let param_node = ASTTree::param(var_name.clone(), var_type.clone());
                param_nodes.push(param_node);
            });

            param_nodes
        }

        /**
        ```
            formal_parameter_list : formal_parameters
                        | formal_parameters SEMI formal_parameter_list
        ```
        */
        fn formal_parameter_list(&mut self) -> Option<Vec<Rc<RefCell<ASTTree>>>> {
            if self.current_token.token_type != Id {
                return None;
            }

            let mut param_nodes = self.formal_parameters();

            while self.current_token.token_type == Semi {
                self.eat(Semi);
                param_nodes.extend(self.formal_parameters());
            }

            Some(param_nodes)
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

            /// TODO expected Semi
            /// ```
            ///PROGRAM Test;
            ///VAR
            ///    a : REAL;
            ///BEGIN
            ///    a := 1.5 {assign here missing semi should be panic}
            ///END.
            /// ```
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

        declarations : (VAR (variable_declaration SEMI)+)*
           | (PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI)*
           | empty

        variable_declaration : ID (COMMA ID)* COLON type_spec

        formal_params_list : formal_parameters
                           | formal_parameters SEMI formal_parameter_list

        formal_parameters : ID (COMMA ID)* COLON type_spec

        type_spec : INTEGER

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
                panic!(
                    "Parser {}",
                    self.current_token.error(ErrorCode::UnexpectedToken)
                );
            }
            node
        }
    }

    pub struct Interpreter<T>
    where
        T: num_traits::Float + num_traits::Num + num_traits::NumCast,
    {
        parser: Parser,
        global_memory: Rc<RefCell<HashMap<String, T>>>,
    }

    impl<T> Interpreter<T>
    where
        T: num_traits::Float + num_traits::Num + num_traits::NumCast,
    {
        pub fn new(parser: Parser) -> Self {
            Self {
                parser,
                global_memory: Default::default(),
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
                Statements::ProcedureDecl { .. } => {
                    self.visit_procedure_decl(node);
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
                .unwrap_or_else(|_| panic!("visit_num parse error. found {:#?}", node.stat));
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
            self.global_memory.borrow_mut().insert(var_name, var);
        }

        fn visit_var(&mut self, node: ASTTree) -> T {
            let var_name = node.stat.get_token().token_value;
            let hash = self.global_memory.borrow();
            let var = hash.get(&var_name);
            match var {
                None => {
                    panic!("visit var can not find var")
                }
                Some(v) => *v,
            }
        }

        fn visit_procedure_decl(&mut self, node: ASTTree) {
            {
                // pass
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
            self.global_memory.take()
        }
    }
    #[derive(Debug, Eq, PartialEq, Clone)]
    pub enum SymbolType {
        Integer,
        Real,
    }

    impl From<TokenType> for SymbolType {
        fn from(value: TokenType) -> Self {
            match value {
                TokenType::Integer => SymbolType::Integer,
                TokenType::Real => SymbolType::Real,
                _ => {
                    panic!("symbol type error: {:#?}", value)
                }
            }
        }
    }

    impl Display for SymbolType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                SymbolType::Integer => {
                    write!(f, "INTEGER")
                }
                SymbolType::Real => {
                    write!(f, "REAL")
                }
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct VarSymbol {
        symbol_name: String,
        symbol_type: SymbolType,
    }

    impl Default for VarSymbol {
        fn default() -> Self {
            VarSymbol {
                symbol_name: "".to_string(),
                symbol_type: SymbolType::Integer,
            }
        }
    }

    impl Display for VarSymbol {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "symbol<{}:{}>", self.symbol_name, self.symbol_type)
        }
    }

    impl VarSymbol {
        pub fn new<S: AsRef<str>>(name: S, symbol_type: SymbolType) -> Self {
            Self {
                symbol_name: name.as_ref().into(),
                symbol_type,
            }
        }

        pub fn set_integer<S: AsRef<str>>(name: S) -> Self {
            Self::new(name, SymbolType::Integer)
        }

        pub fn set_real<S: AsRef<str>>(name: S) -> Self {
            Self::new(name, SymbolType::Real)
        }
    }

    #[derive(Debug, Clone, Default)]
    pub struct ProcedureSymbol {
        procedure_name: String,
        params: Vec<VarSymbol>,
    }

    impl Display for ProcedureSymbol {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut params = vec![];
            self.params.iter().for_each(|v| {
                params.push(v.to_string());
            });
            write!(f, "params<{}: {}>", self.procedure_name, params.join(","))
        }
    }

    #[derive(Debug, Clone)]
    pub enum Symbol {
        Var(VarSymbol),
        Procedure(ProcedureSymbol),
    }

    impl Display for Symbol {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Symbol::Var(v) => {
                    write!(f, "{}", v.to_string())
                }
                Symbol::Procedure(p) => {
                    write!(f, "{}", p.to_string())
                }
            }
        }
    }

    impl Symbol {
        pub fn get_name(&self) -> String {
            match self {
                Symbol::Var(v) => v.symbol_name.clone(),
                Symbol::Procedure(p) => p.procedure_name.clone(),
            }
        }
        pub fn get_var(&self) -> VarSymbol {
            match self {
                Symbol::Var(v) => v.clone(),
                _ => {
                    panic!("type error")
                }
            }
        }
        pub fn get_procedure(&self) -> ProcedureSymbol {
            match self {
                Symbol::Procedure(p) => p.clone(),
                _ => {
                    panic!("type error")
                }
            }
        }
        pub fn set_var(var: VarSymbol) -> Symbol {
            Symbol::Var(var)
        }
    }

    #[derive(Debug, Default)]
    pub struct ScopedSymbolTable {
        _symbols: HashMap<String, Rc<RefCell<Symbol>>>,
        scope_name: String,
    }

    impl Display for ScopedSymbolTable {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut symbols = vec![];
            self._symbols.iter().for_each(|(name, symbol)| {
                symbols.push(format!("{}", symbol.borrow().to_string()));
            });
            write!(f, "SCOPE_NAME:<{}>\n{:#?}", self.scope_name, symbols)
        }
    }

    #[derive(Debug, Default)]
    pub(crate) struct SemanticAnalyzer {
        scopes: LinkedList<Rc<RefCell<ScopedSymbolTable>>>,
        current_scope: Rc<RefCell<ScopedSymbolTable>>,
    }

    impl ScopedSymbolTable {
        pub fn new<S: AsRef<str>>(name: S) -> Self {
            Self {
                _symbols: Default::default(),
                scope_name: name.as_ref().into(),
            }
        }
        pub fn _init_builtins(&mut self) {
            let b = vec![
                VarSymbol::set_real("REAL"),
                VarSymbol::set_integer("INTEGER"),
            ];
            b.iter().for_each(|x| self.insert(Symbol::Var(x.clone())));
        }

        pub fn insert(&mut self, symbol: Symbol) {
            if self._symbols.is_empty() {
                self._symbols = HashMap::new();
            }
            self._symbols
                .insert(symbol.get_name(), Rc::new(RefCell::new(symbol)));
        }

        pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&Rc<RefCell<Symbol>>> {
            self._symbols.get(name.as_ref())
        }
    }

    impl SemanticAnalyzer {
        pub fn new() -> Self {
            Self {
                scopes: Default::default(),
                current_scope: Rc::new(RefCell::new(Default::default())),
            }
        }

        pub fn lookup_all<S: AsRef<str>>(&self, name: S) -> Option<Rc<RefCell<Symbol>>> {
            for scope in &self.scopes {
                if let Some(s) = scope.borrow()._symbols.get(name.as_ref().clone()) {
                    return Some(Rc::clone(s));
                }
            }
            None
        }

        pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<Rc<RefCell<Symbol>>> {
            self.current_scope.borrow().lookup(name).map(Rc::clone)
        }

        fn visit(&mut self, node: ASTTree) {
            match node.stat {
                Statements::Binop(_) => {
                    self.visit_binop(node);
                }
                Statements::Assign(_) => {
                    self.visit_assign(node);
                }
                Statements::Num { .. } => {
                    self.visit_num(node);
                }
                Statements::UnaryOp { .. } => {
                    self.visit_unary(node);
                }
                Statements::Compound { .. } => {
                    self.visit_compound(node);
                }
                Statements::NoOp => {
                    self.visit_noop(node);
                }
                Statements::Var { .. } => {
                    self.visit_var(node);
                }
                Statements::Program { .. } => {
                    self.visit_program(node);
                }
                Statements::Block { .. } => {
                    self.visit_block(node);
                }
                Statements::VarDecl { .. } => {
                    self.visit_var_decl(node);
                }
                Statements::ProcedureDecl { .. } => {
                    self.visit_procedure_decl(node);
                }
                Statements::Param { .. } => {
                    self.visit_params(node);
                }
                _ => {}
            }
        }

        fn visit_block(&mut self, node: ASTTree) {
            let (d, c) = node.stat.get_block();
            d.iter().for_each(|n| {
                self.visit(n.take());
            });

            self.visit(c);
        }

        fn visit_program(&mut self, node: ASTTree) {
            let mut global_scope = Rc::new(RefCell::new(ScopedSymbolTable::new("global")));

            global_scope.borrow_mut()._init_builtins();

            self.scopes.push_back(Rc::clone(&global_scope));
            self.current_scope = Rc::clone(&global_scope);

            self.visit(node.stat.get_program_block());
        }

        fn visit_params(&mut self, node: ASTTree) {
            let (v, t) = node.stat.get_params();
            self.visit(v);
            self.visit(t);
        }

        fn visit_binop(&mut self, node: ASTTree) {
            self.visit(node.left.unwrap().take());
            self.visit(node.right.unwrap().take());
        }

        fn visit_num(&mut self, node: ASTTree) {
            {
                // pass
            }
        }

        fn visit_unary(&mut self, node: ASTTree) {
            self.visit(node.stat.get_unary().1.take());
        }

        fn visit_compound(&mut self, node: ASTTree) {
            let children = node.stat.get_compound();
            children.borrow().iter().for_each(|child| {
                self.visit(child.take());
            });
        }

        fn visit_procedure_decl(&mut self, node: ASTTree) {
            let (proc_name, params, block_node) = node.stat.get_procedure_decl();
            let proc_name = proc_name.token_value;
            let mut procedure_scope = Rc::new(RefCell::new(ScopedSymbolTable::new(&proc_name)));

            self.current_scope = Rc::clone(&procedure_scope);
            let mut proc_params = vec![];

            params.iter().for_each(|param| {
                if let Statements::Param { var_name, var_type } = param.take().stat {
                    let param_name = var_name.take().stat.get_token().token_value;
                    let param_type =
                        SymbolType::from(var_type.take().stat.get_token().token_type());
                    let var_symbol = VarSymbol::new(param_name, param_type);

                    procedure_scope
                        .borrow_mut()
                        .insert(Symbol::Var(var_symbol.clone()));

                    proc_params.push(var_symbol);
                }
            });

            procedure_scope
                .borrow_mut()
                .insert(Symbol::Procedure(ProcedureSymbol {
                    procedure_name: proc_name,
                    params: proc_params,
                }));

            self.scopes.push_back(Rc::clone(&procedure_scope));

            self.visit(block_node);
        }

        fn visit_noop(&mut self, node: ASTTree) {
            {
                // pass
            }
        }

        fn visit_var_decl(&mut self, node: ASTTree) {
            let (var_name, var_type) = node.stat.get_var_decl();

            let type_name = var_name.stat.get_token().token_value;
            let type_symbol = SymbolType::from(var_type.stat.get_token().token_type);

            let var_symbol = VarSymbol::new(&type_name, type_symbol);

            if self.lookup(&type_name).is_some() {
                panic!(
                    "{}",
                    var_name.stat.get_token().error(ErrorCode::DuplicateId)
                );
            }

            self.current_scope
                .borrow_mut()
                .insert(Symbol::Var(var_symbol));
        }

        fn visit_assign(&mut self, node: ASTTree) {
            self.visit(node.left.unwrap().take());
            self.visit(node.right.unwrap().take());
        }

        fn visit_var(&mut self, node: ASTTree) {
            let var_name = node.stat.get_token().get_value();
            if self.lookup_all(var_name).is_none() {
                panic!("{}", node.stat.get_token().error(ErrorCode::IdNotFound));
            }
        }

        pub fn _visit(&mut self, node: ASTTree) -> LinkedList<Rc<RefCell<ScopedSymbolTable>>> {
            self.visit(node);
            self.scopes.clone()
        }
    }
}
