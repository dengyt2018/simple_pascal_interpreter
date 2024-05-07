#![allow(non_camel_case_types, dead_code, unused)]

use crate::error::PascalResult;
use crate::object::Object;
use crate::token::*;
use crate::set_token;
use std::cell::RefCell;
use std::rc::Rc;

pub type RefAST = Rc<RefCell<ASTTree>>;
pub type RefASTS = Rc<RefCell<Vec<RefAST>>>;


#[macro_export]
macro_rules! rclone {
    ($T: expr) => {{
        Rc::clone($T)
    }};
}

#[macro_export]
macro_rules! rc {
    ($T: expr) => {{
        Rc::new(RefCell::new($T))
    }};
}

#[macro_export]
macro_rules! match_token {
    ($self:ident, $token_type: expr) => {{
        $self.tokens[$self.current_pos].token_type == $token_type
    }};
}

#[derive(Debug, Clone)]
pub enum Statements {
    Type(Token),
    Token(Token),
    Binop(Token),
    Assign(Token),
    Var(Token),
    NoOp,
    Object {
        value: Rc<RefCell<Object>>,
    },
    UnaryOp {
        token: Token,
        expr: RefAST,
    },
    Program {
        name: String,
        block: RefAST,
    },
    Param {
        var_name: RefAST,
        var_type: RefAST,
    },
    Compound {
        children: RefASTS,
    },
    Block {
        declaration: RefASTS,
        compound_statement: RefAST,
    },

    VarDecl {
        var_node: RefAST,
        type_node: RefAST,
    },
    ProcedureDecl {
        proc_name: Token,
        params: Vec<RefAST>,
        block_node: RefAST,
    },
    ProcedureCall {
        proc_name: Token,
        actual_params: Vec<RefAST>,
    },
}

impl Statements {
    pub fn get_object(&self) -> Rc<RefCell<Object>> {
        match self {
            Statements::Object { value } => rclone!(value),
            _ => {
                unreachable!();
            }
        }
    }

    pub fn get_unary(&self) -> (Token, RefAST) {
        match self {
            Statements::UnaryOp { token, expr } => (token.clone(), rclone!(expr)),
            _ => {
                unreachable!();
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
                unreachable!();
            }
        }
    }

    pub fn get_compound(&self) -> RefASTS {
        match self {
            Statements::Compound { children } => {
                rclone!(children)
            }
            _ => {
                unreachable!();
            }
        }
    }

    pub fn get_program_block(&self) -> RefAST {
        match self {
            Statements::Program { block, .. } => {
                rclone!(block)
            }
            _ => {
                unreachable!();
            }
        }
    }

    pub fn get_params(&self) -> (RefAST, RefAST) {
        match self {
            Statements::Param {
                var_name: var_node,
                var_type: type_node,
            } => (rclone!(var_node), rclone!(type_node)),
            _ => {
                unreachable!();
            }
        }
    }

    pub fn get_block(&self) -> (RefASTS, RefAST) {
        match self {
            Statements::Block {
                declaration,
                compound_statement,
            } => (rclone!(declaration), rclone!(compound_statement)),
            _ => {
                unreachable!();
            }
        }
    }

    pub fn get_var_decl(&self) -> (RefAST, RefAST) {
        match self {
            Statements::VarDecl {
                var_node,
                type_node,
            } => (rclone!(var_node), rclone!(type_node)),
            _ => {
                unreachable!();
            }
        }
    }

    pub fn get_procedure_decl(&self) -> (Token, Vec<RefAST>, RefAST) {
        match self {
            Statements::ProcedureDecl {
                proc_name,
                params,
                block_node,
            } => (proc_name.clone(), params.clone(), rclone!(block_node)),
            _ => {
                unreachable!();
            }
        }
    }

    pub fn get_procedure_call(&self) -> (Token, Vec<RefAST>) {
        match self {
            Statements::ProcedureCall {
                proc_name,
                actual_params,
            } => (proc_name.clone(), actual_params.clone()),
            _ => {
                unreachable!();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTTree {
    pub left: Option<RefAST>,
    pub(crate) stat: Statements,
    pub right: Option<RefAST>,
    // for ASTVisualizer
    pub _num: usize,
}

impl ASTTree {
    pub fn new(left: ASTTree, stat: Statements, right: ASTTree) -> Rc<RefCell<Self>> {
        let node = Self {
            left: Some(rc!(left)),
            stat,
            right: Some(rc!(right)),
            _num: 0,
        };
        rc!(node)
    }

    #[inline]
    pub fn set_stat(&mut self, stat: Statements) {
        self.stat = stat;
    }

    #[inline]
    pub fn var(token: Token) -> Self {
        let mut node = ASTTree::default();
        node.set_stat(Statements::Var(token));
        node
    }

    #[inline]
    pub fn object(object: Object) -> Self {
        let mut node = ASTTree::default();
        node.set_stat(Statements::Object { value: rc!(object) });
        node
    }

    #[inline]
    pub fn unary(token: Token, expr: ASTTree) -> Self {
        let mut node = ASTTree::default();
        node.set_stat(Statements::UnaryOp {
            token,
            expr: rc!(expr),
        });
        node
    }

    #[inline]
    pub fn program<S: AsRef<str>>(name: S, block: ASTTree) -> Self {
        let mut node = ASTTree::default();
        node.set_stat(Statements::Program {
            name: name.as_ref().into(),
            block: rc!(block),
        });
        node
    }

    #[inline]
    pub fn compound(children: Vec<RefAST>) -> Self {
        let mut node = ASTTree::default();
        node.set_stat(Statements::Compound {
            children: rc!(children),
        });
        node
    }

    #[inline]
    pub fn block(declaration: Vec<RefAST>, compound_statement: ASTTree) -> Self {
        let mut node = ASTTree::default();
        node.set_stat(Statements::Block {
            declaration: rc!(declaration),
            compound_statement: rc!(compound_statement),
        });
        node
    }

    #[inline]
    pub fn var_decl(var_node: ASTTree, type_node: ASTTree) -> Rc<RefCell<Self>> {
        let mut node = ASTTree::default();
        node.set_stat(Statements::VarDecl {
            var_node: rc!(var_node),
            type_node: rc!(type_node),
        });

        rc!(node)
    }

    #[inline]
    pub fn param(var_node: ASTTree, type_node: ASTTree) -> Rc<RefCell<Self>> {
        let mut node = ASTTree::default();
        node.set_stat(Statements::Param {
            var_name: rc!(var_node),
            var_type: rc!(type_node),
        });

        rc!(node)
    }

    #[inline]
    pub fn procedure_decl(
        proc_name: Token,
        params: Vec<RefAST>,
        block_node: ASTTree,
    ) -> Rc<RefCell<Self>> {
        let mut node = ASTTree::default();
        node.set_stat(Statements::ProcedureDecl {
            proc_name,
            params,
            block_node: rc!(block_node),
        });
        rc!(node)
    }

    #[inline]
    pub fn proccall(proc_name: Token, actual_params: Vec<RefAST>) -> Self {
        let mut node = ASTTree::default();
        node.set_stat(Statements::ProcedureCall {
            proc_name,
            actual_params,
        });
        node
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

#[derive(Default)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current_pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new<T: AsRef<[Token]>>(tokens: &'a T) -> Self {
        Self {
            tokens: tokens.as_ref(),
            current_pos: 0,
        }
    }

    fn peek(&self) -> Token {
        if !self.is_at_end() {
            self.tokens[self.current_pos + 1].clone()
        } else {
            set_token!(TokenType::Eof, "EOF")
        }
    }

    fn is_at_end(&self) -> bool {
        match_token!(self, TokenType::Eof)
    }
    ///
    /// ```
    ///    compare the current token type with the passed token
    ///    type and if they match then "eat" the current token
    ///    and assign the next token to the self.current_token,
    ///    otherwise raise an exception.
    /// ```
    ///
    fn eat(&mut self, token_type: TokenType) {
        if match_token!(self, token_type) {
            self.current_pos += 1;
        } else {
            let token = self.tokens[self.current_pos].clone();
            PascalResult::parse_error(
                &token,
                format!(
                    "Parser {} Expected: {:#?}",
                    token.error(ErrorCode::UnexpectedToken),
                    token_type
                ),
            );
        }
    }

    ///
    /// ```
    ///    program : PROGRAM variable SEMI block DOT
    /// ```
    ///
    fn program(&mut self) -> ASTTree {
        self.eat(TokenType::Program);
        let var_node = self.variable().stat.get_token().get_value();
        self.eat(TokenType::Semi);
        let block_node = self.block();
        self.eat(TokenType::Dot);
        ASTTree::program(var_node, block_node)
    }

    ///
    /// ```
    ///    block : declarations compound_statement
    /// ```
    ///
    fn block(&mut self) -> ASTTree {
        ASTTree::block(self.declarations(), self.compound_statement())
    }

    ///
    /// ```
    ///    declarations : (VAR (variable_declaration SEMI)+)*
    ///        | (PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI)*
    ///        | empty
    /// ```
    fn declarations(&mut self) -> Vec<RefAST> {
        let mut declarations = vec![];

        if match_token!(self, TokenType::Var) {
            self.eat(TokenType::Var);
            while match_token!(self, TokenType::Id) {
                let var_decl = self.variable_declaration();

                declarations.extend(var_decl);

                self.eat(TokenType::Semi);
            }
        }

        loop {
            match self.tokens[self.current_pos].token_type {
                TokenType::Var => {
                    self.eat(TokenType::Var);
                    while match_token!(self, TokenType::Id) {
                        let var_decl = self.variable_declaration();
                        declarations.extend(var_decl);
                        self.eat(TokenType::Semi);
                    }
                }
                TokenType::Procedure => {
                    self.eat(TokenType::Procedure);
                    let proc_name = self.tokens[self.current_pos].clone();
                    self.eat(TokenType::Id);
                    let mut params = vec![];

                    if match_token!(self, TokenType::Lbrack) {
                        self.eat(TokenType::Lbrack);
                        if let Some(parm) = self.formal_parameter_list() {
                            params.extend(parm);
                        }
                        self.eat(TokenType::Rbrack);
                    }

                    self.eat(TokenType::Semi);
                    let block_node = self.block();
                    let proc_decl = ASTTree::procedure_decl(proc_name, params, block_node);
                    declarations.push(proc_decl);
                    self.eat(TokenType::Semi);
                }
                _ => break,
            }
        }

        declarations
    }

    ///
    /// ```
    ///    proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN
    /// ```
    ///
    fn proccall_statement(&mut self) -> ASTTree {
        let proc_name = self.tokens[self.current_pos].clone();

        self.eat(TokenType::Id);
        self.eat(TokenType::Lbrack);

        let mut actual_params = vec![];

        if !match_token!(self, TokenType::Rbrack) {
            let node = self.expr();
            actual_params.push(rc!(node));
        }

        while match_token!(self, TokenType::Comma) {
            self.eat(TokenType::Comma);
            let node = self.expr();
            actual_params.push(rc!(node));
        }

        self.eat(TokenType::Rbrack);

        ASTTree::proccall(proc_name, actual_params)
    }

    ///
    /// ```
    ///    formal_parameters : ID (COMMA ID)* COLON type_spec
    /// ```
    ///
    fn formal_parameters(&mut self) -> Vec<RefAST> {
        let mut param_nodes = vec![];

        let mut param_tokens = vec![ASTTree::var(self.tokens[self.current_pos].clone())];
        self.eat(TokenType::Id);
        while match_token!(self, TokenType::Comma) {
            self.eat(TokenType::Comma);
            param_tokens.push(ASTTree::var(self.tokens[self.current_pos].clone()));
            self.eat(TokenType::Id);
        }

        self.eat(TokenType::Colon);
        let var_type = self.type_spec();
        param_tokens.iter().for_each(|var_name| {
            let param_node = ASTTree::param(var_name.clone(), var_type.clone());
            param_nodes.push(param_node);
        });

        param_nodes
    }

    ///
    /// ```
    ///    formal_parameter_list : formal_parameters
    ///                | formal_parameters SEMI formal_parameter_list
    /// ```
    ///
    fn formal_parameter_list(&mut self) -> Option<Vec<RefAST>> {
        if !match_token!(self, TokenType::Id) {
            return None;
        }

        let mut param_nodes = self.formal_parameters();

        while match_token!(self, TokenType::Semi) {
            self.eat(TokenType::Semi);
            param_nodes.extend(self.formal_parameters());
        }

        Some(param_nodes)
    }

    ///
    /// ```
    ///    variable_declaration : ID (COMMA ID)* COLON type_spec
    /// ```
    ///
    fn variable_declaration(&mut self) -> Vec<RefAST> {
        // first ID
        let mut var_nodes = vec![ASTTree::var(self.tokens[self.current_pos].clone())];
        self.eat(TokenType::Id);

        while match_token!(self, TokenType::Comma) {
            self.eat(TokenType::Comma);

            var_nodes.push(ASTTree::var(self.tokens[self.current_pos].clone()));

            self.eat(TokenType::Id);
        }

        self.eat(TokenType::Colon);

        let type_node = self.type_spec();
        let mut var_declarations = vec![];

        var_nodes.iter().for_each(|var_node| {
            let var_node = var_node.clone();
            let var_decl = ASTTree::var_decl(var_node, type_node.clone());

            var_declarations.push(var_decl);
        });

        var_declarations
    }

    ///
    /// ```
    ///    type_spec : INTEGER
    ///            | REAL
    /// ```
    ///
    fn type_spec(&mut self) -> ASTTree {
        let token = self.tokens[self.current_pos].clone();
        if token.token_type == TokenType::Integer {
            self.eat(TokenType::Integer);
        } else if token.token_type == TokenType::Real {
            self.eat(TokenType::Real);
        } else if token.token_type == TokenType::String {
            self.eat(TokenType::String);
        }

        let mut node = ASTTree::default();
        node.set_stat(Statements::Type(token));

        node
    }

    ///
    /// ```
    ///    compound_statement: BEGIN statement_list END
    /// ```
    ///
    fn compound_statement(&mut self) -> ASTTree {
        self.eat(TokenType::Begin);
        let nodes = self.statement_list();
        self.eat(TokenType::End);

        let mut root = vec![];

        nodes.iter().for_each(|node| {
            let child = rc!(node.clone());
            root.push(rclone!(&child));
        });

        ASTTree::compound(root)
    }

    ///
    /// ```
    ///    statement_list : statement
    ///                | statement SEMI statement_list
    /// ```
    ///
    fn statement_list(&mut self) -> Vec<ASTTree> {
        let node = self.statement();

        let mut results = vec![node];

        while match_token!(self, TokenType::Semi) {
            self.eat(TokenType::Semi);
            results.push(self.statement())
        }
        results
    }

    ///
    /// ```
    ///    statement : compound_statement
    ///            | proccall_statement
    ///            | assignment_statement
    ///            | empty
    /// ```
    ///
    fn statement(&mut self) -> ASTTree {
        if match_token!(self, TokenType::Begin) {
            self.compound_statement()
        } else if match_token!(self, TokenType::Id) && self.peek().token_type == TokenType::Lbrack {
            self.proccall_statement()
        } else if match_token!(self, TokenType::Id) {
            self.assignment_statement()
        } else {
            self.empty()
        }
    }

    ///
    /// ```
    ///    assignment_statement : variable ASSIGN expr
    /// ```
    ///
    fn assignment_statement(&mut self) -> ASTTree {
        let left = self.variable();
        let token = self.tokens[self.current_pos].clone();
        let peek = self.peek();

        self.eat(TokenType::Assign);

        ASTTree::new(left, Statements::Assign(token), self.expr()).take()
    }

    ///
    /// ```
    ///    variable : ID
    /// ```
    ///
    fn variable(&mut self) -> ASTTree {
        let var_name = self.tokens[self.current_pos].clone();
        self.eat(TokenType::Id);

        ASTTree::var(var_name)
    }

    ///
    /// ```
    ///    An empty production
    /// ```
    ///
    fn empty(&self) -> ASTTree {
        ASTTree::default()
    }

    ///
    /// ```
    ///    factor : PLUS factor
    ///            | MINUS factor
    ///            | INTEGER
    ///            | LPAREN expr RPAREN
    ///            | variable
    /// ```
    ///
    fn factor(&mut self) -> ASTTree {
        let token = self.tokens[self.current_pos].clone();

        match token.token_type {
            TokenType::Plus => {
                self.eat(TokenType::Plus);
                ASTTree::unary(token, self.factor())
            }
            TokenType::Minus => {
                self.eat(TokenType::Minus);
                ASTTree::unary(token, self.factor())
            }
            TokenType::IntegerConst => {
                self.eat(TokenType::IntegerConst);
                ASTTree::object(token.literal.unwrap())
            }
            TokenType::RealConst => {
                self.eat(TokenType::RealConst);
                ASTTree::object(token.literal.unwrap())
            }
            TokenType::String => {
                self.eat(TokenType::String);
                ASTTree::object(token.literal.unwrap())
            }
            TokenType::Lbrack => {
                self.eat(TokenType::Lbrack);
                let node = self.expr();
                self.eat(TokenType::Rbrack);
                node
            }
            _ => self.variable(),
        }
    }

    ///
    /// ```
    ///    term : factor ((MUL | DIV) factor)*
    /// ```
    ///
    fn term(&mut self) -> ASTTree {
        let mut node = self.factor();
        while match_token!(self, TokenType::Mul)
            || match_token!(self, TokenType::IntegerDiv)
            || match_token!(self, TokenType::FloatDiv)
            || match_token!(self, TokenType::Modulo)
        {
            let token = self.tokens[self.current_pos].clone();
            if token.token_type == TokenType::Mul {
                self.eat(TokenType::Mul);
            } else if token.token_type == TokenType::IntegerDiv {
                self.eat(TokenType::IntegerDiv)
            } else if token.token_type == TokenType::FloatDiv {
                self.eat(TokenType::FloatDiv)
            } else if token.token_type == TokenType::Modulo {
                self.eat(TokenType::Modulo)
            }

            node = ASTTree::new(node, Statements::Binop(token), self.factor()).take()
        }
        node
    }

    ///
    /// ```
    ///    expr : term ((PLUS | MINUS) term)*
    /// ```
    ///
    fn expr(&mut self) -> ASTTree {
        let mut node = self.term();
        while match_token!(self, TokenType::Plus) || match_token!(self, TokenType::Minus) {
            let token = self.tokens[self.current_pos].clone();
            if token.token_type == TokenType::Plus {
                self.eat(TokenType::Plus);
            } else if token.token_type == TokenType::Minus {
                self.eat(TokenType::Minus);
            }
            node = ASTTree::new(node, Statements::Binop(token), self.term()).take()
        }
        node
    }

    /// ```
    ///    program : PROGRAM variable SEMI block DOT
    ///
    ///    block : declarations compound_statement
    ///
    ///    declarations : (VAR (variable_declaration SEMI)+)? procedure_declaration*
    ///
    ///    variable_declaration : ID (COMMA ID)* COLON type_spec
    ///
    ///    procedure_declaration :
    ///        PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI
    ///
    ///    formal_params_list : formal_parameters
    ///                    | formal_parameters SEMI formal_parameter_list
    ///
    ///    formal_parameters : ID (COMMA ID)* COLON type_spec
    ///
    ///    type_spec : INTEGER | REAL
    ///
    ///    compound_statement : BEGIN statement_list END
    ///
    ///    statement_list : statement
    ///                    | statement SEMI statement_list
    ///
    ///    statement : compound_statement
    ///                | proccall_statement
    ///                | assignment_statement
    ///                | empty
    ///
    ///    proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN
    ///
    ///    assignment_statement : variable ASSIGN expr
    ///
    ///    empty :
    ///
    ///    expr : term ((PLUS | MINUS) term)*
    ///
    ///    term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
    ///
    ///    factor : PLUS factor
    ///            | MINUS factor
    ///            | INTEGER_CONST
    ///            | REAL_CONST
    ///            | LPAREN expr RPAREN
    ///            | variable
    ///
    ///    variable: ID
    /// ```
    ///
    pub fn parser(&mut self) -> ASTTree {
        let node = self.program();

        if !self.is_at_end() {
            let token = self.tokens[self.current_pos].clone();
            PascalResult::parse_error(
                &token,
                format!("Parser {}", token.error(ErrorCode::UnexpectedToken), ),
            );
        }
        node
    }
}


