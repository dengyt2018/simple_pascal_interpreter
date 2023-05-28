#![allow(non_camel_case_types, dead_code, unused)]
use crate::parser::{RefAST, Statements};
use crate::token::{ErrorCode, TokenType};
use crate::{rc, rclone};
use std::cell::RefCell;
use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::fmt::Display;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum SymbolType {
    Integer,
    Real,
    String,
}

impl From<TokenType> for SymbolType {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Integer => SymbolType::Integer,
            TokenType::String => SymbolType::String,
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
            SymbolType::String => {
                write!(f, "STRING")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarSymbol {
    pub(crate) symbol_name: String,
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

    pub fn set_string<S: AsRef<str>>(name: S) -> Self {
        Self::new(name, SymbolType::String)
    }
}

#[derive(Debug, Clone, Default)]
pub struct ProcedureSymbol {
    procedure_name: String,
    pub(crate) params: Vec<VarSymbol>,
    pub(crate) block_ast: RefAST,
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
                unreachable!();
            }
        }
    }
    pub fn get_procedure(&self) -> ProcedureSymbol {
        match self {
            Symbol::Procedure(p) => p.clone(),
            _ => {
                unreachable!();
            }
        }
    }
    pub fn set_var(var: VarSymbol) -> Symbol {
        Symbol::Var(var)
    }
}

#[derive(Debug, Default)]
pub struct ScopedSymbolTable {
    pub(crate) _symbols: HashMap<String, Rc<RefCell<Symbol>>>,
    pub(crate) scope_name: String,
    pub(crate) scope_level: usize,
}

impl Display for ScopedSymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut symbols = vec![];
        self._symbols.iter().for_each(|(name, symbol)| {
            symbols.push(format!("{}", symbol.borrow().to_string()));
        });
        write!(
            f,
            "SCOPE_NAME:<{}>\nSCOPE_LEVE:{}\n{:#?}",
            self.scope_name, self.scope_level, symbols
        )
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
            scope_level: 1,
        }
    }
    pub fn _init_builtins(&mut self) {
        let b = vec![
            VarSymbol::set_real("REAL"),
            VarSymbol::set_integer("INTEGER"),
            VarSymbol::set_string("STRING"),
        ];
        b.iter().for_each(|x| self.insert(Symbol::Var(x.clone())));
    }

    pub fn insert(&mut self, symbol: Symbol) {
        if self._symbols.is_empty() {
            self._symbols = HashMap::new();
        }
        self._symbols.insert(symbol.get_name(), rc!(symbol));
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&Rc<RefCell<Symbol>>> {
        self._symbols.get(name.as_ref())
    }

    pub fn get_symbol_type<S: AsRef<str>>(&self, name: S) -> Option<SymbolType> {
        if let Some(ty) = self._symbols.get(name.as_ref()) {
            let symbol = ty.borrow().deref().clone();
            let VarSymbol { symbol_type, .. } = symbol.get_var();
            return Some(symbol_type);
        }
        None
    }
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            scopes: Default::default(),
            current_scope: rc!(Default::default()),
        }
    }

    pub fn lookup_all<S: AsRef<str>>(&self, name: S) -> Option<Rc<RefCell<Symbol>>> {
        for scope in &self.scopes {
            if let Some(s) = scope.borrow()._symbols.get(name.as_ref().clone()) {
                return Some(rclone!(s));
            }
        }
        None
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<Rc<RefCell<Symbol>>> {
        self.current_scope.borrow().lookup(name).map(Rc::clone)
    }

    fn visit(&mut self, node: RefAST) {
        let stat = node.as_ref().borrow().stat.clone();

        match stat {
            Statements::Binop(_) => {
                self.visit_binop(rclone!(&node));
            }
            Statements::Assign(_) => {
                self.visit_assign(rclone!(&node));
            }
            Statements::UnaryOp { .. } => {
                self.visit_unary(rclone!(&node));
            }
            Statements::Compound { .. } => {
                self.visit_compound(rclone!(&node));
            }
            Statements::NoOp => {
                self.visit_noop(rclone!(&node));
            }
            Statements::Var { .. } => {
                self.visit_var(rclone!(&node));
            }
            Statements::Program { .. } => {
                self.visit_program(rclone!(&node));
            }
            Statements::Block { .. } => {
                self.visit_block(rclone!(&node));
            }
            Statements::VarDecl { .. } => {
                self.visit_var_decl(rclone!(&node));
            }
            Statements::ProcedureDecl { .. } => {
                self.visit_procedure_decl(rclone!(&node));
            }
            Statements::Param { .. } => {
                self.visit_params(rclone!(&node));
            }
            Statements::ProcedureCall { .. } => {
                self.visit_procedure_call(rclone!(&node));
            }
            _ => {}
        }
    }

    fn visit_block(&mut self, node: RefAST) {
        let (d, c) = node.borrow().stat.get_block();
        d.borrow().iter().for_each(|n| {
            self.visit(rclone!(n));
        });

        self.visit(c);
    }

    fn visit_program(&mut self, node: RefAST) {
        let global_scope = rc!(ScopedSymbolTable::new("global"));

        global_scope.borrow_mut()._init_builtins();

        self.scopes.push_back(rclone!(&global_scope));
        self.current_scope = rclone!(&global_scope);

        self.visit(node.borrow().stat.get_program_block());
    }

    fn visit_params(&mut self, node: RefAST) {
        let (v, t) = node.borrow().stat.get_params();
        self.visit(v);
        self.visit(t);
    }

    fn visit_binop(&mut self, node: RefAST) {
        if let Some(left) = node.borrow().left.clone() {
            self.visit(left);
        }

        if let Some(right) = node.borrow().right.clone() {
            self.visit(right);
        }
    }

    fn visit_procedure_call(&mut self, node: RefAST) {
        let (proc_name, actual_params) = node.borrow().stat.get_procedure_call();

        if let Some(symbol) = self.lookup(proc_name.get_value()) {
            let params_nums = symbol.borrow().get_procedure().params.len();
            if actual_params.len() != params_nums {
                panic!(
                    "Semantic {} expected {} argument(s), found {}",
                    proc_name.error(ErrorCode::WrongParamsNum),
                    params_nums,
                    actual_params.len(),
                );
            }
        }

        for param in actual_params {
            self.visit(rclone!(&param));
        }
    }

    fn visit_unary(&mut self, node: RefAST) {
        self.visit(node.borrow().stat.get_unary().1);
    }

    fn visit_compound(&mut self, node: RefAST) {
        let children = node.borrow().stat.get_compound();
        children.borrow().iter().for_each(|child| {
            self.visit(rclone!(child));
        });
    }

    fn visit_procedure_decl(&mut self, node: RefAST) {
        let (proc_name, params, block_node) = node.borrow().stat.get_procedure_decl();

        let proc_name = proc_name.token_value;
        let procedure_scope = rc!(ScopedSymbolTable::new(&proc_name));
        procedure_scope.borrow_mut().scope_level = self.current_scope.borrow().scope_level + 1;

        self.current_scope = rclone!(&procedure_scope);
        let mut proc_params = vec![];

        params.iter().for_each(|param| {
            if let Statements::Param { var_name, var_type } = param.take().stat {
                let param_name = var_name.take().stat.get_token().token_value;
                let param_type = SymbolType::from(var_type.take().stat.get_token().token_type());
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
                block_ast: rclone!(&block_node),
            }));

        self.scopes.push_back(rclone!(&procedure_scope));

        self.visit(rclone!(&block_node));
    }

    fn visit_noop(&mut self, _node: RefAST) {
        {
            // pass
        }
    }

    fn visit_var_decl(&mut self, node: RefAST) {
        let (var_name, var_type) = node.borrow().stat.get_var_decl();

        let type_name = var_name.borrow().stat.get_token().token_value;
        let type_symbol = SymbolType::from(var_type.borrow().stat.get_token().token_type);

        let var_symbol = VarSymbol::new(&type_name, type_symbol);

        if self.lookup(&type_name).is_some() {
            panic!(
                "{}",
                var_name
                    .borrow()
                    .stat
                    .get_token()
                    .error(ErrorCode::DuplicateId)
            );
        }

        self.current_scope
            .borrow_mut()
            .insert(Symbol::Var(var_symbol));
    }

    fn visit_assign(&mut self, node: RefAST) {
        if let Some(left) = node.borrow().left.clone() {
            self.visit(left);
        }

        if let Some(right) = node.borrow().right.clone() {
            self.visit(right);
        }
    }

    fn visit_var(&mut self, node: RefAST) {
        let var_name = node.borrow().stat.get_token().get_value();
        if self.lookup_all(var_name).is_none() {
            panic!(
                "{}",
                node.borrow().stat.get_token().error(ErrorCode::IdNotFound)
            );
        }
    }

    pub fn _visit(&mut self, node: RefAST) -> LinkedList<Rc<RefCell<ScopedSymbolTable>>> {
        self.visit(rclone!(&node));
        self.scopes.clone()
    }
}
