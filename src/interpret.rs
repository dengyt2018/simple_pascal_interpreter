#![allow(non_camel_case_types, dead_code, unused)]
use crate::error::PascalResult;
use crate::obj;
use crate::object::Object;
use crate::parser::{ASTTree, RefAST, Statements};
use crate::semantic::{ScopedSymbolTable, SymbolType};
use crate::token::TokenType;
use crate::{rc, rclone};
use std::cell::RefCell;
use std::collections::{HashMap, LinkedList};
use std::ops::{Deref, Neg};
use std::rc::Rc;

#[derive(Debug)]
pub enum ARType {
    Program,
    Procedure,
}

#[derive(Debug)]
pub struct ActivationRecord {
    name: String,
    record_type: ARType,
    nesting_level: usize,
    pub(crate) members: HashMap<String, Object>,
}

impl ActivationRecord {
    pub fn new() -> Self {
        Self {
            name: "".to_string(),
            record_type: ARType::Program,
            nesting_level: 1,
            members: Default::default(),
        }
    }

    pub fn set_item<S: AsRef<str>>(&mut self, key: S, value: Object) {
        self.members.insert(key.as_ref().into(), value);
    }

    pub fn get_item<S: AsRef<str>>(&self, key: S) -> Option<&Object> {
        self.members.get(key.as_ref())
    }
}

#[derive(Debug)]
pub struct CallStack {
    _records: Vec<Rc<RefCell<ActivationRecord>>>,
    pub recodes_debug: Vec<Rc<RefCell<ActivationRecord>>>,
}

impl CallStack {
    pub fn new() -> Self {
        Self {
            _records: vec![],
            recodes_debug: vec![],
        }
    }

    pub fn push(&mut self, ar: ActivationRecord) {
        let ar = rc!(ar);
        self._records.push(rclone!(&ar));
        self.recodes_debug.push(rclone!(&ar));
    }

    pub fn pop(&mut self) -> Option<Rc<RefCell<ActivationRecord>>> {
        self._records.pop()
    }

    pub fn peek(&self) -> Option<Rc<RefCell<ActivationRecord>>> {
        self._records.last().map(Rc::clone)
    }
}

pub struct Interpreter {
    parser_tree: RefAST,
    pub call_stack: CallStack,
    symbol_table: LinkedList<Rc<RefCell<ScopedSymbolTable>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            parser_tree: Default::default(),
            call_stack: CallStack::new(),
            symbol_table: Default::default(),
        }
    }

    pub fn set_symbol_table(
        mut self,
        symbol_table: LinkedList<Rc<RefCell<ScopedSymbolTable>>>,
    ) -> Interpreter {
        self.symbol_table = symbol_table;
        self
    }

    fn visit(&mut self, node: RefAST) -> Option<Object> {
        let stat = node.as_ref().borrow().stat.clone();

        match stat {
            Statements::Binop(_) => Some(self.visit_binop(rclone!(&node))),
            Statements::Assign(_) => {
                self.visit_assign(rclone!(&node));
                None
            }
            Statements::UnaryOp { .. } => Some(self.visit_unary(rclone!(&node))),
            Statements::Object { .. } => Some(self.visit_object(rclone!(&node))),
            Statements::Compound { .. } => {
                self.visit_compound(rclone!(&node));
                None
            }
            Statements::NoOp => {
                self.visit_noop(rclone!(&node));
                None
            }
            Statements::Var { .. } => Some(self.visit_var(rclone!(&node))),
            Statements::Program { .. } => {
                self.visit_program(rclone!(&node));
                None
            }
            Statements::Block { .. } => {
                self.visit_block(rclone!(&node));
                None
            }
            Statements::ProcedureDecl { .. } => {
                self.visit_procedure_decl(rclone!(&node));
                None
            }
            Statements::ProcedureCall { .. } => {
                self.visit_procedure_call(rclone!(&node));
                None
            }
            _ => None,
        }
    }

    fn visit_program(&mut self, node: RefAST) {
        if let Statements::Program { name, block } = node.borrow().stat.clone() {
            let ar = ActivationRecord {
                name,
                record_type: ARType::Program,
                nesting_level: 1,
                members: Default::default(),
            };

            self.call_stack.push(ar);

            self.visit(rclone!(&block));

            self.call_stack.pop();
        }
    }

    fn visit_block(&mut self, node: RefAST) {
        let (declarations, compound_statement) = node.borrow().stat.get_block();
        declarations.borrow().iter().for_each(|n| {
            self.visit(rclone!(n));
        });

        self.visit(rclone!(&compound_statement));
    }

    fn visit_type(&self, _node: RefAST) {
        {}
    }

    fn visit_var_decl(&self, _node: RefAST) {
        {}
    }

    fn visit_binop(&mut self, node: RefAST) -> Object {
        let left = self.visit(node.borrow().left.clone().unwrap()).unwrap();
        let right = self.visit(node.borrow().right.clone().unwrap()).unwrap();

        let token = node.borrow().stat.get_token();

        if token.token_type == TokenType::Plus {
            let r = left + right;
            obj!(r, token)
        } else if token.token_type == TokenType::Minus {
            let r = left - right;
            obj!(r, token)
        } else if token.token_type == TokenType::Mul {
            let r = left * right;
            obj!(r, token)
        } else if token.token_type == TokenType::IntegerDiv {
            let r = left / right;
            obj!(r, token)
        } else if token.token_type == TokenType::FloatDiv {
            let r = Object::RealConst(left.get_real_const())
                / Object::RealConst(right.get_real_const());
            obj!(r, token)
        } else if token.token_type == TokenType::Modulo {
            let r = left % right;
            obj!(r, token)
        } else {
            unreachable!()
        }
    }

    fn visit_unary(&mut self, node: RefAST) -> Object {
        let (token, node) = node.borrow().stat.get_unary();

        match token.token_type {
            TokenType::Minus => {
                let r = self.visit(rclone!(&node)).unwrap().neg();
                obj!(r, token)
            }
            TokenType::Plus => self.visit(rclone!(&node)).unwrap(),
            _ => {
                PascalResult::runtime_error(
                    &token,
                    format!(
                        "The Unary type is wrong, can not be a '{}'.",
                        token.token_value
                    ),
                );
                panic!();
            }
        }
    }

    fn visit_object(&mut self, node: RefAST) -> Object {
        let object = node.borrow().stat.get_object();
        let obj = object.borrow().deref().clone();
        obj
    }

    fn visit_compound(&mut self, node: RefAST) {
        let children = node.borrow().stat.get_compound();
        children.borrow().iter().for_each(|child| {
            self.visit(rclone!(child));
        });
    }

    fn visit_assign(&mut self, node: RefAST) {
        if let Some(var_name) = node.borrow().left.clone() {
            let var_name = var_name.borrow().deref().stat.get_token().token_value;

            if let Some(var) = node.borrow().right.clone() {
                if let Some(v) = self.visit(var) {
                    if let Some(ar) = self.call_stack.peek() {
                        self.symbol_table.iter().for_each(|table| {
                            let sc_lv = table.borrow().deref().scope_level;
                            let ar_lv = ar.borrow().nesting_level;

                            if sc_lv == ar_lv {
                                if let Some(ty) = table.borrow().get_symbol_type(&var_name) {
                                    match ty {
                                        SymbolType::Integer => {
                                            let value = v.get_integer_const();
                                            ar.borrow_mut()
                                                .set_item(&var_name, Object::IntegerConst(value));
                                        }
                                        SymbolType::Real => {
                                            let value = v.get_real_const();
                                            ar.borrow_mut()
                                                .set_item(&var_name, Object::RealConst(value));
                                        }
                                        SymbolType::String => {
                                            ar.borrow_mut().set_item(&var_name, v.clone());
                                        }
                                    }
                                }
                            }
                        });
                    }
                }
            }
        }
    }

    fn visit_var(&mut self, node: RefAST) -> Object {
        let var_name = node.borrow().stat.get_token().token_value;
        if let Some(ar) = self.call_stack.peek() {
            if let Some(var_value) = ar.borrow().get_item(var_name) {
                var_value.clone()
            } else {
                unreachable!();
            }
        } else {
            unreachable!();
        }
    }

    fn visit_procedure_decl(&mut self, _node: RefAST) {
        {}
    }

    fn visit_procedure_call(&mut self, node: RefAST) {
        let (proc_name, proccal) = node.borrow().stat.get_procedure_call();
        let mut ar = ActivationRecord {
            name: proc_name.get_value(),
            record_type: ARType::Procedure,
            nesting_level: Default::default(),
            members: Default::default(),
        };
        let proc_name = proc_name.token_value;
        let mut block_ast = ASTTree::default();

        self.symbol_table.clone().iter().for_each(|table| {
            if table.borrow().scope_name == proc_name {
                if let Some(proc) = table.borrow()._symbols.clone().get(&proc_name) {
                    let proc = proc.borrow().get_procedure();
                    block_ast = proc.block_ast.take();
                    proc.params.iter().zip(&proccal).for_each(|x| {
                        let num = self.visit(rclone!(x.1)).unwrap();
                        let name = x.0.symbol_name.clone();
                        ar.nesting_level = table.borrow().scope_level;
                        if let Some(symbol_type) = table.borrow().get_symbol_type(&name) {
                            match symbol_type {
                                SymbolType::Integer => {
                                    let v = num.get_integer_const();
                                    ar.set_item(name, Object::IntegerConst(v));
                                }
                                SymbolType::Real => {
                                    let v = num.get_real_const();
                                    ar.set_item(name, Object::RealConst(v));
                                }
                                SymbolType::String => {
                                    ar.set_item(name, num);
                                }
                            }
                        }
                    });
                }
            }
        });

        self.call_stack.push(ar);

        self.visit(rc!(block_ast));

        self.call_stack.pop();
    }

    fn visit_noop(&mut self, _node: RefAST) {
        {}
    }

    pub fn set_parser(mut self, parser: RefAST) -> Self {
        self.parser_tree = parser;
        self
    }

    pub fn _interpret(&mut self) {
        self.visit(rclone!(&self.parser_tree));
    }

    pub fn interpret(&mut self) {
        self._interpret();
    }
}

#[macro_export]
macro_rules! obj {
    ($ob: ident, $token: ident) => {
        match $ob {
            Ok(o) => o,
            Err(msg) => {
                PascalResult::runtime_error(&$token, msg);
                panic!()
            }
        }
    };
}
