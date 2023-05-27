#![allow(non_camel_case_types, dead_code, unused)]

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    RealConst(f64),
    IntegerConst(u64),
    Identifier(String),
}

impl Object {
    pub fn get_string(&self) -> String {
        match self {
            Object::Identifier(s) => s.clone(),
            _ => {
                unreachable!()
            }
        }
    }

    pub fn get_real_const(&self) -> f64 {
        match self {
            Object::RealConst(f) => *f,
            _ => {
                unreachable!()
            }
        }
    }

    pub fn get_integer_const(&self) -> u64 {
        match self {
            Object::IntegerConst(u) => *u,
            _ => {
                unreachable!()
            }
        }
    }
}
