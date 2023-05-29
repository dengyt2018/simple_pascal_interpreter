#![allow(non_camel_case_types, dead_code, unused)]

use crate::error::PascalResult;
use crate::error::PascalResult::ParseError;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    RealConst(f64),
    IntegerConst(i64),
    Str(String),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::RealConst(r) => {
                write!(f, "Real: <{}>", r)
            }
            Object::IntegerConst(i) => {
                write!(f, "Integer: <{}>", i)
            }
            Object::Str(s) => {
                write!(f, "String: <{}>", s)
            }
        }
    }
}

impl Add for Object {
    type Output = Result<Self, String>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Ok(Object::RealConst(r + rhs.get_real_const())),
            Object::IntegerConst(i) => Ok(Object::IntegerConst(i + rhs.get_integer_const())),
            Object::Str(s) => Ok(Object::Str(format!("{}{}", s, rhs.get_string()))),
        }
    }
}

impl Sub for Object {
    type Output = Result<Self, String>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Ok(Object::RealConst(r - rhs.get_real_const())),
            Object::IntegerConst(i) => Ok(Object::IntegerConst(i - rhs.get_integer_const())),
            Object::Str(s) => Err("String dot not support method sub".to_string()),
        }
    }
}

impl Mul for Object {
    type Output = Result<Self, String>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Ok(Object::RealConst(r * rhs.get_real_const())),
            Object::IntegerConst(i) => Ok(Object::IntegerConst(i * rhs.get_integer_const())),
            Object::Str(s) => Err("String dot not support method mul".to_string()),
        }
    }
}

impl Div for Object {
    type Output = Result<Self, String>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Ok(Object::RealConst(r / rhs.get_real_const())),
            Object::IntegerConst(i) => Ok(Object::IntegerConst(i / rhs.get_integer_const())),
            Object::Str(_) => Err("String dot not support method div".to_string()),
        }
    }
}

impl Neg for Object {
    type Output = Result<Self, String>;

    fn neg(self) -> Self::Output {
        match self {
            Object::RealConst(r) => Ok(Object::RealConst(-r)),
            Object::IntegerConst(i) => Ok(Object::IntegerConst(-i)),
            Object::Str(_) => Err("String dot not support method neg".to_string()),
        }
    }
}

impl Rem for Object {
    type Output = Result<Self, String>;

    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Ok(Object::RealConst(r % rhs.get_real_const())),
            Object::IntegerConst(i) => Ok(Object::IntegerConst(i % rhs.get_integer_const())),
            Object::Str(s) => Err("String dot not support method rem".to_string()),
        }
    }
}

impl Object {
    pub fn get_string(&self) -> String {
        match self {
            Object::Str(s) => s.clone(),
            Object::RealConst(r) => r.to_string(),
            Object::IntegerConst(i) => i.to_string(),
        }
    }

    pub fn get_real_const(&self) -> f64 {
        match self {
            Object::RealConst(f) => *f,
            Object::IntegerConst(f) => *f as f64,
            Object::Str(s) => {
                if let Ok(v) = s.parse::<f64>() {
                    v
                } else {
                    PascalResult::error(0, 0, format!("Can not parse string '{}' to a float.", s));
                    panic!();
                }
            }
        }
    }

    pub fn get_integer_const(&self) -> i64 {
        match self {
            Object::IntegerConst(u) => *u,
            Object::RealConst(u) => *u as i64,
            Object::Str(s) => {
                if let Ok(v) = s.parse::<i64>() {
                    v
                } else {
                    PascalResult::error(0, 0, format!("Can not parse string '{}' to a float.", s));
                    panic!();
                }
            }
        }
    }
}
