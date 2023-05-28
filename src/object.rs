#![allow(non_camel_case_types, dead_code, unused)]

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
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Object::RealConst(r + rhs.get_real_const()),
            Object::IntegerConst(i) => Object::IntegerConst(i + rhs.get_integer_const()),
            Object::Str(s) => Object::Str(format!("{}{}", s, rhs.get_string())),
        }
    }
}

impl Sub for Object {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Object::RealConst(r - rhs.get_real_const()),
            Object::IntegerConst(i) => Object::IntegerConst(i - rhs.get_integer_const()),
            Object::Str(s) => panic!(),
        }
    }
}

impl Mul for Object {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Object::RealConst(r * rhs.get_real_const()),
            Object::IntegerConst(i) => Object::IntegerConst(i * rhs.get_integer_const()),
            Object::Str(s) => panic!(),
        }
    }
}

impl Div for Object {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Object::RealConst(r / rhs.get_real_const()),
            Object::IntegerConst(i) => Object::IntegerConst(i / rhs.get_integer_const()),
            Object::Str(s) => panic!(),
        }
    }
}

impl Neg for Object {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Object::RealConst(r) => Object::RealConst(-r),
            Object::IntegerConst(i) => Object::IntegerConst(-i),
            Object::Str(s) => panic!(),
        }
    }
}

impl Rem for Object {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Object::RealConst(r) => Object::RealConst(r % rhs.get_real_const()),
            Object::IntegerConst(i) => Object::IntegerConst(i % rhs.get_integer_const()),
            Object::Str(s) => panic!(),
        }
    }
}

impl Object {
    pub fn get_string(&self) -> String {
        match self {
            Object::Str(s) => s.clone(),
            _ => {
                unreachable!()
            }
        }
    }

    pub fn get_real_const(&self) -> f64 {
        match self {
            Object::RealConst(f) => *f,
            Object::IntegerConst(f) => *f as f64,
            _ => {
                unreachable!()
            }
        }
    }

    pub fn get_integer_const(&self) -> i64 {
        match self {
            Object::IntegerConst(u) => *u,
            Object::RealConst(u) => *u as i64,
            _ => {
                unreachable!()
            }
        }
    }
}
