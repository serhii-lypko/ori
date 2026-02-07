use std::fmt;
use std::ops::{Add, Sub};

mod bytecode;
mod op_code;
mod vm;

pub(crate) use vm::VM;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ConstValue {
    Int(i64),
    // Long, Float etc.
}

impl Add<ConstValue> for ConstValue {
    type Output = Self;

    fn add(self, rhs: ConstValue) -> Self::Output {
        match (self, rhs) {
            (ConstValue::Int(l_val), ConstValue::Int(r_val)) => ConstValue::Int(l_val + r_val),
            // TODO -> rest of invariants
        }
    }
}

impl Sub<ConstValue> for ConstValue {
    type Output = Self;

    fn sub(self, rhs: ConstValue) -> Self::Output {
        match (self, rhs) {
            (ConstValue::Int(l_val), ConstValue::Int(r_val)) => ConstValue::Int(r_val - l_val),
            // TODO -> rest of invariants
        }
    }
}

impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
        // write!(f, "({}, {})", self.x)
    }
}
