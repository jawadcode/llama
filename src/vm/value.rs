use std::ops::{Add, Div, Mul, Neg, Not, Sub};

use derive_more::Display;

use super::{VMError, VMResult};

/// All values passed around in the VM will be of this type
#[derive(Debug, Display, Clone, PartialEq)]
pub enum Value {
    #[display(fmt = "{}", _0)]
    Str(String),
    #[display(fmt = "{}", _0)]
    Num(f64),
    #[display(fmt = "{}", _0)]
    Bool(bool),
    #[display(fmt = "()")]
    Unit,
}

/// The types that correspond with the different variants of `Value`
#[derive(Debug, Display, Clone)]
pub enum Type {
    #[display(fmt = "string")]
    Str,
    #[display(fmt = "number")]
    Num,
    #[display(fmt = "boolean")]
    Bool,
    #[display(fmt = "unit type")]
    Unit,
}

impl From<&Value> for Type {
    fn from(value: &Value) -> Self {
        match value {
            Value::Str(_) => Type::Str,
            Value::Num(_) => Type::Num,
            Value::Bool(_) => Type::Bool,
            Value::Unit => Type::Unit,
        }
    }
}

type ValueResult = Result<Value, VMError>;

impl Neg for Value {
    type Output = ValueResult;

    fn neg(self) -> Self::Output {
        match self {
            Value::Num(num) => Ok(Value::Num(-num)),
            _ => Err(VMError::IncorrectType {
                expected: Type::Num,
                got: Type::from(&self),
            }),
        }
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        match value {
            Value::Str(s) => !s.is_empty(),
            Value::Num(num) => num != 0.0,
            Value::Bool(b) => b,
            Value::Unit => true,
        }
    }
}

impl Not for Value {
    type Output = ValueResult;

    fn not(self) -> Self::Output {
        Ok(Value::Bool(!bool::from(self)))
    }
}

impl From<Value> for VMResult<f64> {
    fn from(value: Value) -> Self {
        match value {
            Value::Num(num) => Ok(num),
            _ => Err(VMError::IncorrectType {
                expected: Type::Num,
                got: (&value).into(),
            }),
        }
    }
}

impl From<Value> for VMResult<bool> {
    fn from(value: Value) -> Self {
        match value {
            Value::Bool(b) => Ok(b),
            _ => Err(VMError::IncorrectType {
                expected: Type::Bool,
                got: (&value).into(),
            }),
        }
    }
}

impl From<Value> for VMResult<String> {
    fn from(value: Value) -> Self {
        match value {
            Value::Str(s) => Ok(s),
            _ => Err(VMError::IncorrectType {
                expected: Type::Str,
                got: (&value).into(),
            }),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.partial_cmp(b),
            (Value::Str(a), Value::Str(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

macro_rules! cannot_perform {
    ($lhs:expr, $rhs:expr, $op:expr) => {
        Err(VMError::CannotPerform {
            op: $op,
            lhs: Type::from($lhs),
            rhs: Type::from($rhs),
        })
    };
}

impl Add for Value {
    type Output = ValueResult;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
            (Value::Str(a), Value::Str(b)) => Ok(Value::Str(a + &b)),
            (a, b) => cannot_perform!(&a, &b, "+"),
        }
    }
}

impl Sub for Value {
    type Output = ValueResult;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a - b)),
            (a, b) => cannot_perform!(&a, &b, "-"),
        }
    }
}

impl Mul for Value {
    type Output = ValueResult;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a * b)),
            (Value::Str(a), Value::Num(b)) => Ok(Value::Str(a.repeat(b as usize))),
            (a, b) => cannot_perform!(&a, &b, "*"),
        }
    }
}

impl Div for Value {
    type Output = ValueResult;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a / b)),
            (a, b) => cannot_perform!(&a, &b, "/"),
        }
    }
}
