use derive_more::Display;
use std::fmt;

use self::value::{Type, Value};

pub mod value;

/// The fabled Virtual Machine
pub struct VM<'chunk> {
    /// The chunk of bytecode to be executed
    chunk: &'chunk Chunk,
    /// The instruction pointer, which points to the next instruction in `self.chunk.code`
    ip: usize,
    /// The stack, stores all temporary values
    stack: Vec<Value>,
}

/// All bytecode operations
#[derive(Debug, Display, Clone)]
pub enum Op {
    /// Load constant at given index onto the stack
    #[display(fmt = "LOAD {:016x}", _0)]
    Load(usize),

    /// Pop a value from the stack and then print it
    #[display(fmt = "PRINT")]
    Print,

    /// Return from current function, or, end execution
    #[display(fmt = "RET")]
    Return,

    /* ARITHMETIC OPERATORS */
    #[display(fmt = "ADD")]
    Add,
    #[display(fmt = "SUB")]
    Sub,
    #[display(fmt = "MUL")]
    Mul,
    #[display(fmt = "DIV")]
    Div,

    /* COMPARISON OPERATORS */
    #[display(fmt = "LT")]
    Less,
    #[display(fmt = "LEQ")]
    LessEq,
    #[display(fmt = "GT")]
    Greater,
    #[display(fmt = "GEQ")]
    GreatEq,
    #[display(fmt = "EQ")]
    Eq,
    #[display(fmt = "NEQ")]
    NotEq,

    /* UNARY OPERATORS */
    #[display(fmt = "NOT")]
    Not,
    #[display(fmt = "NEG")]
    Negate,

    /* BOOLEAN OPERATORS */
    #[display(fmt = "AND")]
    And,
    #[display(fmt = "OR")]
    Or,
}

/// A chunk of bytecode with associated constants and line information
#[derive(Debug, Display, Default, Clone)]
#[display(
    fmt = "consts = [\n{}\n]\n\ncode = {{\n{}\n}}",
    "fmt_consts(consts)",
    "fmt_bytecode(code, lines)"
)]
pub struct Chunk {
    pub consts: Vec<Value>,
    pub lines: Vec<usize>,
    pub code: Vec<Op>,
}

impl Chunk {
    /// Add a constant value to the chunk
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.consts.push(value);
        self.consts.len() - 1
    }

    /// Add a bytecode operation to the chunk
    pub fn add_op(&mut self, op: Op, line: usize) {
        self.code.push(op);
        self.lines.push(line);
    }
}

#[derive(Debug, Display, Clone)]
pub enum VMError {
    #[display(fmt = "Could not perform '{op}' on {lhs} and {rhs}")]
    CannotPerform {
        op: &'static str,
        lhs: Type,
        rhs: Type,
    },
    #[display(fmt = "Expected {expected}, got {got}")]
    IncorrectType { expected: Type, got: Type },
}

pub type VMResult<T> = Result<T, VMError>;

impl<'chunk> VM<'chunk> {
    /// Create a new `VM` which will execute `chunk`
    pub fn new(chunk: &'chunk Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: vec![],
        }
    }

    /// Execute `self.chunk`
    pub fn exec(&mut self) -> VMResult<Value> {
        macro_rules! binop {
            ($op:tt) => {{
                let rhs = self.pop();
                let lhs = self.pop();
                self.stack.push((lhs $op rhs)?);
            }};
        }

        macro_rules! binop_cmp {
            ($op:tt) => {{
                let rhs = self.pop();
                let lhs = self.pop();
                self.stack.push(Value::Bool(lhs $op rhs));
            }};
        }

        while self.ip < self.chunk.code.len() {
            match &self.chunk.code[self.ip] {
                Op::Load(idx) => self.stack.push(self.chunk.consts[*idx].clone()),
                Op::Print => println!("{}", self.pop()),
                Op::Return => break,

                Op::Add => binop!(+),
                Op::Sub => binop!(-),
                Op::Mul => binop!(*),
                Op::Div => binop!(/),

                Op::Less => binop_cmp!(<),
                Op::LessEq => binop_cmp!(<=),
                Op::Greater => binop_cmp!(>),
                Op::GreatEq => binop_cmp!(>=),
                Op::Eq => binop_cmp!(==),
                Op::NotEq => binop_cmp!(!=),

                Op::Not => {
                    let operand = self.pop();
                    self.stack.push((!operand)?);
                }
                Op::Negate => {
                    let operand = self.pop();
                    self.stack.push((-operand)?)
                }

                Op::And => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.stack.push(Value::Bool(lhs.into() && rhs.into()));
                }
                Op::Or => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.stack.push(Value::Bool(lhs.into() || rhs.into()));
                }
            }
            {
                println!("stack = [\n{}\n]\n", fmt_stack(&self.stack));
            }
            self.ip += 1;
        }

        Ok(self.stack.pop().unwrap_or(Value::Unit))
    }

    /// Pop value or panic
    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack is empty :(")
    }
}

fn fmt_stack(s: &[Value]) -> String {
    s.iter()
        .map(|i| format!("    {i}"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn fmt_consts(i: &[Value]) -> String {
    i.iter()
        .enumerate()
        .map(|(idx, item)| format!("{idx:016x}    {item}"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn fmt_bytecode(code: &[Op], lines: &[usize]) -> String {
    code.iter()
        .enumerate()
        .zip(lines)
        .map(|((idx, op), line)| format!("{idx:016x}    {} @ {line}", op))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn test() {
    let chunk = Chunk {
        consts: vec![Value::Num(1.0), Value::Num(2.0), Value::Num(3.0)],
        lines: vec![1, 1, 1, 1, 1, 1],
        code: vec![
            Op::Load(0),
            Op::Load(1),
            Op::Mul,
            Op::Load(2),
            Op::Div,
            Op::Print,
        ],
    };
    println!("{chunk}\n");

    let mut vm = VM::new(&chunk);
    vm.exec().unwrap();
}
