use peng_parser::location::position::{Located, Position};

use super::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ByteCode {
    None, Halt,
    Jump, JumpIf, JumpIfNot,
    Call, CallReturn, Return,
    Break(usize), Continue(usize),
    Pointer(usize),
    ConstPointer(ConstPointer),
    Load, Store,
    Field, Index,

    Add, Sub, Mul, Div, Mod, Pow,
    EQ, NE, LT, GT, LE, GE, And, Or, In,
    Neg, Not
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstPointer {
    Int(usize), Float(usize), Bool(bool),
    Char(char), String(usize),
    Vector(usize), Object(usize),
    Option(usize), Result(usize),
    Type(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CodeAddr(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncAddr(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarAddr(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// `ConstAddr(row, addr)`
pub struct ConstAddr(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Code {
    pub code: Vec<Located<ByteCode>>,
    pub funcs: Vec<CodeAddr>,
    pub consts: (Vec<i64>, Vec<f64>, Vec<String>, Vec<Type>),
}
impl Code {
    pub fn new() -> Self {
        Self {
            code: vec![],
            funcs: vec![],
            consts: (vec![], vec![], vec![], vec![]),
        }
    }
    pub fn len(&self) -> usize {
        self.code.len()
    }
    pub fn push(&mut self, bytecode: ByteCode, pos: Position) {
        self.code.push(Located::new(bytecode, pos));
    }
    pub fn overwrite(&mut self, code_addr: CodeAddr, bytecode: ByteCode) {
        let Located { item: _, pos } = self.code.remove(code_addr.0);
        self.code.insert(code_addr.0, Located::new(bytecode, pos));
    }
    pub fn func(&mut self, code_addr: CodeAddr) -> FuncAddr {
        let addr = self.funcs.len();
        self.funcs.push(code_addr);
        FuncAddr(addr)
    }
    pub fn int(&mut self, value: i64) -> ConstAddr {
        let addr = self.consts.0.len();
        self.consts.0.push(value);
        ConstAddr(0, addr)
    }
    pub fn float(&mut self, value: f64) -> ConstAddr {
        let addr = self.consts.1.len();
        self.consts.1.push(value);
        ConstAddr(1, addr)
    }
    pub fn string(&mut self, value: String) -> ConstAddr {
        let addr = self.consts.2.len();
        self.consts.2.push(value);
        ConstAddr(2, addr)
    }
    pub fn type_(&mut self, value: Type) -> ConstAddr {
        let addr = self.consts.3.len();
        self.consts.3.push(value);
        ConstAddr(3, addr)
    }
}