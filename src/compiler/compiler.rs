use std::{collections::{HashSet, HashMap}, fmt::Display};

use crate::*;
use peng_parser::{location::{path::FilePath, position::{Located, Position}}, parser::ast::*, error::Error};

use super::{bytecode::*, types::Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ScopeKind {
    #[default]
    Code,
    Loop,
    Function
}
impl Display for ScopeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeKind::Code => write!(f, "code"),
            ScopeKind::Loop => write!(f, "loop"),
            ScopeKind::Function => write!(f, "function"),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    typ: Type,
    var_addr: VarAddr
}
#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub path: FilePath,
    pub kind: ScopeKind,
    pub vars: HashMap<String, Variable>,
    pub breaks: Vec<CodeAddr>
}
impl Scope {
    pub fn new(path: FilePath, kind: ScopeKind) -> Self {
        Scope { path, kind, vars: HashMap::new(), breaks: vec![] }
    }
    pub fn get(&self, id: &String) -> Option<&Variable> {
        self.vars.get(id)
    }
    pub fn set(&mut self, id: String, variable: Variable) -> Option<Variable> {
        self.vars.insert(id, variable)
    }
    pub fn break_addr(&mut self, code_addr: CodeAddr, pos: Position) -> Result<(), Error> {
        if self.kind != ScopeKind::Loop {
            return Err(Error::new("cannot break out of ", self.path.clone(), Some(pos)))
        }
        self.breaks.push(code_addr);
        Ok(())
    }
}

pub struct Compiler {
    pub path: FilePath,
    pub code: Code,
    pub scopes: Vec<(CodeAddr, Scope)>,
    pub addrs: HashSet<VarAddr>
}
impl Compiler {
    pub fn new(path: &FilePath) -> Self {
        Self {
            path: path.clone(),
            code: Code::new(),
            scopes: vec![(CodeAddr(0), Scope::new(path.clone(), ScopeKind::Function))],
            addrs: HashSet::new()
        }
    }

    pub fn new_addr(&mut self) -> VarAddr {
        let mut addr = 0;
        while self.addrs.contains(&VarAddr(addr)) {
            addr += 1;
        }
        VarAddr(addr)
    }

    pub fn push_scope(&mut self, code_addr: CodeAddr, kind: ScopeKind) {
        self.scopes.push((code_addr, Scope::new(self.path.clone(), kind)))
    }
    pub fn pop_scope(&mut self) {
        if let Some((_, scope)) = self.scopes.pop() {
            for (_, variable) in scope.vars {
                self.addrs.remove(&variable.var_addr);
            }
            for break_addr in scope.breaks {
                self.code.overwrite(break_addr, ByteCode::Break(self.code.len()))
            }
        }
    }
    pub fn get(&self, id: &String) -> Option<&Variable> {
        for scope in self.scopes.iter().rev() {
            if let Some(variable) = scope.1.get(id) {
                return Some(variable)
            }
        }
        None
    }
    pub fn set(&mut self, id: String, variable: Variable) -> Option<Variable> {
        self.scopes.last_mut()?.1.set(id, variable)
    }
    pub fn create_var(&mut self, id: Located<ID>, typ: Type) -> Result<VarAddr, Error> {
        let Located { item: ID(id), pos } = id;
        let var_addr = self.new_addr();
        if let Some(old_variable) = self.set(id, Variable { typ, var_addr }) {
            self.addrs.remove(&old_variable.var_addr);
        }
        Ok(var_addr)
    }

    pub fn compile(&mut self, chunk: Chunk) -> Result<(), Error> {
        self.chunk(chunk)?;
        Ok(())
    }
    pub fn chunk(&mut self, chunk: Chunk) -> Result<(), Error> {
        for statement in chunk.0 {
            self.statement(statement)?;
        }
        Ok(())
    }
    pub fn statement(&mut self, statement: Located<Statment>) -> Result<(), Error> {
        let Located { item: statement, pos } = statement;
        match statement {
            Statment::Variable(id, type_expr, expr) => {
                let expr_pos = expr.pos.clone();
                let expr_typ = self.expr(expr)?;
                let typ = if let Some(type_expr) = type_expr {
                    self.eval_type(type_expr)?
                } else {
                    expr_typ.clone()
                };
                if typ != expr_typ {
                    return Err(Error::new(format!("expected right side of assigment to be {}, not {}", typ, expr_typ), self.path.clone(), Some(expr_pos)));
                }
                let ptr = self.create_var(id, typ)?.0;
                self.code.push(ByteCode::Pointer(ptr), pos.clone());
                self.code.push(ByteCode::Store, pos);
            }
            Statment::Assign(path, op, expr) => todo!(),
            Statment::Call(path, args) => todo!(),
            Statment::If(conds, cases, else_case) => todo!(),
            Statment::Match(expr, match_cases, else_case) => todo!(),
            Statment::While(cond, body) => todo!(),
            Statment::Repeat(count, body) => todo!(),
            Statment::For(parameter, iter, body) => todo!(),
            Statment::Return(expr) => todo!(),
            Statment::Break => todo!(),
            Statment::Continue => todo!(),
            Statment::Pass => todo!(),
            Statment::Function(path, parameters, return_type_expr, body) => todo!(),
        }
        Ok(())
    }
    pub fn expr(&mut self, expr: Located<Expression>) -> Result<Type, Error> {
        let Located { item: expr, pos } = expr;
        match expr {
            Expression::Atom(atom) => self.atom(atom),
            Expression::Binary { op, left, right } => {
                let left_type = self.expr(*left)?;
                let right_type = self.expr(*right)?;
                self.code.push(match op {
                    BinaryOperator::Add => ByteCode::Add,
                    BinaryOperator::Sub => ByteCode::Sub,
                    BinaryOperator::Mul => ByteCode::Mul,
                    BinaryOperator::Div => ByteCode::Div,
                    BinaryOperator::Mod => ByteCode::Mod,
                    BinaryOperator::Pow => ByteCode::Pow,
                    BinaryOperator::EQ => ByteCode::EQ,
                    BinaryOperator::NE => ByteCode::NE,
                    BinaryOperator::LT => ByteCode::LT,
                    BinaryOperator::GT => ByteCode::GT,
                    BinaryOperator::LE => ByteCode::LE,
                    BinaryOperator::GE => ByteCode::GE,
                    BinaryOperator::And => ByteCode::And,
                    BinaryOperator::Or => ByteCode::Or,
                    BinaryOperator::In => ByteCode::In,
                }, pos.clone());
                if let Some(res_type) = Type::binary(op, &left_type, &right_type) {
                    Ok(res_type)
                } else {
                    Err(Error::new(format!("cannot perform {:?} on {} with {}", format!("{:?}", op).to_lowercase(), left_type, right_type), self.path.clone(), Some(pos)))
                }
            }
            Expression::UnaryLeft { op, right } => {
                let right_type = self.expr(*right)?;
                self.code.push(match op {
                    UnaryLeftOperator::Neg => ByteCode::Neg,
                    UnaryLeftOperator::Not => ByteCode::Not,
                }, pos.clone());
                if let Some(res_type) = Type::unary_left(op, &right_type) {
                    Ok(res_type)
                } else {
                    Err(Error::new(format!("cannot perform {:?} on {}", format!("{:?}", op).to_lowercase(), right_type), self.path.clone(), Some(pos)))
                }
            }
            Expression::UnaryRight { op, left } => todo!("unary right compilation"),
            Expression::Call { func, args: Located { item: args, pos: args_pos } } => {
                let func_pos = func.pos.clone();
                let func_type = self.path(func)?;
                let mut arg_types = vec![];
                for arg in args.0 {
                    arg_types.push(self.expr(arg)?);
                }
                match func_type {
                    Type::Function(func_arg_types, ret_type) => {
                        // needs to be revised for args and kwargs
                        if func_arg_types != arg_types {
                            return Err(Error::new(
                                format!("invalid arguments for function, expected ({}), got ({})",
                                    join!(func_arg_types, ", "),
                                    join!(arg_types, ", ")),
                                self.path.clone(), Some(args_pos)
                            ))
                        }
                        if let Some(ret_type) = ret_type {
                            Ok(*ret_type)
                        } else {
                            Err(Error::new("function doesn't return anything", self.path.clone(), Some(func_pos)))
                        }
                    }
                    _ => Err(Error::new(format!("cannot call {}", func_type), self.path.clone(), Some(pos)))
                }
            }
        }
    }
    pub fn atom(&mut self, atom: Located<Atom>) -> Result<Type, Error> {
        let Located { item: atom, pos } = atom;
        match atom {
            Atom::Path(path) => {
                let typ = self.path(path)?;
                self.code.push(ByteCode::Load, pos);
                Ok(typ)
            }
            Atom::Int(value) => {
                let ConstAddr(_, addr) = self.code.int(value);
                self.code.push(ByteCode::ConstPointer(ConstPointer::Int(addr)), pos);
                Ok(Type::Int)
            }
            Atom::Float(value) => {
                let ConstAddr(_, addr) = self.code.float(value);
                self.code.push(ByteCode::ConstPointer(ConstPointer::Float(addr)), pos);
                Ok(Type::Float)
            }
            Atom::Bool(boolean) => {
                self.code.push(ByteCode::ConstPointer(ConstPointer::Bool(boolean)), pos);
                Ok(Type::Bool)
            }
            Atom::Char(char) => {
                self.code.push(ByteCode::ConstPointer(ConstPointer::Char(char)), pos);
                Ok(Type::Char)
            }
            Atom::String(value) => {
                let ConstAddr(_, addr) = self.code.string(value);
                self.code.push(ByteCode::ConstPointer(ConstPointer::String(addr)), pos);
                Ok(Type::String)
            }
            Atom::Expression(expr) => self.expr(*expr),
            Atom::Vector(vector) => {
                let mut vector_type: Option<Box<Type>> = None;
                for expr in vector {
                    let typ = self.expr(expr)?;
                    if let Some(vector_type) = &vector_type {
                        if vector_type.as_ref() != &typ {
                            return Err(Error::new(
                                format!("vector can only have one type, expected {}, got {}", *vector_type, typ),
                                self.path.clone(), Some(pos)
                            ))
                        }
                    } else {
                        vector_type = Some(Box::new(typ));
                    }
                }
                Ok(Type::Vector(vector_type))
            }
            Atom::Object(object) => todo!("object compilation"),
        }
    }
    pub fn path(&mut self, path: Located<Path>) -> Result<Type, Error> {
        let Located { item: path, pos } = path;
        match path {
            Path::ID(ID(id)) => {
                let Some(variable) = self.get(&id) else {
                    return Err(Error::new(format!("variable {:?} not found in current scope", id), self.path.clone(), Some(pos)))
                };
                let variable = variable.clone();
                self.code.push(ByteCode::Pointer(variable.var_addr.0), pos);
                Ok(variable.typ)
            }
            Path::Field(head, Located { item: field, pos: field_pos }) => todo!("field compilation"),
            Path::Index(head, index) => {
                let head_type = self.path(*head)?;
                let index_type = self.expr(*index)?;
                self.code.push(ByteCode::Index, pos.clone());
                match head_type {
                    Type::String => match index_type {
                        Type::Int => Ok(Type::Char),
                        _ => Err(Error::new(format!("cannot index {} with {}", head_type, index_type), self.path.clone(), Some(pos)))
                    }
                    Type::Vector(sub_type) => if let Some(sub_type) = sub_type {
                        match index_type {
                            Type::Int => Ok(*sub_type),
                            _ => Err(Error::new(format!("cannot index {} with {}", Type::Vector(Some(sub_type)), index_type), self.path.clone(), Some(pos)))
                        }
                    } else {
                        Err(Error::new(format!("cannot index {} with {}", Type::Vector(sub_type), index_type), self.path.clone(), Some(pos)))
                    }
                    _ => Err(Error::new(format!("cannot index into {}", head_type), self.path.clone(), Some(pos)))
                }
            }
        }
    }

    pub fn eval_type(&mut self, type_expr: Located<TypeExpression>) -> Result<Type, Error> {
        let Located { item: type_expr, pos } = type_expr;
        match type_expr {
            TypeExpression::Type(id) => match id.to_string().as_str() {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
                "bool" => Ok(Type::Bool),
                "char" => Ok(Type::Char),
                "str" => Ok(Type::String),
                "vec" => Ok(Type::Vector(None)),
                "obj" => Ok(Type::Object),
                "result" => Ok(Type::Result(None)),
                "opt" => Ok(Type::Option(None)),
                _ => {
                    todo!("id type parsing")
                    // Ok(self.get_type_id(id, pos)?)
                }
            }
            TypeExpression::Sub(head_type, sub_type_exprs) => {
                let mut sub_types = vec![];
                for sub_type in sub_type_exprs {
                    sub_types.push(self.eval_type(sub_type)?);
                }
                if sub_types.len() == 0 {
                    return Err(Error::new("no sub-type", self.path.clone(), Some(pos)))
                }
                match self.eval_type(*head_type)? {
                    Type::Vector(_) => {
                        let sub_type = sub_types.remove(0);
                        if sub_types.len() != 0 {
                            return Err(Error::new("too many sub-types, only one required", self.path.clone(), Some(pos)))
                        }
                        Ok(Type::Vector(Some(Box::new(sub_type))))
                    }
                    Type::Option(_) => {
                        let sub_type = sub_types.remove(0);
                        if sub_types.len() != 0 {
                            return Err(Error::new("too many sub-types, only one required", self.path.clone(), Some(pos)))
                        }
                        Ok(Type::Option(Some(Box::new(sub_type))))
                    }
                    Type::Result(_) => {
                        let ok_sub_type = sub_types.remove(0);
                        if sub_types.len() != 0 {
                            return Err(Error::new("missing 2nd sub-type", self.path.clone(), Some(pos)))
                        }
                        let err_sub_type = sub_types.remove(0);
                        if sub_types.len() != 0 {
                            return Err(Error::new("too many sub-types, only one required", self.path.clone(), Some(pos)))
                        }
                        Ok(Type::Result(Some((Box::new(ok_sub_type), Box::new(err_sub_type)))))
                    }
                    typ => Err(Error::new(format!("cannot define sub-type for {}", typ), self.path.clone(), Some(pos)))
                }
            }
        }
    }
}