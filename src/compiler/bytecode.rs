use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ByteCode {
    None, Halt,
    Jump, JumpIf, JumpIfNot,
    Call, CallReturn, Return,
    Pointer(ValuePointer),
    Load, Store,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValuePointer {
    Constant(usize),
    Local(usize),
    Argument(usize),
    Global(usize),
    Function(usize),
    Stack(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstantAddr(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionAddr(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalAddr(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalAddr(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeAddr(pub usize);
#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub parent: Option<ScopeAddr>,
    pub children: Vec<ScopeAddr>,
    pub locals: HashSet<LocalAddr>,
    pub arguments: HashSet<LocalAddr>,
    pub functions: HashSet<FunctionAddr>,
    pub globals: HashSet<GlobalAddr>,
}
impl Scope {
    pub fn new(parent: Option<ScopeAddr>) -> Self {
        Self {
            parent,
            children: vec![],
            locals: HashSet::new(),
            arguments: HashSet::new(),
            functions: HashSet::new(),
            globals: HashSet::new(),
        }
    }
    pub fn local(&mut self) -> LocalAddr {
        let local = LocalAddr(self.locals.len());
        self.locals.insert(local);
        local
    }
    pub fn argument(&mut self) -> LocalAddr {
        let argument = LocalAddr(self.arguments.len());
        self.arguments.insert(argument);
        argument
    }
    pub fn function(&mut self, code: &mut Code) -> FunctionAddr {
        let function = FunctionAddr(self.functions.len());
        self.functions.insert(function);
        code.functions.insert(function);
        function
    }
    pub fn global(&mut self, code: &mut Code) -> GlobalAddr {
        let global = GlobalAddr(self.globals.len());
        self.globals.insert(global);
        code.globals.insert(global);
        global
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Code {
    pub code: Vec<ByteCode>,
    pub constants: HashSet<ConstantAddr>,
    pub functions: HashSet<FunctionAddr>,
    pub globals: HashSet<GlobalAddr>,
    pub scopes: Vec<Scope>,
    pub current_locals: HashSet<GlobalAddr>,
}
impl Code {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: HashSet::new(),
            functions: HashSet::new(),
            globals: HashSet::new(),
            scopes: vec![Scope::new(None)],
        }
    }
    pub fn new_scope(&mut self, parent: Option<ScopeAddr>) -> ScopeAddr {
        let addr = ScopeAddr(self.scopes.len());
        let scope = Scope::new(parent);
        self.scopes.push(scope);
        addr
    }
    pub fn remove_scope(&mut self, addr: ScopeAddr) {
        let scope = self.scopes.remove(addr.0);
        scope.remove(self);
        for child in scope.children {
            self.remove_scope(child);
        }
    }
}