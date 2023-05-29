use std::collections::HashSet;

use crate::*;
use peng_parser::{location::{path::FilePath, position::Located}, parser::ast::*, error::Error};

use super::bytecode::*;



pub struct Compiler {
    pub path: FilePath,
    pub code: Code,
}
impl Compiler {
    pub fn new(path: &FilePath) -> Self {
        Self {
            path: path.clone(),
            code: Code::new(),
        }
    }

    pub fn compile(&mut self, chunk: Chunk) -> Result<(), Error> {
        self.compile_chunk(chunk)?;
        Ok(())
    }
    pub fn compile_chunk(&mut self, chunk: Chunk) -> Result<(), Error> {
        for statement in chunk.0 {
            self.compile_statement(statement)?;
        }
        Ok(())
    }
    pub fn compile_statement(&mut self, statement: Located<Statment>) -> Result<(), Error> {
        let Located { item: statement, pos } = statement;
        todo!("compile_statement")
    }
}