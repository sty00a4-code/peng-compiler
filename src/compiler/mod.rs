pub mod types;
pub mod bytecode;
pub mod compiler;

use peng_parser::{location::path::FilePath, parser::ast::*, error::Error};
use crate::*;
use bytecode::*;
use compiler::*;

pub fn compile(path: &FilePath, ast: Chunk) -> Result<Code, Error> {
    let mut compiler = Compiler::new(path);
    compiler.compile(ast)?;
    Ok(compiler.code)
}