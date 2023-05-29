#![allow(dead_code)]
extern crate peng_parser;
use peng_parser::{*, location::path::FilePath};

pub mod compiler;
pub mod runtime;

fn entry() -> Result<(), error::Error> {
    let mut args = arguments::Arguments::new();
    args.collect(std::env::args()).map_err(|err| error::Error::msg(err))?;
    if let Some(path) = args.next_input() {
        let filepath = FilePath::Path(path.clone());
        let ast = parse_file(&path, &args)?;
        let code = compiler::compile(&filepath, ast)?;
        dbg!(code);
        // runtime::run(code)?;
    }
    Ok(())
}

fn main() {
    if let Some(err) = entry().err() {
        eprintln!("{}", err);
    }
}
