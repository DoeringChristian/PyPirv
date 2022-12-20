use std::collections::HashMap;

use rspirv::binary::Disassemble;

use python_parser::ast::*;

#[allow(dead_code)]
mod ast;

fn build() -> Result<(), rspirv::dr::Error> {
    let mut b = rspirv::dr::Builder::new();

    b.set_version(1, 3);
    b.memory_model(
        rspirv::spirv::AddressingModel::Logical,
        rspirv::spirv::MemoryModel::Simple,
    );

    let void = b.type_void();
    let voidf = b.type_function(void, vec![void]);
    let x = b.begin_function(
        void,
        None,
        (rspirv::spirv::FunctionControl::DONT_INLINE | rspirv::spirv::FunctionControl::CONST),
        voidf,
    );
    b.begin_block(None)?;
    b.ret()?;
    b.end_function()?;
    println!("{}", b.module().disassemble());
    Ok(())
}

fn main() {
    let ty = rspirv::sr::Type::Void;
    //build().unwrap();
    let code = include_str!("input/test.py");
    let ast = python_parser::file_input(python_parser::make_strspan(code))
        .unwrap()
        .1;
    println!("{:#?}", ast);

    let sig = ast::FunctionSig {
        name: "test".into(),
        args: vec![ast::Type::Float32, ast::Type::Float32],
    };
    let mut a = ast::Ast::default();
    a.function(&ast, sig);
    println!("{:#?}", a);
}
