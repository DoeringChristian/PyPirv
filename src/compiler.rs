use std::collections::HashMap;

use crate::ast::{self, Ast, FunctionSig};

pub struct Compiler {
    pub b: rspirv::dr::Builder,
    functions: HashMap<FunctionSig, u32>,
    vars: HashMap<String, u32>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut b = rspirv::dr::Builder::new();

        b.set_version(1, 3);
        b.memory_model(
            rspirv::spirv::AddressingModel::Logical,
            rspirv::spirv::MemoryModel::Simple,
        );
        Self {
            b,
            functions: Default::default(),
            vars: Default::default(),
        }
    }
    fn record_type(&mut self, ty: ast::Type) -> u32 {
        match ty {
            ast::Type::Void => self.b.type_void(),
            ast::Type::Bool => self.b.type_bool(),
            ast::Type::Int32 => self.b.type_int(32, 1),
            ast::Type::Float32 => self.b.type_float(32),
        }
    }
    fn record_uop(&mut self, uop: ast::Uop, expr: &ast::Expression, ty: ast::Type) -> u32 {
        match uop {
            ast::Uop::Plus => {
                let var = self.record_expr(expr);
                var
            }
            ast::Uop::Minus => {
                let var = self.record_expr(expr);
                let ty = self.record_type(ty);
                match expr.ty().unwrap() {
                    ast::Type::Int32 => self.b.s_negate(ty, None, var).unwrap(),
                    ast::Type::Float32 => self.b.f_negate(ty, None, var).unwrap(),
                    _ => unimplemented!(),
                }
            }
            ast::Uop::Invert => {
                todo!()
            }
            ast::Uop::Not => {
                let var = self.record_expr(expr);
                let ty = self.record_type(ty);
                match expr.ty().unwrap() {
                    ast::Type::Bool => self.b.logical_not(ty, None, var).unwrap(),
                    _ => unimplemented!(),
                }
            }
        }
    }
    fn record_bop(
        &mut self,
        bop: ast::Bop,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
        ty: ast::Type,
    ) -> u32 {
        match bop {
            ast::Bop::Add => {
                let lvar = self.record_expr(lhs);
                let rvar = self.record_expr(rhs);
                let rty = self.record_type(ty);
                match ty {
                    ast::Type::Int32 => self.b.i_add(rty, None, lvar, rvar).unwrap(),
                    ast::Type::Float32 => self.b.f_add(rty, None, lvar, rvar).unwrap(),
                    _ => unimplemented!(),
                }
            }
            ast::Bop::Sub => {
                let lvar = self.record_expr(lhs);
                let rvar = self.record_expr(rhs);
                let rty = self.record_type(ty);
                match ty {
                    ast::Type::Int32 => self.b.i_sub(rty, None, lvar, rvar).unwrap(),
                    ast::Type::Float32 => self.b.f_sub(rty, None, lvar, rvar).unwrap(),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }
    fn record_expr(&mut self, expr: &ast::Expression) -> u32 {
        match expr {
            ast::Expression::True => {
                let ty = self.record_type(expr.ty().unwrap());
                self.b.constant_true(ty)
            }
            ast::Expression::False => {
                let ty = self.record_type(expr.ty().unwrap());
                self.b.constant_false(ty)
            }
            ast::Expression::Float(f) => {
                let ty = self.record_type(expr.ty().unwrap());
                self.b.constant_f32(ty, *f as f32)
            }
            ast::Expression::Int(i) => {
                let ty = self.record_type(expr.ty().unwrap());
                self.b.constant_u32(ty, *i as u32)
            }
            ast::Expression::Name(name, ty) => self.vars[name],
            ast::Expression::Uop(uop, expr, ty) => self.record_uop(*uop, expr, ty.unwrap()),
            ast::Expression::Bop(bop, lhs, rhs, ty) => self.record_bop(*bop, lhs, rhs, ty.unwrap()),
            ast::Expression::Call(sig, args, ty) => {
                let ty = self.record_type(ty.unwrap());
                let args = args
                    .iter()
                    .map(|arg| self.record_expr(arg))
                    .collect::<Vec<_>>();
                self.b
                    .function_call(ty, None, self.functions[sig], args)
                    .unwrap()
            }
            _ => unimplemented!(),
        }
    }
    fn record_stmt(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::Return(expr) => {
                let val = self.record_expr(expr);
                self.b.ret_value(val).unwrap();
            }
            ast::Statement::Assign(name, expr) => {
                let var = self.record_expr(expr);
                self.vars.insert(name.clone(), var);
            }
            _ => unimplemented!(),
        }
    }
    pub fn compile(&mut self, ast: &Ast) {
        for (sig, fdef) in ast.functions.iter() {
            let return_type = self.record_type(fdef.return_type);
            let arg_types = sig
                .args
                .iter()
                .map(|arg| self.record_type(*arg))
                .collect::<Vec<_>>();

            // Get function type and define function
            let ftype = self.b.type_function(return_type, arg_types.clone());
            let f = self
                .b
                .begin_function(
                    return_type,
                    None,
                    (rspirv::spirv::FunctionControl::DONT_INLINE
                        | rspirv::spirv::FunctionControl::CONST),
                    ftype,
                )
                .unwrap();
            self.functions.insert(sig.clone(), f);

            // Add function parameters
            for (i, name) in fdef.args.iter().enumerate() {
                let var = self.b.function_parameter(arg_types[i]).unwrap();
                self.vars.insert(name.clone(), var);
            }

            let flabel = self.b.begin_block(None).unwrap();

            for stmt in &fdef.code {
                self.record_stmt(stmt);
            }

            self.b.end_function().unwrap();
        }
    }
}
