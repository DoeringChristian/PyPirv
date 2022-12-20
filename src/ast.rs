use python_parser::ast as py;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Void,
    Bool,
    Int32,
    Float32,
}
impl Default for Type {
    fn default() -> Self {
        Self::Void
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionSig {
    pub name: String,
    pub args: Vec<Type>,
}

#[derive(Debug, Clone, Copy)]
pub enum Uop {
    Plus,
    Minus,
    Invert,
    Not,
}

impl From<&py::Uop> for Uop {
    fn from(value: &py::Uop) -> Self {
        match value {
            py::Uop::Plus => Uop::Plus,
            py::Uop::Minus => Uop::Minus,
            py::Uop::Invert => Uop::Invert,
            py::Uop::Not => Uop::Not,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Bop {
    Add,
    Sub,
    Mult,
    Matmult,
    Mod,
    Floordiv,
    Div,
    Power,
    Lshift,
    Rshift,
    BitAnd,
    BitXor,
    BitOr,
    /// lower than
    Lt,
    /// greater than
    Gt,
    Eq,
    /// lower or equal
    Leq,
    /// greater or equal
    Geq,
    Neq,
    In,
    NotIn,
    Is,
    IsNot,
    And,
    Or,
}
impl Bop {
    fn ty(&self, lhs: &Type, rhs: &Type) -> Type {
        match self {
            Self::Floordiv => Type::Int32,
            Self::Lt => Type::Bool,
            Self::Gt => Type::Bool,
            Self::Eq => Type::Bool,
            Self::Leq => Type::Bool,
            Self::Geq => Type::Bool,
            Self::Neq => Type::Bool,
            Self::And => Type::Bool,
            Self::Or => Type::Bool,
            _ => *lhs.max(rhs),
        }
    }
}
impl From<&py::Bop> for Bop {
    fn from(value: &py::Bop) -> Self {
        match value {
            py::Bop::Add => Bop::Add,
            py::Bop::Sub => Bop::Sub,
            py::Bop::Mult => Bop::Mult,
            py::Bop::Matmult => Bop::Matmult,
            py::Bop::Mod => Bop::Mod,
            py::Bop::Floordiv => Bop::Floordiv,
            py::Bop::Div => Bop::Div,
            py::Bop::Power => Bop::Power,
            py::Bop::Lshift => Bop::Lshift,
            py::Bop::Rshift => Bop::Rshift,
            py::Bop::BitAnd => Bop::BitAnd,
            py::Bop::BitXor => Bop::BitXor,
            py::Bop::BitOr => Bop::BitOr,
            py::Bop::Lt => Bop::Lt,
            py::Bop::Gt => Bop::Gt,
            py::Bop::Eq => Bop::Eq,
            py::Bop::Leq => Bop::Leq,
            py::Bop::Geq => Bop::Geq,
            py::Bop::Neq => Bop::Neq,
            py::Bop::In => Bop::In,
            py::Bop::NotIn => Bop::NotIn,
            py::Bop::Is => Bop::Is,
            py::Bop::IsNot => Bop::IsNot,
            py::Bop::And => Bop::And,
            py::Bop::Or => Bop::Or,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    True,
    False,
    Float(f64),
    Int(i32),
    Name(String, Option<Type>),
    Uop(Uop, Box<Expression>, Option<Type>),
    Bop(Bop, Box<Expression>, Box<Expression>, Option<Type>),
    Call(FunctionSig, Vec<Expression>, Option<Type>),
}

impl Expression {
    pub fn ty(&self) -> Option<Type> {
        match self {
            Self::True => Some(Type::Bool),
            Self::False => Some(Type::Bool),
            Self::Float(_) => Some(Type::Float32),
            Self::Int(_) => Some(Type::Int32),
            Self::Uop(_, _, ty) => *ty,
            Self::Bop(_, _, _, ty) => *ty,
            Self::Name(_, ty) => *ty,
            Self::Call(_, _, ty) => *ty,
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Return(Box<Expression>),
    Assign(String, Box<Expression>),
    If(Vec<(Expression, Vec<Statement>)>, Option<Vec<Statement>>),
}

#[derive(Debug, Default)]
pub struct FunctionDef {
    pub args: Vec<String>,
    pub code: Vec<Statement>,
    pub return_type: Type,
}

#[derive(Debug, Default)]
pub struct Ast {
    pub functions: HashMap<FunctionSig, FunctionDef>,
}

impl Ast {
    fn find_funcdef<'a>(src: &'a [py::Statement], name: &str) -> &'a py::Funcdef {
        for stmt in src.iter() {
            match stmt {
                py::Statement::Compound(compound) => match compound.as_ref() {
                    py::CompoundStatement::Funcdef(fdef) => {
                        if fdef.name == name {
                            return fdef;
                        }
                    }
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            }
        }
        todo!()
    }
    pub fn expr(
        &mut self,
        src: &[py::Statement],
        expr: &py::Expression,
        types: &mut HashMap<String, Type>,
    ) -> Expression {
        match expr {
            py::Expression::True => Expression::True,
            py::Expression::False => Expression::False,
            py::Expression::Float(f) => Expression::Float(*f),
            py::Expression::Int(i) => {
                Expression::Int((*i.to_u32_digits().iter().last().unwrap()) as i32)
            }
            py::Expression::Uop(uop, expr) => {
                let expr = self.expr(src, expr, types);
                let ty = expr.ty();
                Expression::Uop(Uop::from(uop), Box::new(expr), ty)
            }
            py::Expression::Bop(bop, lhs, rhs) => {
                let lhs = self.expr(src, lhs, types);
                let rhs = self.expr(src, rhs, types);
                let bop = Bop::from(bop);
                let ty = bop.ty(&lhs.ty().unwrap(), &rhs.ty().unwrap());
                Expression::Bop(bop, Box::new(lhs), Box::new(rhs), Some(ty))
            }
            py::Expression::Name(name) => {
                let ty = types[name];
                Expression::Name(name.clone(), Some(ty))
            }
            py::Expression::Call(expr, args) => {
                if let py::Expression::Name(name) = expr.as_ref() {
                    let args = args
                        .iter()
                        .map(|arg| {
                            if let py::Argument::Positional(expr) = arg {
                                self.expr(src, expr, types)
                            } else {
                                unimplemented!()
                            }
                        })
                        .collect::<Vec<_>>();
                    let sig = FunctionSig {
                        name: name.clone(),
                        args: args
                            .iter()
                            .map(|arg| arg.ty().expect("Type has to be known at function call!"))
                            .collect::<Vec<_>>(),
                    };
                    let f = self.function(src, sig.clone());
                    let ty = Some(f.return_type);
                    Expression::Call(sig, args, ty)
                } else {
                    unimplemented!()
                }
            }
            _ => unimplemented!(),
        }
    }
    fn statement(
        &mut self,
        src: &[py::Statement],
        stmt: &py::Statement,
        types: &mut HashMap<String, Type>,
    ) -> Statement {
        match stmt {
            py::Statement::Return(expr) => {
                assert!(expr.len() == 1);
                let expr = self.expr(src, &expr[0], types);

                let ty = expr.ty().unwrap();
                if types.contains_key("return".into()) {
                    assert!(ty == types["return"])
                } else {
                    types.insert("return".into(), ty);
                }

                Statement::Return(Box::new(expr))
            }
            py::Statement::Assignment(lhs, rhs) => {
                assert!(lhs.len() == 1);
                assert!(rhs.len() == 1);
                assert!(rhs[0].len() == 1);
                if let py::Expression::Name(name) = &lhs[0] {
                    let rhs = self.expr(src, &rhs[0][0], types);
                    Statement::Assign(name.clone(), Box::new(rhs))
                } else {
                    unimplemented!()
                }
            }
            py::Statement::Compound(stmt) => match stmt.as_ref() {
                py::CompoundStatement::If(i, e) => {
                    let i = i
                        .iter()
                        .map(|(expr, code)| {
                            let expr = self.expr(src, expr, types);
                            let code = code
                                .iter()
                                .map(|stmt| self.statement(src, stmt, types))
                                .collect::<Vec<_>>();
                            (expr, code)
                        })
                        .collect::<Vec<_>>();
                    let e = e.as_ref().map(|code| {
                        code.iter()
                            .map(|stmt| self.statement(src, stmt, types))
                            .collect::<Vec<_>>()
                    });
                    Statement::If(i, e)
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
    pub fn function(&mut self, src: &[py::Statement], sig: FunctionSig) -> &FunctionDef {
        if self.functions.contains_key(&sig) {
            // return early if function signature is already defined
            return &self.functions[&sig];
        }
        let mut fdef = Self::find_funcdef(src, &sig.name);

        // Add function arg types
        let mut types: HashMap<String, Type> = HashMap::default();
        let mut f = FunctionDef::default();
        for (i, arg) in fdef.parameters.args.iter().enumerate() {
            f.args.push(arg.0.clone());
            types.insert(arg.0.clone(), sig.args[i].clone());
        }

        for stmt in &fdef.code {
            f.code.push(self.statement(src, stmt, &mut types))
        }

        if types.contains_key("return".into()) {
            f.return_type = types["return"];
        }

        self.functions.insert(sig.clone(), f);

        return &self.functions[&sig];
    }
}
