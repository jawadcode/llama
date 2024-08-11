use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use error::InferError;
use llamac_ast::{
    stmt::{Const, FunDef},
    Item, SourceFile,
};
use llamac_typed_ast::{Type, TypedItem, TypedSourceFile, Types};
use llamac_utils::{spanned, Ident, Span, Spanned};

mod error;
mod infer_expr;
mod infer_stmt;
mod match_exhaustivity;
mod subst_expr;
mod subst_stmt;

/// The type inference engine
pub struct Engine {
    /// The substitution from type variables to types to be built up
    substitution: Vec<Type>,
    /// A set of relationships between types
    constraints: Vec<Constraint>,
    /// The context Γ
    context: Vec<HashMap<Ident, Spanned<Type>>>,
}

/// A relationship between 2 `Types`s
#[derive(Debug, Clone)]
pub enum Constraint {
    Equality {
        expected: Type,
        expected_span: Span,
        got: Type,
        got_span: Span,
    },
}

impl Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Equality {
                expected,
                expected_span,
                got,
                got_span,
            } => write!(
                f,
                "{expected} == {got}, due to {expected_span}, at {got_span}"
            ),
        }
    }
}

pub type InferResult<T> = Result<T, error::InferError>;

impl Engine {
    pub fn new(env: HashMap<Ident, Spanned<Type>>) -> Self {
        Self {
            substitution: Vec::new(),
            constraints: Vec::new(),
            context: vec![env],
        }
    }

    /// Infer the types of expressions in a source file
    pub fn infer_source_file(
        &mut self,
        SourceFile { path, items }: SourceFile,
    ) -> InferResult<TypedSourceFile> {
        // Scan over the top-level items, without performing any typechecking, adding them to the context
        // This allows for recursion and mutual recursion as functions must always have type annotations
        for Spanned {
            span: _,
            node: item,
        } in items.iter()
        {
            let (Item::Const(Const { name, .. }) | Item::FunDef(FunDef { name, .. })) = item;
            let ty = match item {
                Item::Const(Const { annot, .. }) => (&annot.node).into(),
                Item::FunDef(FunDef { params, ret_ty, .. }) => {
                    let params = Types(
                        params
                            .node
                            .0
                            .iter()
                            .map(|Spanned { node, .. }| (&node.annot.node).into())
                            .collect(),
                    );
                    let ret_ty = Box::new((&ret_ty.node).into());
                    Type::Fun { params, ret_ty }
                }
            };
            self.extend(name.node.clone(), ty, name.span);
        }

        // Actually typecheck the items
        let new_items = items
            .into_iter()
            .map(|item| {
                item.map_res(|item| {
                    Ok(match item {
                        Item::Const(r#const) => TypedItem::Const(self.infer_const(r#const, true)?),
                        Item::FunDef(fun_def) => {
                            TypedItem::FunDef(self.infer_fun_def(fun_def, true)?)
                        }
                    })
                })
            })
            .collect::<InferResult<Vec<_>>>()?;

        Ok(TypedSourceFile {
            path,
            items: new_items,
        })
    }

    /// Unify all of the constraints
    pub fn solve_constraints(&mut self) -> InferResult<()> {
        for Constraint::Equality {
            expected,
            expected_span,
            got,
            got_span,
        } in std::mem::take(&mut self.constraints)
        {
            self.unify(&expected, expected_span, &got, got_span)?;
        }
        Ok(())
    }

    /// Apply the substitution to all of the type variables in a source file
    pub fn subst_source_file(
        &self,
        TypedSourceFile { path, items }: TypedSourceFile,
    ) -> TypedSourceFile {
        let items = items
            .into_iter()
            .map(|item| {
                item.map(|item| match item {
                    TypedItem::Const(r#const) => TypedItem::Const(self.subst_const(r#const)),
                    TypedItem::FunDef(fun_def) => TypedItem::FunDef(self.subst_fun_def(r#fun_def)),
                })
            })
            .collect();
        TypedSourceFile { path, items }
    }

    pub fn is_solved(&self, TypedSourceFile { path, items }: TypedSourceFile) -> InferResult<()> {
        unimplemented!("Not necessary with the current type system afaik")
    }

    pub fn dump_constraints(&self) {
        for Constraint::Equality {
            expected,
            expected_span,
            got,
            got_span,
        } in &self.constraints
        {
            println!(
                "expected {} [{}], got {} [{}]",
                expected, expected_span, got, got_span
            );
        }
    }

    pub fn dump_subst(&self) {
        for (i, ty) in self.substitution.iter().enumerate() {
            println!("T{i} = {ty}");
        }
    }

    /// Resolve an equality constraint, producing an error if it results in a contradiction
    fn unify(
        &mut self,
        expected: &Type,
        expected_span: Span,
        got: &Type,
        got_span: Span,
    ) -> InferResult<()> {
        // this would look so much nicer in ocaml or haskell
        use Type as T;
        match (expected, got) {
            (T::Var(x_id), y) if self.get_subst(x_id) != &T::Var(*x_id) => {
                let x = self.get_subst_cloned(x_id);
                self.unify(&x, expected_span, y, got_span)
            }
            (T::Var(x_id), y) => {
                if self.occurs_in(*x_id, y) {
                    Err(InferError::InfiniteType {
                        id: *x_id,
                        id_span: expected_span,
                        ty: self.subst_ty(y.clone()),
                        ty_span: got_span,
                    })
                } else {
                    self.substitution[*x_id] = y.clone();
                    Ok(())
                }
            }
            (x, T::Var(y_id)) if self.get_subst(y_id) != &T::Var(*y_id) => {
                let y = self.get_subst_cloned(y_id);
                self.unify(x, expected_span, &y, got_span)
            }
            (x, T::Var(y_id)) => {
                if self.occurs_in(*y_id, x) {
                    Err(InferError::InfiniteType {
                        id: *y_id,
                        id_span: got_span,
                        ty: self.subst_ty(x.clone()),
                        ty_span: expected_span,
                    })
                } else {
                    self.substitution[*y_id] = x.clone();
                    Ok(())
                }
            }
            (
                x @ T::Fun {
                    params: x_params,
                    ret_ty: x_ret_ty,
                },
                y @ T::Fun {
                    params: y_params,
                    ret_ty: y_ret_ty,
                },
            ) => {
                if x_params.0.len() != y_params.0.len() {
                    let (got, expected) = (self.subst_ty(x.clone()), self.subst_ty(y.clone()));
                    return Err(InferError::TypeMismatch {
                        expected: got,
                        got_span,
                        got: expected,
                        expected_span,
                    });
                }
                self.unify(x_ret_ty, expected_span, y_ret_ty, got_span)?;
                for (x, y) in x_params.0.iter().zip(y_params.0.iter()) {
                    self.unify(x, expected_span, y, got_span)?;
                }
                Ok(())
            }
            (T::List(x), T::List(y)) => self.unify(x, expected_span, y, got_span),
            (T::Unit, T::Unit)
            | (T::Bool, T::Bool)
            | (T::Int, T::Int)
            | (T::Float, T::Float)
            | (T::String, T::String) => Ok(()),
            (x, y) => Err(InferError::TypeMismatch {
                expected: x.clone(),
                got_span,
                got: y.clone(),
                expected_span,
            }),
        }
    }

    /// Check for infinitely recursive types (no idea if this is necessary yet)
    fn occurs_in(&self, index: usize, ty: &Type) -> bool {
        match ty {
            Type::Var(id) if self.get_subst(id) != &Type::Var(*id) => {
                self.occurs_in(index, self.get_subst(id))
            }
            Type::Var(id) => *id == index,
            Type::Fun { params, ret_ty } => {
                params.0.iter().any(|ty| self.occurs_in(index, ty)) || self.occurs_in(index, ret_ty)
            }
            Type::List(ty) => self.occurs_in(index, ty),
            Type::Unit | Type::Bool | Type::Int | Type::Float | Type::String => false,
        }
    }

    /// Applies the substitution to `ty`
    fn subst_ty(&self, ty: Type) -> Type {
        match ty {
            Type::Var(id) if self.substitution[id] != Type::Var(id) => {
                self.subst_ty(self.substitution[id].clone())
            }
            Type::Fun { params, ret_ty } => Type::Fun {
                params: Types(params.0.into_iter().map(|ty| self.subst_ty(ty)).collect()),
                ret_ty: Box::new(self.subst_ty(*ret_ty)),
            },
            Type::List(ty) => Type::List(Box::new(self.subst_ty(*ty))),
            ty => ty,
        }
    }

    /// Get a reference to an item from the substitution
    fn get_subst(&self, id: &usize) -> &Type {
        &self.substitution[*id]
    }

    /// Clone an item from the substitution
    fn get_subst_cloned(&self, id: &usize) -> Type {
        self.substitution[*id].clone()
    }

    /// Generate a fresh type variable and add it to the list of substitutions
    fn fresh_var(&mut self) -> Type {
        let tyvar = Type::Var(self.substitution.len());
        self.substitution.push(tyvar.clone());
        tyvar
    }

    /// Push a new scope to the context
    fn enter_scope(&mut self) {
        self.context.push(HashMap::new());
    }

    /// Pop the innermost scope from the context
    fn exit_scope(&mut self) {
        self.context.pop();
    }

    /// Get the type of an assumption from the context
    fn get_var(&self, name: &Ident) -> Option<Spanned<Type>> {
        for scope in self.context.iter().rev() {
            match scope.get(name) {
                Some(ty) => return Some(ty.clone()),
                None => continue,
            }
        }

        None
    }

    /// Extend the context with an assumption
    fn extend(&mut self, name: Ident, ty: Type, span: Span) {
        self.context
            .last_mut()
            .unwrap()
            .insert(name, spanned! {span, ty});
    }
}
