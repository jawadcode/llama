use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use llamac_ast::{
    stmt::{Const, FunDef, FunParams},
    Item, SourceFile,
};
use llamac_typed_ast::{Type, TypedItem, TypedSourceFile, Types};
use llamac_utils::{spanned, Ident, Span, Spanned};

pub mod expr;
pub mod stmt;

// The type inference engine
pub struct Engine {
    // The substitution from type variables to types to be built up
    subst: Vec<Type>,
    // A set of relationships between types
    constraints: Vec<Constraint>,
    // The context Γ
    context: Vec<HashMap<Ident, Spanned<Type>>>,
}

// A relationship between 2 `Types`s
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

pub type InferResult<T> = Result<T, InferError>;

#[derive(Debug, Clone)]
pub enum InferError {
    NotFound { ident: Ident, span: Span },
}

impl Display for InferError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl Engine {
    pub fn new(env: HashMap<Ident, Spanned<Type>>) -> Self {
        Self {
            subst: Vec::new(),
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
                    let params = params.map_ref(|FunParams(params)| Types(params.iter().map(|Spanned { span, node }| spanned! {*span, (&node.annot.node).into()}).collect()));
                    let ret_ty = ret_ty.map_ref(|ty| Box::new(ty.into()));
                    Type::Fun { params, ret_ty }
                }
            };
            self.extend(name.node.clone(), ty, name.span);
        }

        dbg!(&self.context);

        // Actually typecheck the items
        let mut new_items = Vec::with_capacity(items.len());
        for Spanned { span, node: item } in items {
            let new_item = match item {
                Item::Const(r#const) => TypedItem::Const(self.infer_const(r#const, true)?),
                Item::FunDef(fun_def) => TypedItem::FunDef(self.infer_fun_def(fun_def, true)?),
            };
            new_items.push(spanned! {span, new_item});
        }

        Ok(TypedSourceFile {
            path,
            items: new_items,
        })
    }

    /// Generate a fresh type variable and add it to the list of substitutions
    fn fresh_var(&mut self) -> Type {
        let tyvar = Type::Var(self.subst.len());
        self.subst.push(tyvar.clone());
        tyvar
    }

    /// Push a new scope to the context
    fn enter_scope(&mut self) {
        self.context.push(HashMap::new());
    }

    // Pop the innermost scope from the context
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
