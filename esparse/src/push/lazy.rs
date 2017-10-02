#![allow(missing_docs)]

use std::borrow::Cow;

use ast::Span;
use push;
use push::*;

#[derive(Debug)]
pub enum Push {}

#[derive(Debug)]
pub struct Expr<'f, 's> {
    pub kind: ExprKind<'f, 's>,
    pub span: Span<'f>,
}

#[derive(Debug)]
pub enum ExprKind<'f, 's> {
    This,
    Ident(Cow<'s, str>),
    Null,
    Bool(bool),
    Int(u64),
    Float(f64),
    Str(Cow<'s, str>),
    // TODO RegExp(),
    Template(Cow<'s, str>, DeferredTemplateParts<'f, 's, Push>),
    Array(DeferredArrayValues<'f, 's, Push>),
    Object(DeferredObjectPropertyDefs<'f, 's, Push>),
}

#[derive(Debug)]
pub struct Stmt<'f, 's> {
    pub kind: StmtKind<'s>,
    pub span: Span<'f>,
}

#[derive(Debug)]
pub enum StmtKind<'s> {
    Todo(&'s str),
    // TODO
}

#[derive(Debug)]
pub struct Decl<'f, 's> {
    pub kind: DeclKind<'s>,
    pub span: Span<'f>,
}

#[derive(Debug)]
pub enum DeclKind<'s> {
    Todo(&'s str),
    // TODO
}

#[derive(Debug)]
pub struct Pat<'f, 's> {
    pub kind: PatKind<'s>,
    pub span: Span<'f>,
}

#[derive(Debug)]
pub enum PatKind<'s> {
    Todo(&'s str),
    // TODO
}

#[derive(Debug)]
pub struct Params<'f, 's> {
    pub params: DeferredFormalParams<'f, 's, Push>,
    pub span: Span<'f>,
}

#[derive(Debug)]
pub struct FnBody<'f, 's> {
    pub body: DeferredBlock<'f, 's, Push>,
    pub span: Span<'f>,
}

impl<'f, 's> push::Push<'f, 's> for Push {
    type Error = Error;

    type Expr = Expr<'f, 's>;
    type Stmt = Stmt<'f, 's>;
    type Decl = Decl<'f, 's>;
    type Pat = Pat<'f, 's>;
    type Params = Params<'f, 's>;
    type FnBody = FnBody<'f, 's>;

    // PrimaryExpression :
    #[inline]
    fn this(span: Span<'f>) -> Result<Self::Expr, Self::Error> {
        Ok(Expr { span, kind: ExprKind::This })
    }
    #[inline]
    fn ident(span: Span<'f>, ident: Cow<'s, str>) -> Result<Self::Expr, Self::Error> {
        Ok(Expr { span, kind: ExprKind::Ident(ident) })
    }
    #[inline]
    fn null_lit(span: Span<'f>) -> Result<Self::Expr, Self::Error> {
        Ok(Expr { span, kind: ExprKind::Null })
    }
    #[inline]
    fn bool_lit(span: Span<'f>, value: bool) -> Result<Self::Expr, Self::Error> {
        Ok(Expr { span, kind: ExprKind::Bool(value) })
    }
    #[inline]
    fn int_lit(span: Span<'f>, value: u64) -> Result<Self::Expr, Self::Error> {
        Ok(Expr { span, kind: ExprKind::Int(value) })
    }
    #[inline]
    fn float_lit(span: Span<'f>, value: f64) -> Result<Self::Expr, Self::Error> {
        Ok(Expr { span, kind: ExprKind::Float(value) })
    }
    #[inline]
    fn str_lit(span: Span<'f>, value: Cow<'s, str>) -> Result<Self::Expr, Self::Error> {
        Ok(Expr { span, kind: ExprKind::Str(value) })
    }
    #[inline]
    fn reg_exp_lit(span: Span<'f>, source: &'s str, flags: &'s str) -> Result<Self::Expr, Self::Error> {
        unimplemented!()
    }
    #[inline]
    fn template_lit<P: Content<
        Continue = Span<'f>,
        Goal = Vec<TemplatePart<'s, Self::Expr>>,
        Deferred = DeferredTemplateParts<'f, 's, Self>,
        Error = Self::Error,
    >>(head: Cow<'s, str>, parts: P) -> Result<Self::Expr, Self::Error> {
        let (span, parts) = parts.defer()?;
        Ok(Expr {
            span,
            kind: ExprKind::Template(head, parts),
        })
    }

    #[inline]
    fn array_lit<V: Content<
        Continue = Span<'f>,
        Goal = Vec<ArrayLitElement<Self::Expr>>,
        Deferred = DeferredArrayValues<'f, 's, Self>,
        Error = Self::Error,
    >>(values: V) -> Result<Self::Expr, Self::Error> {
        let (span, values) = values.defer()?;
        Ok(Expr {
            span,
            kind: ExprKind::Array(values),
        })
    }

    fn formal_params<F: Content<
        Continue = Span<'f>,
        Goal = Vec<FormalParameter<'s, Self::Pat, Self::Expr>>,
        Deferred = DeferredFormalParams<'f, 's, Self>,
        Error = Self::Error,
    >>(params: F) -> Result<Self::Params, Self::Error> {
        let (span, params) = params.defer()?;
        Ok(Params {
            span,
            params,
        })
    }
    fn fn_body<B: Content<
        Continue = Span<'f>,
        Goal = Vec<StmtListItem<Self::Stmt, Self::Decl>>,
        Deferred = DeferredBlock<'f, 's, Self>,
        Error = Self::Error,
    >>(body: B) -> Result<Self::FnBody, Self::Error> {
        let (span, body) = body.defer()?;
        Ok(FnBody {
            span,
            body,
        })
    }
    fn object_lit<D: Content<
        Continue = Span<'f>,
        Goal = Vec<PropertyDefinition<'s, Self::Expr, Self::Params, Self::FnBody>>,
        Deferred = DeferredObjectPropertyDefs<'f, 's, Self>,
        Error = Self::Error,
    >>(defs: D) -> Result<Self::Expr, Self::Error> {
        let (span, defs) = defs.defer()?;
        Ok(Expr {
            span,
            kind: ExprKind::Object(defs),
        })
    }

    // #[inline]
    // fn arrow_function<
    //     P: 'static + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'static + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, kind: ArrowFunctionKind, name: Option<Cow<'s, str>>, params: P, body: ArrowBody<Self::Expr, B>) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn function_expr<
    //     P: 'static + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'static + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, kind: FunctionKind, name: Option<Cow<'s, str>>, params: P, body: B) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn class_expr<
    //     M: 'static + IntoIterator<Item=ClassElement<'s, Self::Expr, F, B>>,
    //     F: 'static + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'static + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, name: Option<Cow<'s, str>>, extends: Option<Self::Expr>, body: M) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // MemberExpression :
    // #[inline]
    // fn access_bracket(span: Span<'f>, object: Self::Expr, prop: Self::Expr) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn access_dot(span: Span<'f>, object: Self::Expr, prop: Cow<str>) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn tagged_template<
    //     P: 'static + IntoIterator<Item=Cow<'s, str>>,
    //     V: 'static + IntoIterator<Item=Self::Expr>,
    // >(span: Span<'f>, tag: Self::Expr, lit_parts: P, values: V) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn new<
    //     A: 'static + IntoIterator<Item=Argument<Self::Expr>>,
    // >(span: Span<'f>, constructor: Self::Expr, arguments: A) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // SuperProperty :
    // #[inline]
    // fn super_dot(span: Span<'f>, prop: Cow<str>) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn super_bracket(span: Span<'f>, prop: Self::Expr) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn super_call<
    //     A: 'static + IntoIterator<Item=Argument<Self::Expr>>,
    // >(span: Span<'f>, arguments: A) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // MetaProperty :
    // #[inline]
    // fn new_dot_target(span: Span<'f>) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // CallExpression :
    // #[inline]
    // fn call<
    //     A: 'static + IntoIterator<Item=Argument<Self::Expr>>,
    // >(span: Span<'f>, function: Self::Expr, arguments: A) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // UpdateExpression :
    // #[inline]
    // fn unary_update(span: Span<'f>, pos: UpdatePos, op: UpdateOp, var: Self::Expr) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // UnaryExpression :
    // #[inline]
    // fn unary(span: Span<'f>, op: UnaryOp, expr: Self::Expr) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // ExponentiationExpression :
    // // MultiplicativeExpression :
    // // AdditiveExpression :
    // // ShiftExpression :
    // // RelationalExpression :
    // // EqualityExpression :
    // // BitwiseANDExpression :
    // // BitwiseXORExpression :
    // // BitwiseORExpression :
    // // LogicalANDExpression :
    // // LogicalORExpression :
    // // Expression :
    // #[inline]
    // fn binary(span: Span<'f>, op: BinaryOp, lhs: Self::Expr, rhs: Self::Expr) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // AssignmentExpression :
    // #[inline]
    // fn assign(span: Span<'f>, op: AssignOp, lhs: Self::Expr, rhs: Self::Expr) -> Result<Self::Expr, Self::Error> {
    //     unimplemented!()
    // }

    // // Statement :
    // #[inline]
    // fn block<
    //     S: 'static + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, stmts: S) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn var<
    //     D: 'static + IntoIterator<Item=Binding<'s, Self::Pat, Self::Expr>>,
    // >(span: Span<'f>, decls: D) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn empty(span: Span<'f>) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn expr_stmt(span: Span<'f>, expr: Self::Expr) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn if_(span: Span<'f>, cond: Self::Expr, then: Self::Stmt, otherwise: Option<Self::Stmt>) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn do_while(span: Span<'f>, body: Self::Stmt, cond: Self::Expr) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn while_(span: Span<'f>, cond: Self::Expr, body: Self::Stmt) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn for_<
    //     D: 'static + IntoIterator<Item=Binding<'s, Self::Pat, Self::Expr>>,
    // >(span: Span<'f>, init: ForInit<Self::Expr, D>, cond: Option<Self::Expr>, update: Option<Self::Expr>, body: Self::Stmt) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn for_iter(span: Span<'f>, kind: IterKind, lhs: ForLhs<Self::Expr, Self::Pat>, rhs: Self::Expr, body: Self::Stmt) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn continue_(span: Span<'f>, label: Option<Cow<'s, str>>) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn break_(span: Span<'f>, label: Option<Cow<'s, str>>) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn return_(span: Span<'f>, expr: Option<Self::Expr>) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn with(span: Span<'f>, expr: Self::Expr, body: Self::Stmt) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn switch<
    //     C: 'static + IntoIterator<Item=CaseClause<Self::Expr, L>>,
    //     L: 'static + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, expr: Self::Expr, clauses: C) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn labeled(span: Span<'f>, label: Cow<'s, str>, item: LabeledItem<Self::Stmt, Self::Decl>) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn throw(span: Span<'f>, expr: Self::Expr) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn try<
    //     B: 'static + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, try: B, catch: Option<Catch<'s, Self::Pat, B>>, finally: Option<B>) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn debugger(span: Span<'f>) -> Result<Self::Stmt, Self::Error> {
    //     unimplemented!()
    // }

    // // Declaration :
    // #[inline]
    // fn function_decl<
    //     P: 'static + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'static + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, kind: FunctionKind, name: Cow<'s, str>, params: P, body: B) -> Result<Self::Decl, Self::Error> {
    //     unimplemented!()
    // }
    // #[inline]
    // fn class_decl<
    //     M: 'static + IntoIterator<Item=ClassElement<'s, Self::Expr, F, B>>,
    //     F: 'static + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'static + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, name: Cow<'s, str>, extends: Option<Self::Expr>, body: M) -> Result<Self::Decl, Self::Error> {
    //     unimplemented!()
    // }
}
