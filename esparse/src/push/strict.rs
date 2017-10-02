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
    // TODO Template(Iterator<Item=Cow<'s, str>>, Iterator<Item=Expr<'f, 's>>),
    Array(Vec<ArrayLitElement<Expr<'f, 's>>>),
}

#[derive(Debug)]
pub struct Stmt<'f, 's> {
    pub kind: StmtKind<'s>,
    pub span: Span<'f>
}

#[derive(Debug)]
pub enum StmtKind<'s> {
    Todo(&'s str),
    // TODO
}

#[derive(Debug)]
pub struct Decl<'f, 's> {
    pub kind: DeclKind<'s>,
    pub span: Span<'f>
}

#[derive(Debug)]
pub enum DeclKind<'s> {
    Todo(&'s str),
    // TODO
}

#[derive(Debug)]
pub struct Pat<'f, 's> {
    pub kind: PatKind<'s>,
    pub span: Span<'f>
}

#[derive(Debug)]
pub enum PatKind<'s> {
    Todo(&'s str),
    // TODO
}

impl<'f, 's> push::Push<'f, 's> for Push {
    type Error = Error;

    type Expr = Expr<'f, 's>;
    type Stmt = Stmt<'f, 's>;
    type Decl = Decl<'f, 's>;
    type Pat = Pat<'f, 's>;

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
    // #[inline]
    // fn template_lit<
    //     P: 'static + IntoIterator<Item=Cow<'s, str>>,
    //     V: 'static + IntoIterator<Item=Self::Expr>,
    // >(span: Span<'f>, lit_parts: P, values: V) -> Result<Self::Expr, Self::Error> {
    //     Ok(Expr {
    //         span,
    //         kind: ExprKind::Template(
    //             lit_parts.into_iter(),
    //             values.into_iter(),
    //         ),
    //     })
    // }

    #[inline]
    fn array_lit<
        V: Content<Goal=(Span<'f>, Vec<ArrayLitElement<Self::Expr>>), Deferred=(Span<'f>, DeferredArrayValues<'f, 's, Self>), Error=Self::Error>,
    >(values: V) -> Result<Self::Expr, Self::Error> {
        let (span, values) = values.parse()?;
        Ok(Expr {
            span,
            kind: ExprKind::Array(values),
        })
    }
}
