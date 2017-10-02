#![allow(missing_docs)]

use std::num;
use std::borrow::Cow;
use std::marker::PhantomData;

use ast::{Span, SpanT};
use skip;
use lex::{self, Tt};

pub mod lazy;
// pub mod strict;

pub type Lazy<'f, 's> = Parser<'f, 's, lazy::Push>;
// pub type Strict<'f, 's> = Parser<'f, 's, strict::Push>;

#[derive(Debug)]
pub enum ArrayLitElement<E> {
    Elision,
    Item(E),
    Spread(E),
}

#[derive(Debug)]
pub struct TemplatePart<'s, E> {
    pub expr: E,
    pub lit: Cow<'s, str>,
}

#[derive(Debug)]
pub enum Argument<E> {
    Item(E),
    Spread(E),
}

#[derive(Debug)]
pub enum PropertyDefinition<'s, E, F, B> {
    Ident(Cow<'s, str>),
    Property(PropertyName<'s, E>, E),
    Method(MethodDefinition<'s, E, F, B>),
}

#[derive(Debug)]
pub enum MethodDefinition<'s, E, F, B> {
    Method(FunctionKind, PropertyName<'s, E>, F, B),
    Get(PropertyName<'s, E>, B),
    Set(PropertyName<'s, E>, F, B),
}

#[derive(Debug)]
pub enum PropertyName<'s, E> {
    Literal(Cow<'s, str>),
    Computed(E),
}

#[derive(Debug)]
pub enum ClassElement<'s, E, F, B> {
    Method(MethodDefinition<'s, E, F, B>),
    Static(MethodDefinition<'s, E, F, B>),
}

#[derive(Debug)]
pub enum UpdateOp {
    Incr,
    Decr,
}

#[derive(Debug)]
pub enum UpdatePos {
    Prefix,
    Postfix,
}

#[derive(Debug)]
pub enum UnaryOp {
    Delete,
    Void,
    Typeof,
    Plus,
    Minus,
    BitInv,
    Not,
    Await,
}

#[derive(Debug)]
pub enum BinaryOp {
    Exp,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    Ushr,
    Lt,
    Gt,
    Lte,
    Gte,
    Instanceof,
    In,
    AbEq,
    AbNe,
    StEq,
    StNe,
    BitAnd,
    BitXor,
    BitOr,
    LogAnd,
    LogOr,
    Comma,
}

#[derive(Debug)]
pub enum AssignOp {
    Assign,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    Ushr,
    BitAnd,
    BitXor,
    BitOr,
    Exp,
}

#[derive(Debug)]
pub enum StmtListItem<S, D> {
    Stmt(S),
    Decl(D),
}

#[derive(Debug)]
pub enum Binding<'s, P, E> {
    Ident(Cow<'s, str>, Option<E>),
    Pat(P, E),
}

#[derive(Debug)]
pub enum ForInit<E, D> {
    Expr(E),
    Var(D),
    Let(D),
    Const(D),
}

#[derive(Debug)]
pub enum ForBinding<'s, P> {
    Ident(Cow<'s, str>),
    Pat(P),
}

#[derive(Debug)]
pub enum ForLhs<'s, E, P> {
    Var(ForBinding<'s, P>),
    VarInit(ForBinding<'s, P>, E),
    Let(ForBinding<'s, P>),
    Const(ForBinding<'s, P>),
    Expr(E),
}

#[derive(Debug)]
pub enum IterKind {
    Enumerate,
    Iterate,
}

#[derive(Debug)]
pub enum CaseClause<E, L> {
    Case(E, L),
    Default(L),
}

#[derive(Debug)]
pub enum LabeledItem<S, D> {
    Stmt(S),
    FuncDecl(D),
}

#[derive(Debug)]
pub struct Catch<'s, P, B> {
    param: CatchParameter<'s, P>,
    block: B,
}

#[derive(Debug)]
pub enum CatchParameter<'s, P> {
    Ident(Cow<'s, str>),
    Pat(P),
}

#[derive(Debug)]
pub enum FormalParameter<'s, P, E> {
    Item(ParamBinding<'s, P>, Option<E>),
    Rest(ParamBinding<'s, P>),
}

#[derive(Debug)]
pub enum ParamBinding<'s, P> {
    Ident(Cow<'s, str>),
    Pat(P),
}

#[derive(Debug)]
pub enum FunctionKind {
    Function,
    Generator,
    Async,
}

#[derive(Debug)]
pub enum ArrowFunctionKind {
    Function,
    Async,
}

#[derive(Debug)]
pub enum ArrowBody<E, B> {
    Expr(E),
    Block(B),
}

pub trait Content: Sized {
    type Continue;
    type Goal;
    type Deferred: Deferred<Error=Self::Error>;
    type Error;

    fn defer(self) -> Result<(Self::Continue, Self::Deferred), Self::Error>;
    fn parse(self) -> Result<(Self::Continue, Self::Goal), Self::Error>;

    #[inline]
    fn skip(self) -> Result<Self::Continue, Self::Error> {
        self.defer().map(|p| p.0)
    }
}

pub trait Deferred {
    type Goal;
    type Error;

    fn parse(self) -> Result<Self::Goal, Self::Error>;
}
impl<'f, D: Deferred> Deferred for (Span<'f>, D) {
    type Goal = D::Goal;
    type Error = D::Error;

    fn parse(self) -> Result<Self::Goal, Self::Error> {
        self.1.parse()
    }
}

// pub trait DeferredParse {
//     type Goal;
//     // TODO elide?
//     fn parse<'f, 's, 'p, P: Push<'f, 's> + 'p>(lexer: &mut lex::Lexer<'f, 's>, push: &'p P) -> Result<Self::Goal, P::Error>;
// }

// pub trait DeferredSkip {
//     type Error;
//     // TODO elide?
//     fn skip<'f, 's>(lexer: &mut lex::Lexer<'f, 's>) -> Result<(), Self::Error>;
// }

// #[derive(Debug)]
// pub struct Deferred<'f, 's, 'l, 'p, P, S, D> where
// 'f: 'l,
// 's: 'l,
// P: Push<'f, 's> + 'p,
// S: DeferredSkip,
// D: DeferredParse {
//     pub push: &'p P,
//     pub lexer: &'l mut lex::Lexer<'f, 's>,
//     // pub input: &'s str,
//     // pub span: Span<'f>,
//     pub phantom: PhantomData<(S, D)>,
// }

// impl<'f, 's, 'l, 'p, P, S, D> Content<'f, 's> for Deferred<'f, 's, 'l, 'p, P, S, D> where
// 'f: 'l,
// 's: 'l,
// P: Push<'f, 's> + 'p,
// S: DeferredSkip,
// D: DeferredParse {
//     type Goal = D::Goal;
//     type Error = P::Error;

//     fn skip(self) -> Result<(), Self::Error> {
//         S::skip(self.lexer)
//     }
//     fn parse(self) -> Result<Self::Goal, Self::Error> {
//         D::parse(self.lexer, self.push)
//     }
// }

// #[derive(Debug)]
// pub struct InputSpan<'f, 's> {
//     pub input: &'s str,
//     pub span: Span<'f>,
// }

// pub trait GetInputSpan<'f, 's> {
//     fn input_span(&self, span: Span<'f>) -> InputSpan<'f, 's>;
// }

// impl<'f, 's> GetInputSpan<'f, 's> for lex::Lexer<'f, 's> {
//     fn input_span(&self, span: Span<'f>) -> InputSpan<'f, 's> {
//         InputSpan {
//             input: &self.input()[span.start.pos..span.end.pos],
//             span,
//         }
//     }
// }

pub trait Push<'f, 's> {
    type Error:
        From<num::ParseIntError> +
        From<num::ParseFloatError> +
        From<lex::ParseStrLitError> +
        From<skip::Error> +
        From<ParseError>;

    type Expr;
    type Stmt;
    type Decl;
    type Pat;
    type Params;
    type FnBody;

    // PrimaryExpression :
    fn this(span: Span<'f>) -> Result<Self::Expr, Self::Error>;
    fn ident(span: Span<'f>, ident: Cow<'s, str>) -> Result<Self::Expr, Self::Error>;
    fn null_lit(span: Span<'f>) -> Result<Self::Expr, Self::Error>;
    fn bool_lit(span: Span<'f>, value: bool) -> Result<Self::Expr, Self::Error>;
    fn int_lit(span: Span<'f>, value: u64) -> Result<Self::Expr, Self::Error>;
    fn float_lit(span: Span<'f>, value: f64) -> Result<Self::Expr, Self::Error>;
    fn str_lit(span: Span<'f>, value: Cow<'s, str>) -> Result<Self::Expr, Self::Error>;
    fn reg_exp_lit(span: Span<'f>, source: &'s str, flags: &'s str) -> Result<Self::Expr, Self::Error>;
    fn template_lit<P: Content<
        Continue = Span<'f>,
        Goal = Vec<TemplatePart<'s, Self::Expr>>,
        Deferred = DeferredTemplateParts<'f, 's, Self>,
        Error = Self::Error,
    >>(head: Cow<'s, str>, parts: P) -> Result<Self::Expr, Self::Error>;

    fn array_lit<V: Content<
        Continue = Span<'f>,
        Goal = Vec<ArrayLitElement<Self::Expr>>,
        Deferred = DeferredArrayValues<'f, 's, Self>,
        Error = Self::Error,
    >>(values: V) -> Result<Self::Expr, Self::Error>;

    fn formal_params<F: Content<
        Continue = Span<'f>,
        Goal = Vec<FormalParameter<'s, Self::Pat, Self::Expr>>,
        Deferred = DeferredFormalParams<'f, 's, Self>,
        Error = Self::Error,
    >>(params: F) -> Result<Self::Params, Self::Error>;
    fn fn_body<B: Content<
        Continue = Span<'f>,
        Goal = Vec<StmtListItem<Self::Stmt, Self::Decl>>,
        Deferred = DeferredBlock<'f, 's, Self>,
        Error = Self::Error,
    >>(body: B) -> Result<Self::FnBody, Self::Error>;
    fn object_lit<D: Content<
        Continue = Span<'f>,
        Goal = Vec<PropertyDefinition<'s, Self::Expr, Self::Params, Self::FnBody>>,
        Deferred = DeferredObjectPropertyDefs<'f, 's, Self>,
        Error = Self::Error,
    >>(defs: D) -> Result<Self::Expr, Self::Error>;

    // fn arrow_function<
    //     P: 'i + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'i + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, kind: ArrowFunctionKind, name: Option<Cow<'s, str>>, params: P, body: ArrowBody<Self::Expr, B>) -> Result<Self::Expr, Self::Error>;
    // fn function_expr<
    //     P: 'i + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'i + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, kind: FunctionKind, name: Option<Cow<'s, str>>, params: P, body: B) -> Result<Self::Expr, Self::Error>;
    // fn class_expr<
    //     M: 'i + IntoIterator<Item=ClassElement<'s, Self::Expr, F, B>>,
    //     F: 'i + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'i + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, name: Option<Cow<'s, str>>, extends: Option<Self::Expr>, body: M) -> Result<Self::Expr, Self::Error>;

    // // MemberExpression :
    // fn access_bracket(span: Span<'f>, object: Self::Expr, prop: Self::Expr) -> Result<Self::Expr, Self::Error>;
    // fn access_dot(span: Span<'f>, object: Self::Expr, prop: Cow<str>) -> Result<Self::Expr, Self::Error>;
    // fn tagged_template<
    //     P: 'i + IntoIterator<Item=Cow<'s, str>>,
    //     V: 'i + IntoIterator<Item=Self::Expr>,
    // >(span: Span<'f>, tag: Self::Expr, lit_parts: P, values: V) -> Result<Self::Expr, Self::Error>;
    // fn new<
    //     A: 'i + IntoIterator<Item=Argument<Self::Expr>>,
    // >(span: Span<'f>, constructor: Self::Expr, arguments: A) -> Result<Self::Expr, Self::Error>;

    // // SuperProperty :
    // fn super_dot(span: Span<'f>, prop: Cow<str>) -> Result<Self::Expr, Self::Error>;
    // fn super_bracket(span: Span<'f>, prop: Self::Expr) -> Result<Self::Expr, Self::Error>;
    // fn super_call<
    //     A: 'i + IntoIterator<Item=Argument<Self::Expr>>,
    // >(span: Span<'f>, arguments: A) -> Result<Self::Expr, Self::Error>;

    // // MetaProperty :
    // fn new_dot_target(span: Span<'f>) -> Result<Self::Expr, Self::Error>;

    // // CallExpression :
    // fn call<
    //     A: 'i + IntoIterator<Item=Argument<Self::Expr>>,
    // >(span: Span<'f>, function: Self::Expr, arguments: A) -> Result<Self::Expr, Self::Error>;

    // // UpdateExpression :
    // fn unary_update(span: Span<'f>, pos: UpdatePos, op: UpdateOp, var: Self::Expr) -> Result<Self::Expr, Self::Error>;

    // // UnaryExpression :
    // fn unary(span: Span<'f>, op: UnaryOp, expr: Self::Expr) -> Result<Self::Expr, Self::Error>;

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
    // fn binary(span: Span<'f>, op: BinaryOp, lhs: Self::Expr, rhs: Self::Expr) -> Result<Self::Expr, Self::Error>;

    // // AssignmentExpression :
    // fn assign(span: Span<'f>, op: AssignOp, lhs: Self::Expr, rhs: Self::Expr) -> Result<Self::Expr, Self::Error>;

    // // Statement :
    // fn block<
    //     S: 'i + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, stmts: S) -> Result<Self::Stmt, Self::Error>;
    // fn var<
    //     D: 'i + IntoIterator<Item=Binding<'s, Self::Pat, Self::Expr>>,
    // >(span: Span<'f>, decls: D) -> Result<Self::Stmt, Self::Error>;
    // fn empty(span: Span<'f>) -> Result<Self::Stmt, Self::Error>;
    // fn expr_stmt(span: Span<'f>, expr: Self::Expr) -> Result<Self::Stmt, Self::Error>;
    // fn if_(span: Span<'f>, cond: Self::Expr, then: Self::Stmt, otherwise: Option<Self::Stmt>) -> Result<Self::Stmt, Self::Error>;
    // fn do_while(span: Span<'f>, body: Self::Stmt, cond: Self::Expr) -> Result<Self::Stmt, Self::Error>;
    // fn while_(span: Span<'f>, cond: Self::Expr, body: Self::Stmt) -> Result<Self::Stmt, Self::Error>;
    // fn for_<
    //     D: 'i + IntoIterator<Item=Binding<'s, Self::Pat, Self::Expr>>,
    // >(span: Span<'f>, init: ForInit<Self::Expr, D>, cond: Option<Self::Expr>, update: Option<Self::Expr>, body: Self::Stmt) -> Result<Self::Stmt, Self::Error>;
    // fn for_iter(span: Span<'f>, kind: IterKind, lhs: ForLhs<Self::Expr, Self::Pat>, rhs: Self::Expr, body: Self::Stmt) -> Result<Self::Stmt, Self::Error>;
    // fn continue_(span: Span<'f>, label: Option<Cow<'s, str>>) -> Result<Self::Stmt, Self::Error>;
    // fn break_(span: Span<'f>, label: Option<Cow<'s, str>>) -> Result<Self::Stmt, Self::Error>;
    // fn return_(span: Span<'f>, expr: Option<Self::Expr>) -> Result<Self::Stmt, Self::Error>;
    // fn with(span: Span<'f>, expr: Self::Expr, body: Self::Stmt) -> Result<Self::Stmt, Self::Error>;
    // fn switch<
    //     C: 'i + IntoIterator<Item=CaseClause<Self::Expr, L>>,
    //     L: 'i + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, expr: Self::Expr, clauses: C) -> Result<Self::Stmt, Self::Error>;
    // fn labeled(span: Span<'f>, label: Cow<'s, str>, item: LabeledItem<Self::Stmt, Self::Decl>) -> Result<Self::Stmt, Self::Error>;
    // fn throw(span: Span<'f>, expr: Self::Expr) -> Result<Self::Stmt, Self::Error>;
    // fn try<
    //     B: 'i + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, try: B, catch: Option<Catch<'s, Self::Pat, B>>, finally: Option<B>) -> Result<Self::Stmt, Self::Error>;
    // fn debugger(span: Span<'f>) -> Result<Self::Stmt, Self::Error>;

    // // Declaration :
    // fn function_decl<
    //     P: 'i + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'i + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, kind: FunctionKind, name: Cow<'s, str>, params: P, body: B) -> Result<Self::Decl, Self::Error>;
    // fn class_decl<
    //     M: 'i + IntoIterator<Item=ClassElement<'s, Self::Expr, F, B>>,
    //     F: 'i + IntoIterator<Item=FormalParameter<'s, Self::Pat, Self::Expr>>,
    //     B: 'i + IntoIterator<Item=StmtListItem<Self::Stmt, Self::Decl>>,
    // >(span: Span<'f>, name: Cow<'s, str>, extends: Option<Self::Expr>, body: M) -> Result<Self::Decl, Self::Error>;
}

#[derive(Debug)]
pub struct Parser<'f, 's, P: ?Sized> {
    lexer: lex::Lexer<'f, 's>,
    _phantom: PhantomData<P>,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ExprFlags {
    pub in_: bool,
    pub yield_: bool,
    pub await: bool,
}

#[derive(Debug)]
pub struct ParseError {
    pub span: SpanT<String>,
    pub kind: ParseErrorKind,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    ExpectedArrayContinue,
    ExpectedTemplateContinue,
}

macro_rules! expected {
    ($span:expr, $kind:expr) => {
        return Err(From::from(ParseError {
            span: $span.with_owned(),
            kind: $kind,
        }))
    }
}

pub const PREC_COMMA: usize = 10;
pub const PREC_ASSIGN: usize = 20;
pub const PREC_CONDITION: usize = 30;
pub const PREC_LOG_OR: usize = 40;
pub const PREC_LOG_AND: usize = 50;
pub const PREC_BIT_OR: usize = 60;
pub const PREC_BIT_XOR: usize = 70;
pub const PREC_BIT_AND: usize = 80;
pub const PREC_EQ: usize = 90;
pub const PREC_REL: usize = 100;
pub const PREC_SHIFT: usize = 110;
pub const PREC_ADD: usize = 120;
pub const PREC_MUL: usize = 130;
pub const PREC_EXP: usize = 140;
pub const PREC_UNARY: usize = 150;
pub const PREC_UPDATE: usize = 160;
pub const PREC_NEW: usize = 160;
pub const PREC_MEMBER: usize = 170;

impl<'f, 's, P: Push<'f, 's> + ?Sized> Parser<'f, 's, P> {
    #[inline]
    pub fn with_lexer(lexer: lex::Lexer<'f, 's>) -> Self {
        Parser {
            lexer,
            _phantom: PhantomData,
        }
    }

    #[inline]
    pub fn new(file_name: &'f str, input: &'s str) -> Self {
        Self::with_lexer(lex::Lexer::new(file_name, input))
    }

    #[inline]
    pub fn new_unnamed(input: &'s str) -> Self {
        Self::with_lexer(lex::Lexer::new_unnamed(input))
    }

    #[inline]
    pub fn parse_expr_prec(&mut self, prec: usize, flags: ExprFlags) -> Result<P::Expr, P::Error> {
        // TODO preops
        let expr = self.parse_expr_primary(flags);
        // TODO ops
        expr
    }

    #[inline]
    pub fn parse_expr_primary(&mut self, flags: ExprFlags) -> Result<P::Expr, P::Error> {
        //- PrimaryExpression[Yield, Await] :
        //-     this
        //-     IdentifierReference[?Yield, ?Await]
        //-     Literal
        //-     ArrayLiteral[?Yield, ?Await]
        //-     ObjectLiteral[?Yield, ?Await]
        //-     FunctionExpression
        //-     ClassExpression[?Yield, ?Await]
        //-     GeneratorExpression
        //-     AsyncFunctionExpression
        //-     RegularExpressionLiteral
        //-     TemplateLiteral[?Yield, ?Await]
        //-     CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
        eat!(self.lexer => it,
            //- this
            Tt::This => P::this(it.span),

            //- IdentifierReference[Yield, Await] :
            //-     Identifier
            //-     [~Yield] yield
            //-     [~Await] await
            Tt::Id(val) => {
                // TODO un-backslash
                P::ident(it.span, Cow::Borrowed(val))
            },
            Tt::Yield if !flags.yield_ => {
                P::ident(it.span, Cow::Borrowed("yield"))
            },
            Tt::Await if !flags.await => {
                P::ident(it.span, Cow::Borrowed("await"))
            },

            //- Literal :
            //-     NullLiteral
            //-     BooleanLiteral
            //-     NumericLiteral
            //-     StringLiteral

            //- NullLiteral ::
            //-     `null`
            Tt::Null => P::null_lit(it.span),

            //- BooleanLiteral ::
            //-     `true`
            //-     `false`
            Tt::False => P::bool_lit(it.span, false),
            Tt::True => P::bool_lit(it.span, true),

            //- NumericLiteral ::
            //-     DecimalLiteral
            //-     BinaryIntegerLiteral
            //-     OctalIntegerLiteral
            //-     HexIntegerLiteral
            Tt::NumLitDec(src) => {
                P::float_lit(it.span, src.parse::<f64>()?)
            },
            Tt::NumLitBin(src) => {
                P::int_lit(it.span, u64::from_str_radix(src, 2)?)
            },
            Tt::NumLitOct(src) => {
                P::int_lit(it.span, u64::from_str_radix(src, 8)?)
            },
            Tt::NumLitHex(src) => {
                P::int_lit(it.span, u64::from_str_radix(src, 16)?)
            },

            //- StringLiteral :: …
            Tt::StrLitSgl(src) |
            Tt::StrLitDbl(src) => {
                P::str_lit(it.span, lex::str_lit_value(src)?)
            },

            //- ArrayLiteral[Yield, Await] :
            //-     `[` Elision? `]`
            //-     `[` ElementList[?Yield, ?Await] `]`
            //-     `[` ElementList[?Yield, ?Await] `,` Elision? `]`
            Tt::Lbracket => {
                P::array_lit(ArrayValues {
                    start: it.span,
                    parser: self,
                    flags,
                })
            },

            //- TemplateLiteral[Yield, Await] :
            //-     NoSubstitutionTemplate
            //-     TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await]
            Tt::TemplateStart(head_src) => {
                P::template_lit(lex::template_start_value(head_src)?, TemplateParts {
                    start: it.span,
                    parser: self,
                    flags,
                })
            },

            //- ObjectLiteral[Yield, Await] :
            //-     `{` `}`
            //-     `{` PropertyDefinitionList[?Yield, ?Await] `}`
            //-     `{` PropertyDefinitionList[?Yield, ?Await] `,` `}`
            Tt::Lbrace => {
                P::object_lit(ObjectPropertyDefs {
                    start: it.span,
                    parser: self,
                    flags,
                })
            },

            _ => unimplemented!(),
        )
    }

    #[inline]
    pub fn parse_array_literal(&mut self, mut flags: ExprFlags) -> Result<(Span<'f>, Vec<ArrayLitElement<P::Expr>>), P::Error> {
        //- ArrayLiteral[Yield, Await] :
        //-     `[` Elision? `]`
        //-     `[` ElementList[?Yield, ?Await] `]`
        //-     `[` ElementList[?Yield, ?Await] `,` Elision? `]`
        let mut values = Vec::new();
        flags.in_ = true;
        loop {
            eat!(self.lexer => tok,
                Tt::Comma => {
                    values.push(ArrayLitElement::Elision);
                },
                Tt::DotDotDot => {
                    let expr = self.parse_expr_prec(PREC_COMMA + 1, flags)?;
                    values.push(ArrayLitElement::Spread(expr));
                    eat!(self.lexer => tok,
                        Tt::Rbracket => {
                            return Ok((tok.span, values))
                        },
                        Tt::Comma => continue,
                        _ => expected!(tok.span, ParseErrorKind::ExpectedArrayContinue),
                    )
                },
                Tt::Rbracket => {
                    return Ok((tok.span, values))
                },
                _ => {
                    let expr = self.parse_expr_prec(PREC_COMMA + 1, flags)?;
                    values.push(ArrayLitElement::Item(expr));
                    eat!(self.lexer => tok,
                        Tt::Rbracket => {
                            return Ok((tok.span, values))
                        },
                        Tt::Comma => continue,
                        _ => expected!(tok.span, ParseErrorKind::ExpectedArrayContinue),
                    )
                }
            );
        }
    }

    #[inline]
    pub fn parse_template(&mut self, mut flags: ExprFlags) -> Result<(Span<'f>, Vec<TemplatePart<'s, P::Expr>>), P::Error> {
        //- TemplateLiteral[Yield, Await] :
        //-     NoSubstitutionTemplate
        //-     TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await]
        //- TemplateSpans[Yield, Await] :
        //-     TemplateTail
        //-     TemplateMiddleList[?Yield, ?Await] TemplateTail
        //- TemplateMiddleList[Yield, Await] :
        //-     TemplateMiddle Expression[+In, ?Yield, ?Await]
        //-     TemplateMiddleList[?Yield, ?Await] TemplateMiddle Expression[+In, ?Yield, ?Await]
        let mut parts = Vec::new();
        flags.in_ = true;
        loop {
            let expr = self.parse_expr_prec(0, flags)?;
            eat!(self.lexer => tok,
                Tt::TemplateMiddle(lit_src) => {
                    parts.push(TemplatePart {
                        expr,
                        lit: lex::template_middle_value(lit_src)?,
                    });
                },
                Tt::TemplateEnd(lit_src) => {
                    parts.push(TemplatePart {
                        expr,
                        lit: lex::template_end_value(lit_src)?,
                    });
                    return Ok((tok.span, parts))
                },
                _ => expected!(tok.span, ParseErrorKind::ExpectedTemplateContinue),
            );
        }
    }

    #[inline]
    pub fn parse_object_property_defs(&mut self, mut flags: ExprFlags) -> Result<(Span<'f>, Vec<PropertyDefinition<'s, P::Expr, P::Params, P::FnBody>>)> {
        let mut defs = Vec::new();
        flags.in_ = true;
        loop {

        }
    }
}

macro_rules! deferred_pair {
    (
        $Name:ident $DeferredName:ident<$f:tt, $s:tt, $P:tt> => $Goal:ty $(,)*,
        state { $($sname:ident : $sty:tt),* $(,)* }
        skip($sself:ident, $sparser:ident) { $($skip:tt)* }
        parse($pself:ident, $pparser:ident) { $($parse:tt)* }
    ) => {
        #[derive(Debug)]
        pub struct $Name<$f: 'r, $s: 'r, 'r, $P: Push<$f, $s> + ?Sized + 'r> {
            parser: &'r mut Parser<$f, $s, $P>,
            start: Span<$f>,
            $($sname : $sty),*
        }
        impl<$f, $s, 'r, $P: Push<$f, $s> + ?Sized + 'r> Content for $Name<$f, $s, 'r, $P> {
            type Continue = Span<$f>;
            type Goal = $Goal;
            type Deferred = $DeferredName<$f, $s, $P>;
            type Error = $P::Error;

            fn defer($sself) -> Result<(Self::Continue, Self::Deferred), P::Error> {
                let clone = $sself.parser.lexer.clone();
                let $sparser = $sself.parser;
                let end = {
                    $($skip)*
                };
                Ok(($sself.start.extend_to_cover(end), $DeferredName {
                    lexer: clone,
                    _phantom: PhantomData,
                    $($sname: $sself.$sname),*
                }))
            }
            #[inline]
            fn parse($pself) -> Result<(Self::Continue, Self::Goal), P::Error> {
                let $pparser = $pself.parser;
                let (end, goal) = {
                    $($parse)*
                }?;
                Ok(($pself.start.extend_to_cover(end), goal))
            }
        }

        #[derive(Debug)]
        pub struct $DeferredName<$f, $s, $P: ?Sized> {
            lexer: lex::Lexer<$f, $s>,
            _phantom: PhantomData<P>,
            $($sname : $sty),*
        }

        impl<$f, $s, $P: Push<$f, $s> + ?Sized> Deferred for $DeferredName<$f, $s, $P> {
            type Goal = $Goal;
            type Error = $P::Error;

            #[inline]
            fn parse($pself) -> Result<Self::Goal, Self::Error> {
                let ref mut $pparser = Parser::<$P>::with_lexer($pself.lexer);
                let (_, goal): (Span<$f>, _) = {
                    $($parse)*
                }?;
                Ok(goal)
            }
        }
    };
}

deferred_pair! {
    ArrayValues DeferredArrayValues<'f, 's, P> =>
        Vec<ArrayLitElement<P::Expr>>,
    state {
        flags: ExprFlags,
    }
    skip(self, parser) {
        skip::balanced_brackets(&mut parser.lexer, 1)?.span
    }
    parse(self, parser) {
        parser.parse_array_literal(self.flags)
    }
}

deferred_pair! {
    TemplateParts DeferredTemplateParts<'f, 's, P> =>
        Vec<TemplatePart<'s, P::Expr>>,
    state {
        flags: ExprFlags,
    }
    skip(self, parser) {
        skip::balanced_templates(&mut parser.lexer, 1)?.span
    }
    parse(self, parser) {
        parser.parse_template(self.flags)
    }
}

deferred_pair! {
    FormalParams DeferredFormalParams<'f, 's, P> =>
        Vec<FormalParameter<'s, P::Pat, P::Expr>>,
    state {
        flags: ExprFlags,
    }
    skip(self, parser) {
        skip::balanced_parens(&mut parser.lexer, 1)?.span
    }
    parse(self, parser) {
        Ok(unimplemented!())
        // parser.parse_formal_params(self.flags)
    }
}

deferred_pair! {
    Block DeferredBlock<'f, 's, P> =>
        Vec<StmtListItem<P::Stmt, P::Decl>>,
    state {
        flags: ExprFlags,
    }
    skip(self, parser) {
        skip::balanced_braces(&mut parser.lexer, 1)?.span
    }
    parse(self, parser) {
        Ok(unimplemented!())
        // parser.parse_block(self.flags)
    }
}

deferred_pair! {
    ObjectPropertyDefs DeferredObjectPropertyDefs<'f, 's, P> =>
        Vec<PropertyDefinition<'s, P::Expr, P::Params, P::FnBody>>,
    state {
        flags: ExprFlags,
    }
    skip(self, parser) {
        skip::balanced_braces(&mut parser.lexer, 1)?.span
    }
    parse(self, parser) {
        parser.parse_object_property_defs(self.flags)
    }
}

// #[derive(Debug)]
// pub struct ArrayValues<'f: 'r, 's: 'r, 'r, P: Push<'f, 's> + ?Sized + 'r> {
//     parser: &'r mut Parser<'f, 's, P>,
//     start: Span<'f>,
//     flags: ExprFlags,
// }
// impl<'f, 's, 'r, P: Push<'f, 's> + ?Sized + 'r> Content for ArrayValues<'f, 's, 'r, P> {
//     type Continue = Span<'f>;
//     type Goal = Vec<ArrayLitElement<P::Expr>>;
//     type Deferred = DeferredArrayValues<'f, 's, P>;
//     type Error = P::Error;

//     fn defer(self) -> Result<(Self::Continue, Self::Deferred), P::Error> {
//         let clone = self.parser.lexer.clone();
//         let end = skip::balanced_brackets(&mut self.parser.lexer, 1)?.span;
//         Ok((
//             self.start.extend_to_cover(end),
//             DeferredArrayValues {
//                 lexer: clone,
//                 flags: self.flags,
//                 _phantom: PhantomData,
//             },
//         ))
//     }
//     fn parse(self) -> Result<(Self::Continue, Self::Goal), Self::Error> {
//         let (end, els) = self.parser.parse_array_literal(self.flags)?;
//         Ok((
//             self.start.extend_to_cover(end),
//             els,
//         ))
//     }
// }

// #[derive(Debug)]
// pub struct DeferredArrayValues<'f, 's, P: ?Sized> {
//     lexer: lex::Lexer<'f, 's>,
//     flags: ExprFlags,
//     _phantom: PhantomData<P>,
// }

// impl<'f, 's, P: Push<'f, 's> + ?Sized> Deferred for DeferredArrayValues<'f, 's, P> {
//     type Goal = Vec<ArrayLitElement<P::Expr>>;
//     type Error = P::Error;

//     fn parse(self) -> Result<Self::Goal, Self::Error> {
//         Ok(Parser::<P>::with_lexer(self.lexer)
//         .parse_array_literal(self.flags)?.1)
//     }
// }

// #[derive(Debug)]
// pub struct TemplateParts<'f: 'r, 's: 'r, 'r, P: Push<'f, 's> + ?Sized + 'r> {
//     parser: &'r mut Parser<'f, 's, P>,
//     start: Span<'f>,
//     flags: ExprFlags,
// }
// impl<'f, 's, 'r, P: Push<'f, 's> + ?Sized + 'r> Content for TemplateParts<'f, 's, 'r, P> {
//     type Continue = Span<'f>;
//     type Goal = Vec<TemplatePart<'s, P::Expr>>;
//     type Deferred = DeferredTemplateParts<'f, 's, P>;
//     type Error = P::Error;

//     fn defer(self) -> Result<(Self::Continue, Self::Deferred), P::Error> {
//         let clone = self.parser.lexer.clone();
//         let end = skip::balanced_templates(&mut self.parser.lexer, 1)?.span;
//         Ok((
//             self.start.extend_to_cover(end),
//             DeferredTemplateParts {
//                 lexer: clone,
//                 flags: self.flags,
//                 _phantom: PhantomData,
//             },
//         ))
//     }
//     fn parse(self) -> Result<(Self::Continue, Self::Goal), Self::Error> {
//         let (end, els) = self.parser.parse_template(self.flags)?;
//         Ok((
//             self.start.extend_to_cover(end),
//             els,
//         ))
//     }
// }

// #[derive(Debug)]
// pub struct DeferredTemplateParts<'f, 's, P: ?Sized> {
//     lexer: lex::Lexer<'f, 's>,
//     flags: ExprFlags,
//     _phantom: PhantomData<P>,
// }

// impl<'f, 's, P: Push<'f, 's> + ?Sized> Deferred for DeferredTemplateParts<'f, 's, P> {
//     type Goal = Vec<TemplatePart<'s, P::Expr>>;
//     type Error = P::Error;

//     fn parse(self) -> Result<Self::Goal, Self::Error> {
//         Ok(Parser::<P>::with_lexer(self.lexer)
//         .parse_template(self.flags)?.1)
//     }
// }

#[derive(Debug)]
pub enum Error {
    ParseInt(num::ParseIntError),
    ParseFloat(num::ParseFloatError),
    ParseStrLit(lex::ParseStrLitError),
    Skip(skip::Error),
    Parse(ParseError),
}

impl From<num::ParseIntError> for Error {
    fn from(inner: num::ParseIntError) -> Self {
        Error::ParseInt(inner)
    }
}
impl From<num::ParseFloatError> for Error {
    fn from(inner: num::ParseFloatError) -> Self {
        Error::ParseFloat(inner)
    }
}
impl From<lex::ParseStrLitError> for Error {
    fn from(inner: lex::ParseStrLitError) -> Self {
        Error::ParseStrLit(inner)
    }
}
impl From<skip::Error> for Error {
    fn from(inner: skip::Error) -> Self {
        Error::Skip(inner)
    }
}
impl From<ParseError> for Error {
    fn from(inner: ParseError) -> Self {
        Error::Parse(inner)
    }
}
