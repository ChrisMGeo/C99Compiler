use super::lexer::{AssignmentOperator, Literal};
#[derive(Debug, Clone, PartialEq)]
pub enum NonTerminal {
    PrimaryExpression(PrimaryExpression),
    PostfixExpression(PostfixExpression),
    ArgumentExpressionList(ArgumentExpressionList),
    UnaryExpression(UnaryExpression),
    UnaryOperator(UnaryOperator),
    CastExpression(CastExpression),
    MultiplicativeExpression(MultiplicativeExpression),
    MultiplicativeOperator(MultiplicativeOperator),
    AdditiveExpression(AdditiveExpression),
    AdditiveOperator(AdditiveOperator),
    ShiftExpression(ShiftExpression),
    ShiftOperator(ShiftOperator),
    RelationalExpression(RelationalExpression),
    RelationalOperator(RelationalOperator),
    EqualityExpression(EqualityExpression),
    EqualityOperator(EqualityOperator),
    BitwiseAndExpression(BitwiseAndExpression),
    ExclusiveOrExpression(ExclusiveOrExpression),
    InclusiveOrExpression(InclusiveOrExpression),
    LogicalAndExpression(LogicalAndExpression),
    LogicalOrExpression(LogicalOrExpression),
    ConditionalExpression(ConditionalExpression),
    AssignmentExpression(AssignmentExpression),
    AssignmentOperator(AssignmentOperator),
    Expression(Expression),
    ConstantExpression(ConstantExpression),
    Declaration(Declaration),
    DeclarationSpecifiers(DeclarationSpecifiers),
    InitDeclaratorList(InitDeclaratorList),
    InitDeclarator(InitDeclarator),
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    StructOrUnionSpecifier(StructOrUnionSpecifier),
    StructOrUnion(StructOrUnion),
    StructDeclarationList(StructDeclarationList),
    StructDeclaration(StructDeclaration),
    SpecifierQualifierList(SpecifierQualifierList),
    StructDeclaratorList(StructDeclaratorList),
    StructDeclarator(StructDeclarator),
    EnumSpecifier(EnumSpecifier),
    EnumeratorList(EnumeratorList),
    Enumerator(Enumerator),
    TypeQualifier(TypeQualifier),
    FunctionSpecifier(FunctionSpecifier),
    Declarator(Declarator),
    DirectDeclarator(DirectDeclarator),
    Pointer(Pointer),
    TypeQualifierList(TypeQualifierList),
    ParameterTypeList(ParameterTypeList),
    ParameterList(ParameterList),
    ParameterDeclaration(ParameterDeclaration),
    IdentifierList(IdentifierList),
    TypeName(TypeName),
    AbstractDeclarator(AbstractDeclarator),
    DirectAbstractDeclarator(DirectAbstractDeclarator),
    Initializer(Initializer),
    InitializerList(InitializerList),
    Designation(Designation),
    DesignatorList(DesignatorList),
    Designator(Designator),
    Statement(Statement),
    LabeledStatement(LabeledStatement),
    CompoundStatement(CompoundStatement),
    BlockItemList(BlockItemList),
    BlockItem(BlockItem),
    ExpressionStatement(ExpressionStatement),
    SelectionStatement(SelectionStatement),
    IterationStatement(IterationStatement),
    JumpStatement(JumpStatement),
    TranslationUnit(TranslationUnit),
    ExternalDeclaration(ExternalDeclaration),
    FunctionDefinition(FunctionDefinition),
    DeclarationList(DeclarationList),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimaryExpression {
    Identifier(String),
    Literal(Literal),
    ParenthesizedExpression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixExpression {
    Primary(PrimaryExpression),
    Subscript(Box<PostfixExpression>, Expression),
    FunctionCall(Box<PostfixExpression>, Option<ArgumentExpressionList>),
    MemberAccess(Box<PostfixExpression>, String),
    PointerAccess(Box<PostfixExpression>, String),
    PostIncrement(Box<PostfixExpression>),
    PostDecrement(Box<PostfixExpression>),
    CastedStruct(TypeName, InitializerList),
}
#[derive(Debug, Clone, PartialEq)]
pub struct ArgumentExpressionList(pub(crate) Vec<AssignmentExpression>);

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryExpression {
    Postfix(PostfixExpression),
    PreIncrement(Box<UnaryExpression>),
    PreDecrement(Box<UnaryExpression>),
    UnaryOperation(UnaryOperator, Box<CastExpression>),
    SizeofExpression(Box<UnaryExpression>),
    SizeofType(TypeName),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOperator {
    AddressOf,
    Dereference,
    UnaryPlus,
    UnaryMinus,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CastExpression {
    Unary(UnaryExpression),
    TypeCast(TypeName, Box<CastExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MultiplicativeExpression {
    Cast(CastExpression),
    Multiplicative(
        Box<MultiplicativeExpression>,
        MultiplicativeOperator,
        CastExpression,
    ),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MultiplicativeOperator {
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AdditiveExpression {
    Multiplicative(MultiplicativeExpression),
    Additive(
        Box<AdditiveExpression>,
        AdditiveOperator,
        MultiplicativeExpression,
    ),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AdditiveOperator {
    Plus,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ShiftExpression {
    Additive(AdditiveExpression),
    Shift(Box<ShiftExpression>, ShiftOperator, AdditiveExpression),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ShiftOperator {
    LeftShift,
    RightShift,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelationalExpression {
    Shift(ShiftExpression),
    Relational(
        Box<RelationalExpression>,
        RelationalOperator,
        ShiftExpression,
    ),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RelationalOperator {
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EqualityExpression {
    Relational(RelationalExpression),
    Equality(
        Box<EqualityExpression>,
        EqualityOperator,
        RelationalExpression,
    ),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BitwiseAndExpression {
    Equality(EqualityExpression),
    BitwiseAnd(Box<BitwiseAndExpression>, EqualityExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExclusiveOrExpression {
    BitwiseAnd(BitwiseAndExpression),
    ExclusiveOr(Box<ExclusiveOrExpression>, BitwiseAndExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum InclusiveOrExpression {
    ExclusiveOr(ExclusiveOrExpression),
    InclusiveOr(Box<InclusiveOrExpression>, ExclusiveOrExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalAndExpression {
    InclusiveOr(InclusiveOrExpression),
    LogicalAnd(Box<LogicalAndExpression>, InclusiveOrExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOrExpression {
    LogicalAnd(LogicalAndExpression),
    LogicalOr(Box<LogicalOrExpression>, LogicalAndExpression),
}
#[derive(Debug, Clone, PartialEq)]
pub enum ConditionalExpression {
    LogicalOr(LogicalOrExpression),
    Conditional(LogicalOrExpression, Expression, Box<ConditionalExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentExpression {
    Conditional(ConditionalExpression),
    Assignment(
        UnaryExpression,
        AssignmentOperator,
        Box<AssignmentExpression>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression(pub(crate) Vec<AssignmentExpression>);

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantExpression(pub(crate) ConditionalExpression);

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub(crate) declaration_specifiers: DeclarationSpecifiers,
    pub(crate) init_declarator_list: Option<InitDeclaratorList>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationSpecifiers(pub(crate) Vec<DeclarationSpecifier>);

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    FunctionSpecifier(FunctionSpecifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InitDeclaratorList(pub(crate) Vec<InitDeclarator>);

#[derive(Debug, Clone, PartialEq)]
pub struct InitDeclarator {
    pub(crate) declarator: Declarator,
    pub(crate) initializer: Option<Initializer>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum StorageClassSpecifier {
    Typedef,
    Extern,
    Static,
    Auto,
    Register,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    Complex,
    Imaginary,
    StructOrUnionSpecifier(StructOrUnionSpecifier),
    EnumSpecifier(EnumSpecifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructOrUnionSpecifier {
    StructOrUnion(StructOrUnion, Option<String>, StructDeclarationList),
    StructOrUnionWithId(StructOrUnion, String),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum StructOrUnion {
    Struct,
    Union,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclarationList(pub(crate) Vec<StructDeclaration>);
#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub(crate) specifier_qualifier_list: SpecifierQualifierList,
    pub(crate) struct_declarator_list: StructDeclaratorList,
}
#[derive(Debug, Clone, PartialEq)]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpecifierQualifierList(pub(crate) Vec<SpecifierQualifier>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaratorList(pub(crate) Vec<StructDeclarator>);
#[derive(Debug, Clone, PartialEq)]
pub enum StructDeclarator {
    Declarator(Declarator),
    DeclaratorWithBitField(Option<Declarator>, ConstantExpression),
}
#[derive(Debug, Clone, PartialEq)]
pub enum EnumSpecifier {
    Enum(Option<String>, EnumeratorList),
    EnumWithId(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumeratorList(pub(crate) Vec<Enumerator>);

#[derive(Debug, Clone, PartialEq)]
pub enum Enumerator {
    Enumerator(String),
    EnumeratorWithValue(String, ConstantExpression),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FunctionSpecifier {
    Inline,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Declarator {
    pub(crate) pointer: Option<Pointer>,
    pub(crate) direct_declarator: DirectDeclarator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DirectDeclarator {
    // TODO: direct_declarator
    Identifier(String),
    Declarator(Box<Declarator>),
    ToAo(
        Box<DirectDeclarator>,
        Option<TypeQualifierList>,
        Option<AssignmentExpression>,
    ),
    SToA(
        Box<DirectDeclarator>,
        Option<TypeQualifierList>,
        AssignmentExpression,
    ),
    TSA(
        Box<DirectDeclarator>,
        TypeQualifierList,
        AssignmentExpression,
    ),
    ToAsterisk(Box<DirectDeclarator>, Option<TypeQualifierList>),
    P(Box<DirectDeclarator>, ParameterTypeList),
    I(Box<DirectDeclarator>, Option<IdentifierList>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pointer {
    pub(crate) type_qualifier_list: Option<TypeQualifierList>,
    pub(crate) pointer: Option<Box<Pointer>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeQualifierList(pub(crate) Vec<TypeQualifier>);

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterTypeList {
    pub(crate) parameter_list: ParameterList,
    pub(crate) ellipsis: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterList(pub(crate) Vec<ParameterDeclaration>);

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterDeclaration {
    pub(crate) declaration_specifiers: DeclarationSpecifiers,
    pub(crate) declarator_or_abstract_declarator: Option<DeclaratorOrAbstractDeclarator>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclaratorOrAbstractDeclarator {
    Declarator(Declarator),
    AbstractDeclarator(AbstractDeclarator),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierList(pub(crate) Vec<String>);

#[derive(Debug, Clone, PartialEq)]
pub struct TypeName {
    pub(crate) specifier_qualifier_list: SpecifierQualifierList,
    pub(crate) abstract_declarator: Option<AbstractDeclarator>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractDeclarator {
    pub(crate) pointer: Option<Pointer>,
    pub(crate) direct_abstract_declarator: Option<DirectAbstractDeclarator>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DirectAbstractDeclarator {
    AbstractDeclarator(Box<AbstractDeclarator>),
    AssignmentExpression(
        Option<Box<DirectAbstractDeclarator>>,
        Option<Box<AssignmentExpression>>,
    ),
    Asterisk(Option<Box<DirectAbstractDeclarator>>),
    ParameterTypeList(
        Option<Box<DirectAbstractDeclarator>>,
        Option<ParameterTypeList>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Initializer {
    AssignmentExpression(AssignmentExpression),
    InitializerList(InitializerList),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DesignatedInitializer {
    Designated(Designation, Initializer),
    Initializer(Initializer),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InitializerList(pub(crate) Vec<DesignatedInitializer>);

#[derive(Debug, Clone, PartialEq)]
pub struct Designation(pub(crate) DesignatorList);

#[derive(Debug, Clone, PartialEq)]
pub struct DesignatorList(pub(crate) Vec<Designator>);

#[derive(Debug, Clone, PartialEq)]
pub enum Designator {
    ArrayDesignator(ConstantExpression),
    MemberDesignator(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Labeled(LabeledStatement),
    Compound(CompoundStatement),
    Expression(ExpressionStatement),
    Selection(SelectionStatement),
    Iteration(IterationStatement),
    Jump(JumpStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LabeledStatement {
    Identifier(String, Box<Statement>),
    Case(ConstantExpression, Box<Statement>),
    Default(Box<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundStatement(pub(crate) Option<BlockItemList>);

#[derive(Debug, Clone, PartialEq)]
pub struct BlockItemList(pub(crate) Vec<BlockItem>);

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement(pub(crate) Option<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub enum SelectionStatement {
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Switch(Expression, Box<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IterationStatement {
    While(Expression, Box<Statement>),
    DoWhile(Box<Statement>, Expression),
    For(
        ExpressionStatement,
        ExpressionStatement,
        Option<Expression>,
        Box<Statement>,
    ),
    ForWithDeclaration(
        Declaration,
        ExpressionStatement,
        Option<Expression>,
        Box<Statement>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub enum JumpStatement {
    Goto(String),
    Continue,
    Break,
    Return(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration),
}
#[derive(Debug, Clone, PartialEq)]
pub struct TranslationUnit(pub(crate) Vec<ExternalDeclaration>);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub(crate) declaration_specifiers: DeclarationSpecifiers,
    pub(crate) declarator: Declarator,
    pub(crate) declaration_list: Option<DeclarationList>,
    pub(crate) compound_statement: CompoundStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationList(pub(crate) Vec<Declaration>);
