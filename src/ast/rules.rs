use crate::ast::nonterminals::{
    AbstractDeclarator, ArgumentExpressionList, BlockItem, BlockItemList, CompoundStatement,
    DeclarationList, DeclarationSpecifier, DeclarationSpecifiers, Declarator,
    DeclaratorOrAbstractDeclarator, DesignatedInitializer, Designation, Designator, DesignatorList,
    DirectAbstractDeclarator, DirectDeclarator, EnumSpecifier, Enumerator, EnumeratorList,
    ExpressionStatement, ExternalDeclaration, FunctionDefinition, FunctionSpecifier,
    IdentifierList, InitDeclaratorList, Initializer, InitializerList, IterationStatement,
    JumpStatement, LabeledStatement, ParameterDeclaration, ParameterList, ParameterTypeList,
    Pointer, SelectionStatement, SpecifierQualifier, SpecifierQualifierList, Statement,
    StructDeclaration, StructDeclarationList, StructDeclarator, StructDeclaratorList,
    StructOrUnion, StructOrUnionSpecifier, TranslationUnit, TypeName, TypeQualifier,
    TypeQualifierList,
};

use super::lexer::{AssignmentOperator, Delimiter, Keyword, Operator, Token, TokenKind};
use super::nonterminals::{
    AdditiveExpression, AdditiveOperator, AssignmentExpression, BitwiseAndExpression,
    CastExpression, ConditionalExpression, ConstantExpression, Declaration, EqualityExpression,
    EqualityOperator, ExclusiveOrExpression, Expression, InclusiveOrExpression, InitDeclarator,
    LogicalAndExpression, LogicalOrExpression, MultiplicativeExpression, MultiplicativeOperator,
    NonTerminal, PostfixExpression, PrimaryExpression, RelationalExpression, RelationalOperator,
    ShiftExpression, ShiftOperator, StorageClassSpecifier, TypeSpecifier, UnaryExpression,
    UnaryOperator,
};
use super::parser::{ParseError, Parser, Production, TokenOrNonTerminal};

macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {
        $sub
    };
}

macro_rules! count_tts {
    ($($tts:tt)*) => {0usize $(+ replace_expr!($tts 1usize))*};
}
macro_rules! token_kind_match {
    ($kind:pat) => {
        Token { kind: $kind, .. }
    };
}
macro_rules! generate_reduce_fn {
    ($non_terminal:expr, $($tokens:pat), * ) => {
        |parser: &mut Parser| {
            let last_n = parser.last_n_parse_stack(count_tts!($(($tokens))*));
            match last_n.as_slice() {
                [$( $tokens ),*] => Ok(($non_terminal, count_tts!($(($tokens))*))),
                [] => Err(ParseError::EmptyParseStack),
                _ => Err(ParseError::WrongParsedToReduce(count_tts!($(($tokens))*))),
            }
        }
    };
}

pub fn rules() -> Vec<Production> {
    vec![
        // 0
        |_: &mut Parser| Err(ParseError::EmptyParseStack),
        // 1 primary_expression: IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::PrimaryExpression(PrimaryExpression::Identifier(identifier.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier)))
        ),
        // 2 primary_expression: LITERAL
        generate_reduce_fn!(
            NonTerminal::PrimaryExpression(PrimaryExpression::Literal(literal.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Literal(literal)))
        ),
        // 3 primary_expression: '(' expression ')'
        generate_reduce_fn!(
            NonTerminal::PrimaryExpression(PrimaryExpression::ParenthesizedExpression(
                expr.clone()
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 4 postfix_expression: primary_expression
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::Primary(expr.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PrimaryExpression(expr))
        ),
        // 5 postfix_expression: postfix_expression '[' expression ']'
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::Subscript(
                Box::new(expr.clone()),
                expression.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PostfixExpression(expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ), // 6 postfix_expression: postfix_expression '(' ')'
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::FunctionCall(
                Box::new(expr.clone()),
                None
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PostfixExpression(expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ), // 7 postfix_expression: postfix_expression '(' argument_expression_list ')'
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::FunctionCall(
                Box::new(expr.clone()),
                Some(argument_expression_list.clone())
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PostfixExpression(expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ArgumentExpressionList(
                argument_expression_list
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 8 postfix_expression: postfix_expression '.' IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::MemberAccess(
                Box::new(expr.clone()),
                identifier.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PostfixExpression(expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Period))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier)))
        ),
        // 9 postfix_expression: postfix_expression PTR_OP IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::PointerAccess(
                Box::new(expr.clone()),
                identifier.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PostfixExpression(expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Arrow))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier)))
        ),
        // 10 postfix_expression: postfix_expression INC_OP
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::PostIncrement(Box::new(
                expr.clone()
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PostfixExpression(expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::PlusPlus)))
        ), // 11 postfix_expression: postfix_expression DEC_OP
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::PostDecrement(Box::new(
                expr.clone()
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PostfixExpression(expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::MinusMinus)))
        ),
        // 12 postfix_expression: '(' type_name ')' '{' initializer_list '}'
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::CastedStruct(
                type_name.clone(),
                initializer_list.clone()
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeName(type_name)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitializerList(initializer_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 13 postfix_expression: '(' type_name ')' '{' initializer_list ',' '}'
        generate_reduce_fn!(
            NonTerminal::PostfixExpression(PostfixExpression::CastedStruct(
                type_name.clone(),
                initializer_list.clone()
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeName(type_name)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitializerList(initializer_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        //  14 argument_expression_list: assignment_expression
        generate_reduce_fn!(
            NonTerminal::ArgumentExpressionList(ArgumentExpressionList(vec![
                assignment_expression.clone()
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            ))
        ),
        //  15                         | argument_expression_list ',' assignment_expression
        generate_reduce_fn!(
            NonTerminal::ArgumentExpressionList({
                let mut arg_expr_list = arg_expr_list.clone();
                arg_expr_list.push(assignment_expression.clone());
                ArgumentExpressionList(arg_expr_list)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ArgumentExpressionList(
                ArgumentExpressionList(arg_expr_list)
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            ))
        ),
        //
        //  16 unary_expression: postfix_expression
        generate_reduce_fn!(
            NonTerminal::UnaryExpression(UnaryExpression::Postfix(postfix_expr.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::PostfixExpression(postfix_expr))
        ),
        //  17                 | INC_OP unary_expression
        generate_reduce_fn!(
            NonTerminal::UnaryExpression(UnaryExpression::PreIncrement(Box::new(
                unary_expr.clone()
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::PlusPlus))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::UnaryExpression(unary_expr))
        ),
        //  18                 | DEC_OP unary_expression
        generate_reduce_fn!(
            NonTerminal::UnaryExpression(UnaryExpression::PreDecrement(Box::new(
                unary_expr.clone()
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::MinusMinus))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::UnaryExpression(unary_expr))
        ),
        //  19                 | unary_operator cast_expression
        generate_reduce_fn!(
            NonTerminal::UnaryExpression(UnaryExpression::UnaryOperation(
                *unary_operator,
                Box::new(cast_expr.clone())
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::UnaryOperator(unary_operator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::CastExpression(cast_expr))
        ),
        //  20                 | SIZEOF unary_expression
        generate_reduce_fn!(
            NonTerminal::UnaryExpression(UnaryExpression::SizeofExpression(Box::new(
                unary_expr.clone()
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Sizeof))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::UnaryExpression(unary_expr))
        ),
        //  21                 | SIZEOF '(' type_name ')'
        generate_reduce_fn!(
            NonTerminal::UnaryExpression(UnaryExpression::SizeofType(type_name.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Sizeof))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeName(type_name)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        //  22 unary_operator: '&'
        generate_reduce_fn!(
            NonTerminal::UnaryOperator(UnaryOperator::AddressOf),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Ampersand)))
        ),
        //  23               | '*'
        generate_reduce_fn!(
            NonTerminal::UnaryOperator(UnaryOperator::Dereference),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk)))
        ),
        //  24               | '+'
        generate_reduce_fn!(
            NonTerminal::UnaryOperator(UnaryOperator::UnaryPlus),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Plus)))
        ),
        //  25               | '-'
        generate_reduce_fn!(
            NonTerminal::UnaryOperator(UnaryOperator::UnaryMinus),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Minus)))
        ),
        //  26               | '~'
        generate_reduce_fn!(
            NonTerminal::UnaryOperator(UnaryOperator::BitwiseNot),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Tilde)))
        ),
        //  27               | '!'
        generate_reduce_fn!(
            NonTerminal::UnaryOperator(UnaryOperator::LogicalNot),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Exclamation
            )))
        ),
        //
        //  28 cast_expression: unary_expression
        generate_reduce_fn!(
            NonTerminal::CastExpression(CastExpression::Unary(unary_expr.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::UnaryExpression(unary_expr))
        ),
        //  29                | '(' type_name ')' cast_expression
        generate_reduce_fn!(
            NonTerminal::CastExpression(CastExpression::TypeCast(
                type_name.clone(),
                Box::new(cast_expr.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeName(type_name)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::CastExpression(cast_expr))
        ),
        //  30 multiplicative_expression: cast_expression
        generate_reduce_fn!(
            NonTerminal::MultiplicativeExpression(MultiplicativeExpression::Cast(
                cast_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::CastExpression(cast_expr))
        ),
        //  31                          | multiplicative_expression multiplicative_operator cast_expression
        generate_reduce_fn!(
            NonTerminal::MultiplicativeExpression(MultiplicativeExpression::Multiplicative(
                Box::new(multiplicative_expr.clone()),
                *multiplicative_operator,
                cast_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::MultiplicativeExpression(
                multiplicative_expr
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::MultiplicativeOperator(
                multiplicative_operator
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::CastExpression(cast_expr))
        ),
        //
        //  32 multiplicative_operator: '*'
        generate_reduce_fn!(
            NonTerminal::MultiplicativeOperator(MultiplicativeOperator::Multiply),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk)))
        ),
        //  33                        | '/'
        generate_reduce_fn!(
            NonTerminal::MultiplicativeOperator(MultiplicativeOperator::Divide),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Slash)))
        ),
        //  34                        | '%'
        generate_reduce_fn!(
            NonTerminal::MultiplicativeOperator(MultiplicativeOperator::Modulo),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Percent)))
        ),
        //
        //  35 additive_expression: multiplicative_expression
        generate_reduce_fn!(
            NonTerminal::AdditiveExpression(AdditiveExpression::Multiplicative(
                multiplicative_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::MultiplicativeExpression(
                multiplicative_expr
            ))
        ),
        //  36                    | additive_expression additive_operator multiplicative_expression
        generate_reduce_fn!(
            NonTerminal::AdditiveExpression(AdditiveExpression::Additive(
                Box::new(additive_expr.clone()),
                *additive_operator,
                multiplicative_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AdditiveExpression(additive_expr)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AdditiveOperator(additive_operator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::MultiplicativeExpression(
                multiplicative_expr
            ))
        ),
        //
        //  37 additive_operator: '+'
        generate_reduce_fn!(
            NonTerminal::AdditiveOperator(AdditiveOperator::Plus),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Plus)))
        ),
        //  38                  | '-'
        generate_reduce_fn!(
            NonTerminal::AdditiveOperator(AdditiveOperator::Minus),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Minus)))
        ),
        //
        //  39 shift_expression: additive_expression
        generate_reduce_fn!(
            NonTerminal::ShiftExpression(ShiftExpression::Additive(additive_expr.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AdditiveExpression(additive_expr))
        ),
        //  40                 | shift_expression shift_operator additive_expression
        generate_reduce_fn!(
            NonTerminal::ShiftExpression(ShiftExpression::Shift(
                Box::new(shift_expr.clone()),
                *shift_operator,
                additive_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ShiftExpression(shift_expr)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ShiftOperator(shift_operator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AdditiveExpression(additive_expr))
        ),
        //
        //  41 shift_operator: LEFT_OP
        generate_reduce_fn!(
            NonTerminal::ShiftOperator(ShiftOperator::LeftShift),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::LeftShift)))
        ),
        //  42               | RIGHT_OP
        generate_reduce_fn!(
            NonTerminal::ShiftOperator(ShiftOperator::RightShift),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::RightShift)))
        ),
        //
        //  43 relational_expression: shift_expression
        generate_reduce_fn!(
            NonTerminal::RelationalExpression(RelationalExpression::Shift(shift_expr.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ShiftExpression(shift_expr))
        ),
        //  44                      | relational_expression relational_operator shift_expression
        generate_reduce_fn!(
            NonTerminal::RelationalExpression(RelationalExpression::Relational(
                Box::new(relational_expr.clone()),
                *relational_operator,
                shift_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::RelationalExpression(relational_expr)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::RelationalOperator(relational_operator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ShiftExpression(shift_expr))
        ),
        //
        //  45 relational_operator: '<'
        generate_reduce_fn!(
            NonTerminal::RelationalOperator(RelationalOperator::LessThan),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::LessThan)))
        ),
        //  46                    | '>'
        generate_reduce_fn!(
            NonTerminal::RelationalOperator(RelationalOperator::GreaterThan),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::GreaterThan
            )))
        ),
        //  47                    | LE_OP
        generate_reduce_fn!(
            NonTerminal::RelationalOperator(RelationalOperator::LessThanEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::LessThanEquals
            )))
        ),
        //  48                    | GE_OP
        generate_reduce_fn!(
            NonTerminal::RelationalOperator(RelationalOperator::GreaterThanEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::GreaterThanEquals
            )))
        ),
        //
        //  49 equality_expression: relational_expression
        generate_reduce_fn!(
            NonTerminal::EqualityExpression(EqualityExpression::Relational(
                relational_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::RelationalExpression(relational_expr))
        ),
        //  50                    | equality_expression equality_operator relational_expression
        generate_reduce_fn!(
            NonTerminal::EqualityExpression(EqualityExpression::Equality(
                Box::new(equality_expr.clone()),
                *equality_operator,
                relational_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EqualityExpression(equality_expr)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EqualityOperator(equality_operator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::RelationalExpression(relational_expr))
        ),
        //
        //  51 equality_operator: EQ_OP
        generate_reduce_fn!(
            NonTerminal::EqualityOperator(EqualityOperator::Equal),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::EqualEqual)))
        ),
        //  52                  | NE_OP
        generate_reduce_fn!(
            NonTerminal::EqualityOperator(EqualityOperator::NotEqual),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::ExclamationEqual
            )))
        ),
        //
        //  53 and_expression: equality_expression
        generate_reduce_fn!(
            NonTerminal::BitwiseAndExpression(BitwiseAndExpression::Equality(
                equality_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EqualityExpression(equality_expr))
        ),
        //  54               | and_expression '&' equality_expression
        generate_reduce_fn!(
            NonTerminal::BitwiseAndExpression(BitwiseAndExpression::BitwiseAnd(
                Box::new(and_expr.clone()),
                equality_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::BitwiseAndExpression(and_expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Ampersand))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EqualityExpression(equality_expr))
        ),
        //
        //  55 exclusive_or_expression: and_expression
        generate_reduce_fn!(
            NonTerminal::ExclusiveOrExpression(ExclusiveOrExpression::BitwiseAnd(and_expr.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::BitwiseAndExpression(and_expr))
        ),
        //  56                        | exclusive_or_expression '^' and_expression
        generate_reduce_fn!(
            NonTerminal::ExclusiveOrExpression(ExclusiveOrExpression::ExclusiveOr(
                Box::new(exclusive_or_expr.clone()),
                and_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExclusiveOrExpression(exclusive_or_expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Caret))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::BitwiseAndExpression(and_expr))
        ),
        //
        //  57 inclusive_or_expression: exclusive_or_expression
        generate_reduce_fn!(
            NonTerminal::InclusiveOrExpression(InclusiveOrExpression::ExclusiveOr(
                exclusive_or_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExclusiveOrExpression(exclusive_or_expr))
        ),
        //  58                        | inclusive_or_expression '|' exclusive_or_expression
        generate_reduce_fn!(
            NonTerminal::InclusiveOrExpression(InclusiveOrExpression::InclusiveOr(
                Box::new(inclusive_or_expr.clone()),
                exclusive_or_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InclusiveOrExpression(inclusive_or_expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Pipe))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExclusiveOrExpression(exclusive_or_expr))
        ),
        //
        //  59 logical_and_expression: inclusive_or_expression
        generate_reduce_fn!(
            NonTerminal::LogicalAndExpression(LogicalAndExpression::InclusiveOr(
                inclusive_or_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InclusiveOrExpression(inclusive_or_expr))
        ),
        //  60                       | logical_and_expression AND_OP inclusive_or_expression
        generate_reduce_fn!(
            NonTerminal::LogicalAndExpression(LogicalAndExpression::LogicalAnd(
                Box::new(logical_and_expr.clone()),
                inclusive_or_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::LogicalAndExpression(logical_and_expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::AmpersandAmpersand
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InclusiveOrExpression(inclusive_or_expr))
        ),
        //
        //  61 logical_or_expression: logical_and_expression
        generate_reduce_fn!(
            NonTerminal::LogicalOrExpression(LogicalOrExpression::LogicalAnd(
                logical_and_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::LogicalAndExpression(logical_and_expr))
        ),
        //  62                      | logical_or_expression OR_OP logical_and_expression
        generate_reduce_fn!(
            NonTerminal::LogicalOrExpression(LogicalOrExpression::LogicalOr(
                Box::new(logical_or_expr.clone()),
                logical_and_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::LogicalOrExpression(logical_or_expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::PipePipe))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::LogicalAndExpression(logical_and_expr))
        ),
        //
        //  63 conditional_expression: logical_or_expression
        generate_reduce_fn!(
            NonTerminal::ConditionalExpression(ConditionalExpression::LogicalOr(
                logical_or_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::LogicalOrExpression(logical_or_expr))
        ),
        //  64                       | logical_or_expression '?' expression ':' conditional_expression
        generate_reduce_fn!(
            NonTerminal::ConditionalExpression(ConditionalExpression::Conditional(
                logical_or_expr.clone(),
                expression.clone(),
                Box::new(conditional_expr.clone())
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::LogicalOrExpression(logical_or_expr)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Question))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Colon))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ConditionalExpression(conditional_expr))
        ),
        //
        //  65 assignment_expression: conditional_expression
        generate_reduce_fn!(
            NonTerminal::AssignmentExpression(AssignmentExpression::Conditional(
                conditional_expr.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ConditionalExpression(conditional_expr))
        ),
        //  66                      | unary_expression assignment_operator assignment_expression
        generate_reduce_fn!(
            NonTerminal::AssignmentExpression(AssignmentExpression::Assignment(
                unary_expr.clone(),
                *assignment_operator,
                Box::new(assignment_expr.clone())
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::UnaryExpression(unary_expr)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentOperator(assignment_operator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(assignment_expr))
        ),
        //
        //  67 assignment_operator: '='
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::Equals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::Equals)
            )))
        ),
        //  68                    | MUL_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::AsteriskEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::AsteriskEquals)
            )))
        ),
        //  69                    | DIV_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::SlashEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::SlashEquals)
            )))
        ),
        //  70                    | MOD_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::PercentEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::PercentEquals)
            )))
        ),
        //  71                    | ADD_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::PlusEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::PlusEquals)
            )))
        ),
        //  72                    | SUB_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::MinusEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::MinusEquals)
            )))
        ),
        //  73                    | LEFT_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::LeftShiftEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::LeftShiftEquals)
            )))
        ),
        //  74                    | RIGHT_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::RightShiftEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::RightShiftEquals)
            )))
        ),
        //  75                    | AND_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::AmpersandEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::AmpersandEquals)
            )))
        ),
        //  76                    | XOR_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::CaretEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::CaretEquals)
            )))
        ),
        //  77                    | OR_ASSIGN
        generate_reduce_fn!(
            NonTerminal::AssignmentOperator(AssignmentOperator::PipeEquals),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::PipeEquals)
            )))
        ),
        //
        //  78 expression: assignment_expression
        generate_reduce_fn!(
            NonTerminal::Expression(Expression(vec![assignment_expr.clone()])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(assignment_expr))
        ),
        //  79           | expression ',' assignment_expression
        generate_reduce_fn!(
            NonTerminal::Expression({
                let mut expr = expression.clone();
                expr.push(assignment_expr.clone());
                Expression(expr)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(Expression(expression))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(assignment_expr))
        ),
        //
        //  80 constant_expression: conditional_expression
        generate_reduce_fn!(
            NonTerminal::ConstantExpression(ConstantExpression(conditional_expr.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ConditionalExpression(conditional_expr))
        ),
        //
        //  81 declaration: declaration_specifiers ';'
        generate_reduce_fn!(
            NonTerminal::Declaration(Declaration {
                declaration_specifiers: declaration_specifiers.clone(),
                init_declarator_list: None
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        //  82            | declaration_specifiers init_declarator_list ';'
        generate_reduce_fn!(
            NonTerminal::Declaration(Declaration {
                declaration_specifiers: declaration_specifiers.clone(),
                init_declarator_list: Some(init_declarator_list.clone())
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitDeclaratorList(init_declarator_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        //
        //  83 declaration_specifiers: storage_class_specifier
        generate_reduce_fn!(
            NonTerminal::DeclarationSpecifiers(DeclarationSpecifiers(vec![
                DeclarationSpecifier::StorageClassSpecifier(*storage_class_specifier)
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StorageClassSpecifier(
                storage_class_specifier
            ))
        ),
        //  84                       | storage_class_specifier declaration_specifiers
        generate_reduce_fn!(
            NonTerminal::DeclarationSpecifiers({
                let mut decl_specifiers = declaration_specifiers.0.clone();
                decl_specifiers.insert(
                    0,
                    DeclarationSpecifier::StorageClassSpecifier(*storage_class_specifier),
                );
                DeclarationSpecifiers(decl_specifiers)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StorageClassSpecifier(
                storage_class_specifier
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            ))
        ),
        //  85                       | type_specifier
        generate_reduce_fn!(
            NonTerminal::DeclarationSpecifiers(DeclarationSpecifiers(vec![
                DeclarationSpecifier::TypeSpecifier(type_specifier.clone())
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeSpecifier(type_specifier))
        ),
        //  86                       | type_specifier declaration_specifiers
        generate_reduce_fn!(
            NonTerminal::DeclarationSpecifiers({
                let mut decl_specifiers = declaration_specifiers.0.clone();
                decl_specifiers.insert(
                    0,
                    DeclarationSpecifier::TypeSpecifier(type_specifier.clone()),
                );
                DeclarationSpecifiers(decl_specifiers)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeSpecifier(type_specifier)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            ))
        ),
        //  87                       | type_qualifier
        generate_reduce_fn!(
            NonTerminal::DeclarationSpecifiers(DeclarationSpecifiers(vec![
                DeclarationSpecifier::TypeQualifier(*type_qualifier)
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifier(type_qualifier))
        ),
        //  88                       | type_qualifier declaration_specifiers
        generate_reduce_fn!(
            NonTerminal::DeclarationSpecifiers({
                let mut decl_specifiers = declaration_specifiers.0.clone();
                decl_specifiers.insert(0, DeclarationSpecifier::TypeQualifier(*type_qualifier));
                DeclarationSpecifiers(decl_specifiers)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifier(type_qualifier)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            ))
        ),
        //  89                       | function_specifier
        generate_reduce_fn!(
            NonTerminal::DeclarationSpecifiers(DeclarationSpecifiers(vec![
                DeclarationSpecifier::FunctionSpecifier(*function_specifier)
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::FunctionSpecifier(function_specifier))
        ),
        //  90                       | function_specifier declaration_specifiers
        generate_reduce_fn!(
            NonTerminal::DeclarationSpecifiers({
                let mut decl_specifiers = declaration_specifiers.0.clone();
                decl_specifiers.insert(
                    0,
                    DeclarationSpecifier::FunctionSpecifier(*function_specifier),
                );
                DeclarationSpecifiers(decl_specifiers)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::FunctionSpecifier(function_specifier)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            ))
        ),
        //
        //  91 init_declarator_list: init_declarator
        generate_reduce_fn!(
            NonTerminal::InitDeclaratorList(InitDeclaratorList(vec![init_declarator.clone()])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitDeclarator(init_declarator))
        ),
        //  92                     | init_declarator_list ',' init_declarator
        generate_reduce_fn!(
            NonTerminal::InitDeclaratorList({
                let mut init_decl_list = init_declarator_list.0.clone();
                init_decl_list.push(init_declarator.clone());
                InitDeclaratorList(init_decl_list)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitDeclaratorList(init_declarator_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitDeclarator(init_declarator))
        ),
        //
        //  93 init_declarator: declarator
        generate_reduce_fn!(
            NonTerminal::InitDeclarator(InitDeclarator {
                declarator: declarator.clone(),
                initializer: None
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declarator(declarator))
        ),
        //  94                | declarator '=' initializer
        generate_reduce_fn!(
            NonTerminal::InitDeclarator(InitDeclarator {
                declarator: declarator.clone(),
                initializer: Some(initializer.clone())
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::Equals)
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Initializer(initializer))
        ),
        //
        //  95 storage_class_specifier: TYPEDEF
        generate_reduce_fn!(
            NonTerminal::StorageClassSpecifier(StorageClassSpecifier::Typedef),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Typedef)))
        ),
        //  96                        | EXTERN
        generate_reduce_fn!(
            NonTerminal::StorageClassSpecifier(StorageClassSpecifier::Extern),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Extern)))
        ),
        //  97                        | STATIC
        generate_reduce_fn!(
            NonTerminal::StorageClassSpecifier(StorageClassSpecifier::Static),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Static)))
        ),
        //  98                        | AUTO
        generate_reduce_fn!(
            NonTerminal::StorageClassSpecifier(StorageClassSpecifier::Auto),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Auto)))
        ),
        //  99                        | REGISTER
        generate_reduce_fn!(
            NonTerminal::StorageClassSpecifier(StorageClassSpecifier::Register),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Register)))
        ),
        //
        // 100 type_specifier: VOID
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Void),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Void)))
        ),
        // 101               | CHAR
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Char),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Char)))
        ),
        // 102               | SHORT
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Short),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Short)))
        ),
        // 103               | INT
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Int),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Int)))
        ),
        // 104               | LONG
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Long),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Long)))
        ),
        // 105               | FLOAT
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Float),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Float)))
        ),
        // 106               | DOUBLE
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Double),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Double)))
        ),
        // 107               | SIGNED
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Signed),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Signed)))
        ),
        // 108               | UNSIGNED
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Unsigned),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Unsigned)))
        ),
        // 109               | BOOL
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Bool),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::_Bool)))
        ),
        // 110               | COMPLEX
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Complex),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::_Complex)))
        ),
        // 111               | IMAGINARY
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::Imaginary),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::_Imaginary)))
        ),
        // 112               | struct_or_union_specifier
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::StructOrUnionSpecifier(
                struct_or_union_specifier.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructOrUnionSpecifier(
                struct_or_union_specifier
            ))
        ),
        // 113               | enum_specifier
        generate_reduce_fn!(
            NonTerminal::TypeSpecifier(TypeSpecifier::EnumSpecifier(enum_specifier.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EnumSpecifier(enum_specifier))
        ),
        //
        // 114 struct_or_union_specifier: struct_or_union IDENTIFIER '{' struct_declaration_list '}'
        generate_reduce_fn!(
            NonTerminal::StructOrUnionSpecifier(StructOrUnionSpecifier::StructOrUnion(
                *struct_or_union,
                Some(identifier.clone()),
                struct_declaration_list.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructOrUnion(struct_or_union)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclarationList(
                struct_declaration_list
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 115                          | struct_or_union '{' struct_declaration_list '}'
        generate_reduce_fn!(
            NonTerminal::StructOrUnionSpecifier(StructOrUnionSpecifier::StructOrUnion(
                *struct_or_union,
                None,
                struct_declaration_list.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructOrUnion(struct_or_union)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclarationList(
                struct_declaration_list
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 116                          | struct_or_union IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::StructOrUnionSpecifier(StructOrUnionSpecifier::StructOrUnionWithId(
                *struct_or_union,
                identifier.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructOrUnion(struct_or_union)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier)))
        ),
        //
        // 117 struct_or_union: STRUCT
        generate_reduce_fn!(
            NonTerminal::StructOrUnion(StructOrUnion::Struct),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Struct)))
        ),
        // 118                | UNION
        generate_reduce_fn!(
            NonTerminal::StructOrUnion(StructOrUnion::Union),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Union)))
        ),
        //
        // 119 struct_declaration_list: struct_declaration
        generate_reduce_fn!(
            NonTerminal::StructDeclarationList(StructDeclarationList(vec![
                struct_declaration.clone()
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclaration(struct_declaration))
        ),
        // 120                        | struct_declaration_list struct_declaration
        generate_reduce_fn!(
            NonTerminal::StructDeclarationList({
                let mut vec = struct_declaration_list.0.clone();
                vec.push(struct_declaration.clone());
                StructDeclarationList(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclarationList(
                struct_declaration_list
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclaration(struct_declaration))
        ),
        //
        // 121 struct_declaration: specifier_qualifier_list struct_declarator_list ';'
        generate_reduce_fn!(
            NonTerminal::StructDeclaration(StructDeclaration {
                specifier_qualifier_list: specifier_qualifier_list.clone(),
                struct_declarator_list: struct_declarator_list.clone()
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::SpecifierQualifierList(
                specifier_qualifier_list
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclaratorList(
                struct_declarator_list
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        //
        // 122 specifier_qualifier_list: type_specifier specifier_qualifier_list
        generate_reduce_fn!(
            NonTerminal::SpecifierQualifierList({
                let mut vec = specifier_qualifier_list.0.clone();
                vec.insert(0, SpecifierQualifier::TypeSpecifier(type_specifier.clone()));
                SpecifierQualifierList(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeSpecifier(type_specifier)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::SpecifierQualifierList(
                specifier_qualifier_list
            ))
        ),
        // 123                         | type_specifier
        generate_reduce_fn!(
            NonTerminal::SpecifierQualifierList(SpecifierQualifierList(vec![
                SpecifierQualifier::TypeSpecifier(type_specifier.clone())
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeSpecifier(type_specifier))
        ),
        // 124                         | type_qualifier specifier_qualifier_list
        generate_reduce_fn!(
            NonTerminal::SpecifierQualifierList({
                let mut vec = specifier_qualifier_list.0.clone();
                vec.insert(0, SpecifierQualifier::TypeQualifier(*type_qualifier));
                SpecifierQualifierList(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifier(type_qualifier)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::SpecifierQualifierList(
                specifier_qualifier_list
            ))
        ),
        // 125                         | type_qualifier
        generate_reduce_fn!(
            NonTerminal::SpecifierQualifierList(SpecifierQualifierList(vec![
                SpecifierQualifier::TypeQualifier(*type_qualifier)
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifier(type_qualifier))
        ),
        //
        // 126 struct_declarator_list: struct_declarator
        generate_reduce_fn!(
            NonTerminal::StructDeclaratorList(StructDeclaratorList(
                vec![struct_declarator.clone()]
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclarator(struct_declarator))
        ),
        // 127                       | struct_declarator_list ',' struct_declarator
        generate_reduce_fn!(
            NonTerminal::StructDeclaratorList({
                let mut vec = struct_declarator_list.0.clone();
                vec.push(struct_declarator.clone());
                StructDeclaratorList(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclarator(struct_declarator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::StructDeclaratorList(
                struct_declarator_list
            ))
        ),
        //
        // 128 struct_declarator: declarator
        generate_reduce_fn!(
            NonTerminal::StructDeclarator(StructDeclarator::Declarator(declarator.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declarator(declarator))
        ),
        // 129                  | ':' constant_expression
        generate_reduce_fn!(
            NonTerminal::StructDeclarator(StructDeclarator::DeclaratorWithBitField(
                None,
                constant_expression.clone()
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Colon))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ConstantExpression(constant_expression))
        ),
        // 130                  | declarator ':' constant_expression
        generate_reduce_fn!(
            NonTerminal::StructDeclarator(StructDeclarator::DeclaratorWithBitField(
                Some(declarator.clone()),
                constant_expression.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Colon))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ConstantExpression(constant_expression))
        ),
        //
        // 131 enum_specifier: ENUM '{' enumerator_list '}'
        generate_reduce_fn!(
            NonTerminal::EnumSpecifier(EnumSpecifier::Enum(None, enumerator_list.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Enum))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EnumeratorList(enumerator_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 132               | ENUM IDENTIFIER '{' enumerator_list '}'
        generate_reduce_fn!(
            NonTerminal::EnumSpecifier(EnumSpecifier::Enum(
                Some(identifier.clone()),
                enumerator_list.clone()
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Enum))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EnumeratorList(enumerator_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 133               | ENUM '{' enumerator_list ',' '}'
        generate_reduce_fn!(
            NonTerminal::EnumSpecifier(EnumSpecifier::Enum(None, enumerator_list.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Enum))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EnumeratorList(enumerator_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 134               | ENUM IDENTIFIER '{' enumerator_list ',' '}'
        generate_reduce_fn!(
            NonTerminal::EnumSpecifier(EnumSpecifier::Enum(
                Some(identifier.clone()),
                enumerator_list.clone()
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Enum))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EnumeratorList(enumerator_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 135               | ENUM IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::EnumSpecifier(EnumSpecifier::EnumWithId(identifier.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Enum))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier)))
        ),
        //
        // 136 enumerator_list: enumerator
        generate_reduce_fn!(
            NonTerminal::EnumeratorList(EnumeratorList(vec![enumerator.clone()])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Enumerator(enumerator))
        ),
        // 137                | enumerator_list ',' enumerator
        generate_reduce_fn!(
            NonTerminal::EnumeratorList({
                let mut vec = enumerator_list.0.clone();
                vec.push(enumerator.clone());
                EnumeratorList(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::EnumeratorList(enumerator_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Enumerator(enumerator))
        ),
        //
        // 138 enumerator: IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::Enumerator(Enumerator::Enumerator(enumerator.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(enumerator)))
        ),
        // 139           | IDENTIFIER '=' constant_expression
        generate_reduce_fn!(
            NonTerminal::Enumerator(Enumerator::EnumeratorWithValue(
                enumerator.clone(),
                constant_expression.clone()
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(enumerator))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::Equals)
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ConstantExpression(constant_expression))
        ),
        //
        // 140 type_qualifier: CONST
        generate_reduce_fn!(
            NonTerminal::TypeQualifier(TypeQualifier::Const),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Const)))
        ),
        // 141               | RESTRICT
        generate_reduce_fn!(
            NonTerminal::TypeQualifier(TypeQualifier::Restrict),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Restrict)))
        ),
        // 142               | VOLATILE
        generate_reduce_fn!(
            NonTerminal::TypeQualifier(TypeQualifier::Volatile),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Volatile)))
        ),
        //
        // 143 function_specifier: INLINE
        generate_reduce_fn!(
            NonTerminal::FunctionSpecifier(FunctionSpecifier::Inline),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Inline)))
        ),
        //
        // 144 declarator: pointer direct_declarator
        generate_reduce_fn!(
            NonTerminal::Declarator(Declarator {
                pointer: Some(pointer.clone()),
                direct_declarator: declarator.clone()
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Pointer(pointer)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator))
        ),
        // 145           | direct_declarator
        generate_reduce_fn!(
            NonTerminal::Declarator(Declarator {
                pointer: None,
                direct_declarator: declarator.clone()
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator))
        ),
        //
        // 146 direct_declarator: IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::Identifier(declarator.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(declarator)))
        ),
        // 147                  | '(' declarator ')'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::Declarator(Box::new(
                declarator.clone()
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 148                  | direct_declarator '[' type_qualifier_list assignment_expression ']'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::ToAo(
                Box::new(declarator.clone()),
                Some(type_qualifier_list.clone()),
                Some(assignment_expression.clone())
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifierList(type_qualifier_list)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 149                  | direct_declarator '[' type_qualifier_list ']'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::ToAo(
                Box::new(declarator.clone()),
                Some(type_qualifier_list.clone()),
                None
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifierList(type_qualifier_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 150                  | direct_declarator '[' assignment_expression ']'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::ToAo(
                Box::new(declarator.clone()),
                None,
                Some(assignment_expression.clone())
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 151                  | direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::SToA(
                Box::new(declarator.clone()),
                Some(type_qualifier_list.clone()),
                assignment_expression.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Static))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifierList(type_qualifier_list)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 152                  | direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::TSA(
                Box::new(declarator.clone()),
                type_qualifier_list.clone(),
                assignment_expression.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifierList(type_qualifier_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Static))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 153                  | direct_declarator '[' type_qualifier_list '*' ']'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::ToAsterisk(
                Box::new(declarator.clone()),
                Some(type_qualifier_list.clone()),
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifierList(type_qualifier_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 154                  | direct_declarator '[' '*' ']'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::ToAsterisk(
                Box::new(declarator.clone()),
                None,
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 155                  | direct_declarator '[' ']'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::ToAo(
                Box::new(declarator.clone()),
                None,
                None
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 156                  | direct_declarator '(' parameter_type_list ')'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::P(
                Box::new(declarator.clone()),
                parameter_type_list.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ParameterTypeList(parameter_type_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 157                  | direct_declarator '(' identifier_list ')'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::I(
                Box::new(declarator.clone()),
                Some(identifier_list.clone())
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::IdentifierList(identifier_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 158                  | direct_declarator '(' ')'
        generate_reduce_fn!(
            NonTerminal::DirectDeclarator(DirectDeclarator::I(Box::new(declarator.clone()), None)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectDeclarator(declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        //
        // 159 pointer: '*'
        generate_reduce_fn!(
            NonTerminal::Pointer(Pointer {
                type_qualifier_list: None,
                pointer: None
            }),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk)))
        ),
        // 160        | '*' type_qualifier_list
        generate_reduce_fn!(
            NonTerminal::Pointer(Pointer {
                type_qualifier_list: Some(type_qualifier_list.clone()),
                pointer: None
            }),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifierList(type_qualifier_list))
        ),
        // 161        | '*' pointer
        generate_reduce_fn!(
            NonTerminal::Pointer(Pointer {
                pointer: Some(Box::new(pointer.clone())),
                type_qualifier_list: None
            }),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Pointer(pointer))
        ),
        // 162        | '*' type_qualifier_list pointer
        generate_reduce_fn!(
            NonTerminal::Pointer(Pointer {
                type_qualifier_list: Some(type_qualifier_list.clone()),
                pointer: Some(Box::new(pointer.clone())),
            }),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifierList(type_qualifier_list)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Pointer(pointer))
        ),
        //
        // 163 type_qualifier_list: type_qualifier
        generate_reduce_fn!(
            NonTerminal::TypeQualifierList(TypeQualifierList(vec![*type_qualifier])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifier(type_qualifier))
        ),
        // 164                    | type_qualifier_list type_qualifier
        generate_reduce_fn!(
            NonTerminal::TypeQualifierList({
                let mut vec = type_qualifier_list.0.clone();
                vec.push(*type_qualifier);
                TypeQualifierList(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifierList(type_qualifier_list)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TypeQualifier(type_qualifier))
        ),
        //
        // 165 parameter_type_list: parameter_list
        generate_reduce_fn!(
            NonTerminal::ParameterTypeList(ParameterTypeList {
                parameter_list: parameter_list.clone(),
                ellipsis: false
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ParameterList(parameter_list))
        ),
        // 166                    | parameter_list ',' ELLIPSIS
        generate_reduce_fn!(
            NonTerminal::ParameterTypeList(ParameterTypeList {
                parameter_list: parameter_list.clone(),
                ellipsis: true
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ParameterList(parameter_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Ellipsis))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Ellipsis)))
        ),
        //
        // 167 parameter_list: parameter_declaration
        generate_reduce_fn!(
            NonTerminal::ParameterList(ParameterList(vec![parameter_declaration.clone()])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ParameterDeclaration(
                parameter_declaration
            ))
        ),
        // 168               | parameter_list ',' parameter_declaration
        generate_reduce_fn!(
            NonTerminal::ParameterList({
                let mut vec = parameter_list.0.clone();
                vec.push(parameter_declaration.clone());
                ParameterList(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ParameterList(parameter_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ParameterDeclaration(
                parameter_declaration
            ))
        ),
        //
        // 169 parameter_declaration: declaration_specifiers declarator
        generate_reduce_fn!(
            NonTerminal::ParameterDeclaration(ParameterDeclaration {
                declaration_specifiers: declaration_specifiers.clone(),
                declarator_or_abstract_declarator: Some(
                    DeclaratorOrAbstractDeclarator::Declarator(declarator.clone())
                )
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declarator(declarator))
        ),
        // 170                      | declaration_specifiers abstract_declarator
        generate_reduce_fn!(
            NonTerminal::ParameterDeclaration(ParameterDeclaration {
                declaration_specifiers: declaration_specifiers.clone(),
                declarator_or_abstract_declarator: Some(
                    DeclaratorOrAbstractDeclarator::AbstractDeclarator(abstract_declarator.clone())
                )
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AbstractDeclarator(abstract_declarator))
        ),
        // 171                      | declaration_specifiers
        generate_reduce_fn!(
            NonTerminal::ParameterDeclaration(ParameterDeclaration {
                declaration_specifiers: declaration_specifiers.clone(),
                declarator_or_abstract_declarator: None
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            ))
        ),
        //
        // 172 identifier_list: IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::IdentifierList(IdentifierList(vec![identifier.clone()])),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier)))
        ),
        // 173                | identifier_list ',' IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::IdentifierList({
                let mut vec = identifier_list.0.clone();
                vec.push(identifier.clone());
                IdentifierList(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::IdentifierList(identifier_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier)))
        ),
        //
        // 174 type_name: specifier_qualifier_list
        generate_reduce_fn!(
            NonTerminal::TypeName(TypeName {
                specifier_qualifier_list: specifier_qualifier_list.clone(),
                abstract_declarator: None
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::SpecifierQualifierList(
                specifier_qualifier_list
            ))
        ),
        // 175          | specifier_qualifier_list abstract_declarator
        generate_reduce_fn!(
            NonTerminal::TypeName(TypeName {
                specifier_qualifier_list: specifier_qualifier_list.clone(),
                abstract_declarator: Some(abstract_declarator.clone())
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::SpecifierQualifierList(
                specifier_qualifier_list
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AbstractDeclarator(abstract_declarator))
        ),
        //
        // 176 abstract_declarator: pointer
        generate_reduce_fn!(
            NonTerminal::AbstractDeclarator(AbstractDeclarator {
                pointer: Some(pointer.clone()),
                direct_abstract_declarator: None
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Pointer(pointer))
        ),
        // 177                    | direct_abstract_declarator
        generate_reduce_fn!(
            NonTerminal::AbstractDeclarator(AbstractDeclarator {
                pointer: None,
                direct_abstract_declarator: Some(direct_abstract_declarator.clone())
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectAbstractDeclarator(
                direct_abstract_declarator
            ))
        ),
        // 178                    | pointer direct_abstract_declarator
        generate_reduce_fn!(
            NonTerminal::AbstractDeclarator(AbstractDeclarator {
                pointer: Some(pointer.clone()),
                direct_abstract_declarator: Some(direct_abstract_declarator.clone())
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Pointer(pointer)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectAbstractDeclarator(
                direct_abstract_declarator
            ))
        ),
        //
        // 179 direct_abstract_declarator: '(' abstract_declarator ')'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::AbstractDeclarator(
                Box::new(abstract_declarator.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AbstractDeclarator(abstract_declarator)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 180                           | '[' ']'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::AssignmentExpression(
                None, None
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 181                           | '[' assignment_expression ']'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::AssignmentExpression(
                None,
                Some(Box::new(assignment_expression.clone()))
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 182                           | direct_abstract_declarator '[' ']'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::AssignmentExpression(
                Some(Box::new(direct_abstract_declarator.clone())),
                None
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectAbstractDeclarator(
                direct_abstract_declarator
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 183                           | direct_abstract_declarator '[' assignment_expression ']'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::AssignmentExpression(
                Some(Box::new(direct_abstract_declarator.clone())),
                Some(Box::new(assignment_expression.clone()))
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectAbstractDeclarator(
                direct_abstract_declarator
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 184                           | '[' '*' ']'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::Asterisk(None,)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 185                           | direct_abstract_declarator '[' '*' ']'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::Asterisk(Some(
                Box::new(direct_abstract_declarator.clone())
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectAbstractDeclarator(
                direct_abstract_declarator
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(Operator::Asterisk))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 186                           | '(' ')'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::ParameterTypeList(
                None, None
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 187                           | '(' parameter_type_list ')'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::ParameterTypeList(
                None,
                Some(parameter_type_list.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ParameterTypeList(parameter_type_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 188                           | direct_abstract_declarator '(' ')'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::ParameterTypeList(
                Some(Box::new(direct_abstract_declarator.clone())),
                None
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectAbstractDeclarator(
                direct_abstract_declarator
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        // 189                           | direct_abstract_declarator '(' parameter_type_list ')'
        generate_reduce_fn!(
            NonTerminal::DirectAbstractDeclarator(DirectAbstractDeclarator::ParameterTypeList(
                Some(Box::new(direct_abstract_declarator.clone())),
                Some(parameter_type_list.clone())
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DirectAbstractDeclarator(
                direct_abstract_declarator
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ParameterTypeList(parameter_type_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            )))
        ),
        //
        // 190 initializer: assignment_expression
        generate_reduce_fn!(
            NonTerminal::Initializer(Initializer::AssignmentExpression(
                assignment_expression.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::AssignmentExpression(
                assignment_expression
            ))
        ),
        // 191            | '{' initializer_list '}'
        generate_reduce_fn!(
            NonTerminal::Initializer(Initializer::InitializerList(initializer_list.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitializerList(initializer_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 192            | '{' initializer_list ',' '}'
        generate_reduce_fn!(
            NonTerminal::Initializer(Initializer::InitializerList(initializer_list.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitializerList(initializer_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        //
        // 193 initializer_list: initializer
        generate_reduce_fn!(
            NonTerminal::InitializerList(InitializerList(vec![
                DesignatedInitializer::Initializer(initializer.clone())
            ])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Initializer(initializer))
        ),
        // 194                 | designation initializer
        generate_reduce_fn!(
            NonTerminal::InitializerList(InitializerList(vec![DesignatedInitializer::Designated(
                designation.clone(),
                initializer.clone()
            )])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Designation(designation)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Initializer(initializer))
        ),
        // 195                 | initializer_list ',' initializer
        generate_reduce_fn!(
            NonTerminal::InitializerList({
                let mut initializer_list = initializer_list.0.clone();
                initializer_list.push(DesignatedInitializer::Initializer(initializer.clone()));
                InitializerList(initializer_list)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitializerList(initializer_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Initializer(initializer))
        ),
        // 196                 | initializer_list ',' designation initializer
        generate_reduce_fn!(
            NonTerminal::InitializerList({
                let mut initializer_list = initializer_list.0.clone();
                initializer_list.push(DesignatedInitializer::Designated(
                    designation.clone(),
                    initializer.clone(),
                ));
                InitializerList(initializer_list)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::InitializerList(initializer_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Comma))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Designation(designation)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Initializer(initializer))
        ),
        //
        // 197 designation: designator_list '='
        generate_reduce_fn!(
            NonTerminal::Designation(Designation(designator_list.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DesignatorList(designator_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Operator(
                Operator::Assignment(AssignmentOperator::Equals)
            )))
        ),
        //
        // 198 designator_list: designator
        generate_reduce_fn!(
            NonTerminal::DesignatorList(DesignatorList(vec![designator.clone()])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Designator(designator))
        ),
        // 199                | designator_list designator
        generate_reduce_fn!(
            NonTerminal::DesignatorList({
                let mut designator_list = designator_list.0.clone();
                designator_list.push(designator.clone());
                DesignatorList(designator_list)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DesignatorList(designator_list)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Designator(designator))
        ),
        //
        // 200 designator: '[' constant_expression ']'
        generate_reduce_fn!(
            NonTerminal::Designator(Designator::ArrayDesignator(constant_expression.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBracket
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ConstantExpression(constant_expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBracket
            )))
        ),
        // 201           | '.' IDENTIFIER
        generate_reduce_fn!(
            NonTerminal::Designator(Designator::MemberDesignator(identifier.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Period))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier)))
        ),
        //
        // 202 statement: labeled_statement
        generate_reduce_fn!(
            NonTerminal::Statement(Statement::Labeled(labeled_statement.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::LabeledStatement(labeled_statement))
        ),
        // 203          | compound_statement
        generate_reduce_fn!(
            NonTerminal::Statement(Statement::Compound(compound_statement.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::CompoundStatement(compound_statement))
        ),
        // 204          | expression_statement
        generate_reduce_fn!(
            NonTerminal::Statement(Statement::Expression(expression_statement.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExpressionStatement(expression_statement))
        ),
        // 205          | selection_statement
        generate_reduce_fn!(
            NonTerminal::Statement(Statement::Selection(selection_statement.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::SelectionStatement(selection_statement))
        ),
        // 206          | iteration_statement
        generate_reduce_fn!(
            NonTerminal::Statement(Statement::Iteration(iteration_statement.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::IterationStatement(iteration_statement))
        ),
        // 207          | jump_statement
        generate_reduce_fn!(
            NonTerminal::Statement(Statement::Jump(jump_statement.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::JumpStatement(jump_statement))
        ),
        //
        // 208 labeled_statement: IDENTIFIER ':' statement
        generate_reduce_fn!(
            NonTerminal::LabeledStatement(LabeledStatement::Identifier(
                identifier.clone(),
                Box::new(statement.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(identifier))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Colon))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        // 209                  | CASE constant_expression ':' statement
        generate_reduce_fn!(
            NonTerminal::LabeledStatement(LabeledStatement::Case(
                constant_expression.clone(),
                Box::new(statement.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Case))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ConstantExpression(constant_expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Colon))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        // 210                  | DEFAULT ':' statement
        generate_reduce_fn!(
            NonTerminal::LabeledStatement(LabeledStatement::Default(Box::new(statement.clone()))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::DefaultCase))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(Delimiter::Colon))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        //
        // 211 compound_statement: '{' '}'
        generate_reduce_fn!(
            NonTerminal::CompoundStatement(CompoundStatement(None)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        // 212                   | '{' block_item_list '}'
        generate_reduce_fn!(
            NonTerminal::CompoundStatement(CompoundStatement(Some(block_item_list.clone()))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftBrace
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::BlockItemList(block_item_list)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightBrace
            )))
        ),
        //
        // 213 block_item_list: block_item
        generate_reduce_fn!(
            NonTerminal::BlockItemList(BlockItemList(vec![block_item.clone()])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::BlockItem(block_item))
        ),
        // 214                | block_item_list block_item
        generate_reduce_fn!(
            NonTerminal::BlockItemList({
                let mut block_item_list = block_item_list.0.clone();
                block_item_list.push(block_item.clone());
                BlockItemList(block_item_list)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::BlockItemList(block_item_list)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::BlockItem(block_item))
        ),
        //
        // 215 block_item: declaration
        generate_reduce_fn!(
            NonTerminal::BlockItem(BlockItem::Declaration(declaration.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declaration(declaration))
        ),
        // 216           | statement
        generate_reduce_fn!(
            NonTerminal::BlockItem(BlockItem::Statement(statement.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        //
        // 217 expression_statement: ';'
        generate_reduce_fn!(
            NonTerminal::ExpressionStatement(ExpressionStatement(None)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        // 218                     | expression ';'
        generate_reduce_fn!(
            NonTerminal::ExpressionStatement(ExpressionStatement(Some(expression.clone()))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        //
        // 219 selection_statement: IF '(' expression ')' statement
        generate_reduce_fn!(
            NonTerminal::SelectionStatement(SelectionStatement::If(
                expression.clone(),
                Box::new(statement.clone()),
                None
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::If))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        // 220                    | IF '(' expression ')' statement ELSE statement
        generate_reduce_fn!(
            NonTerminal::SelectionStatement(SelectionStatement::If(
                expression.clone(),
                Box::new(statement.clone()),
                Some(Box::new(else_statement.clone()))
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::If))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Else))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(else_statement))
        ),
        // 221                    | SWITCH '(' expression ')' statement
        generate_reduce_fn!(
            NonTerminal::SelectionStatement(SelectionStatement::Switch(
                expression.clone(),
                Box::new(statement.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Switch))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        //
        // 222 iteration_statement: WHILE '(' expression ')' statement
        generate_reduce_fn!(
            NonTerminal::IterationStatement(IterationStatement::While(
                expression.clone(),
                Box::new(statement.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::While))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        // 223                    | DO statement WHILE '(' expression ')' ';'
        generate_reduce_fn!(
            NonTerminal::IterationStatement(IterationStatement::DoWhile(
                Box::new(statement.clone()),
                expression.clone()
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Do))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::While))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        // 224                    | FOR '(' expression_statement expression_statement ')' statement
        generate_reduce_fn!(
            NonTerminal::IterationStatement(IterationStatement::For(
                expression_statement1.clone(),
                expression_statement2.clone(),
                None,
                Box::new(statement.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::For))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExpressionStatement(
                expression_statement1
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExpressionStatement(
                expression_statement2
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        // 225                    | FOR '(' expression_statement expression_statement expression ')' statement
        generate_reduce_fn!(
            NonTerminal::IterationStatement(IterationStatement::For(
                expression_statement1.clone(),
                expression_statement2.clone(),
                Some(expression.clone()),
                Box::new(statement.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::For))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExpressionStatement(
                expression_statement1
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExpressionStatement(
                expression_statement2
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        // 226                    | FOR '(' declaration expression_statement ')' statement
        generate_reduce_fn!(
            NonTerminal::IterationStatement(IterationStatement::ForWithDeclaration(
                declaration.clone(),
                expression_statement.clone(),
                None,
                Box::new(statement.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::For))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declaration(declaration)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExpressionStatement(expression_statement)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        // 227                    | FOR '(' declaration expression_statement expression ')' statement
        generate_reduce_fn!(
            NonTerminal::IterationStatement(IterationStatement::ForWithDeclaration(
                declaration.clone(),
                expression_statement.clone(),
                Some(expression.clone()),
                Box::new(statement.clone())
            )),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::For))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::LeftParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declaration(declaration)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExpressionStatement(expression_statement)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::RightParen
            ))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Statement(statement))
        ),
        //
        // 228 jump_statement: GOTO IDENTIFIER ';'
        generate_reduce_fn!(
            NonTerminal::JumpStatement(JumpStatement::Goto(declarator.clone())),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Goto))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Identifier(declarator))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        // 229               | CONTINUE ';'
        generate_reduce_fn!(
            NonTerminal::JumpStatement(JumpStatement::Continue),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Continue))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        // 230               | BREAK ';'
        generate_reduce_fn!(
            NonTerminal::JumpStatement(JumpStatement::Break),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Break))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        // 231               | RETURN ';'
        generate_reduce_fn!(
            NonTerminal::JumpStatement(JumpStatement::Return(None)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Return))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        // 232               | RETURN expression ';'
        generate_reduce_fn!(
            NonTerminal::JumpStatement(JumpStatement::Return(Some(expression.clone()))),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Keyword(Keyword::Return))),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Expression(expression)),
            TokenOrNonTerminal::Token(token_kind_match!(TokenKind::Delimiter(
                Delimiter::Semicolon
            )))
        ),
        //
        // 233 translation_unit: external_declaration
        generate_reduce_fn!(
            NonTerminal::TranslationUnit(TranslationUnit(vec![external_declaration.clone()])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExternalDeclaration(external_declaration))
        ),
        // 234                 | translation_unit external_declaration
        generate_reduce_fn!(
            NonTerminal::TranslationUnit({
                let mut vec = translation_unit.0.clone();
                vec.push(external_declaration.clone());
                TranslationUnit(vec)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::TranslationUnit(translation_unit)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::ExternalDeclaration(external_declaration))
        ),
        //
        // 235 external_declaration: function_definition
        generate_reduce_fn!(
            NonTerminal::ExternalDeclaration(ExternalDeclaration::FunctionDefinition(
                function_definition.clone()
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::FunctionDefinition(function_definition))
        ),
        // 236                     | declaration
        generate_reduce_fn!(
            NonTerminal::ExternalDeclaration(ExternalDeclaration::Declaration(declaration.clone())),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declaration(declaration))
        ),
        //
        // 237 function_definition: declaration_specifiers declarator declaration_list compound_statement
        generate_reduce_fn!(
            NonTerminal::FunctionDefinition(FunctionDefinition {
                declaration_specifiers: declaration_specifiers.clone(),
                declarator: declarator.clone(),
                declaration_list: Some(list.clone()),
                compound_statement: compound_statement.clone()
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declarator(declarator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationList(list)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::CompoundStatement(compound_statement))
        ),
        // 238                    | declaration_specifiers declarator compound_statement
        generate_reduce_fn!(
            NonTerminal::FunctionDefinition(FunctionDefinition {
                declaration_specifiers: declaration_specifiers.clone(),
                declarator: declarator.clone(),
                declaration_list: None,
                compound_statement: compound_statement.clone()
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationSpecifiers(
                declaration_specifiers
            )),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declarator(declarator)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::CompoundStatement(compound_statement))
        ),
        //
        // 239 declaration_list: declaration
        generate_reduce_fn!(
            NonTerminal::DeclarationList(DeclarationList(vec![declaration.clone()])),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declaration(declaration))
        ),
        // 240                 | declaration_list declaration
        generate_reduce_fn!(
            NonTerminal::DeclarationList({
                let mut list = list.0.clone();
                list.push(declaration.clone());
                DeclarationList(list)
            }),
            TokenOrNonTerminal::NonTerminal(NonTerminal::DeclarationList(list)),
            TokenOrNonTerminal::NonTerminal(NonTerminal::Declaration(declaration))
        ),
    ]
}
