#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    InvalidCharacter(char),
    InvalidNumericLiteral(NumericLiteralError),
    InvalidStringLiteral(StringCharLiteralError),
    InvalidCharLiteral(StringCharLiteralError),
    InvalidBlockComment(BlockCommentError),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumericLiteralError {
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    InvalidDigit(char),
    UnexpectedEOF,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum StringCharLiteralError {
    Unterminated,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BlockCommentError {
    Unterminated,
}

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Numeric(NumericLiteral),
    String(String),
    Char(String),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {
    If,
    Else,
    While,
    Return,
    For,
    Break,
    Continue,
    Const,
    Enum,
    Auto,
    Case,
    DefaultCase,
    Do,
    Extern,
    Goto,
    Long,
    Register,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Volatile,
    Inline,
    Restrict,
    Int,
    Float,
    Double,
    Char,
    Void,
    _Bool,
    _Complex,
    _Imaginary,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Delimiter {
    LeftParen,
    RightParen,
    Semicolon,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Comma,
    Period,
    Ellipsis,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AssignmentOperator {
    Equals,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    SlashEquals,
    PercentEquals,
    CaretEquals,
    AmpersandEquals,
    PipeEquals,
    LeftShiftEquals,
    RightShiftEquals,
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operator {
    Assignment(AssignmentOperator), // = , +=, -=, *= , etc.
    Minus,
    Exclamation,
    Asterisk,
    Ampersand,
    Tilde,
    Plus,
    Slash,
    EqualEqual,
    ExclamationEqual,
    Percent,
    Caret,
    Pipe,
    LessThan,
    LessThanEquals,
    LeftShift,
    GreaterThan,
    GreaterThanEquals,
    RightShift,
    PipePipe,
    PlusPlus,
    MinusMinus,
    AmpersandAmpersand,
    Question,
    Arrow,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    EOF,
    Literal(Literal),   // integer, string, char, bool, etc. literals
    Identifier(String), // names for variables, functions, macros, custom types, etc.
    Keyword(Keyword),   // reserved keywords like if, else, while, return, etc.
    Delimiter(Delimiter),
    Operator(Operator),
    Whitespace,           // whitespace characters like space, tab, newline, etc.
    LineComment(String),  // single line comments starting with //
    BlockComment(String), // multi line comments starting with /* and ending with */
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextSpan {
    start: usize,
    end: usize,
    literal: String,
}
impl TextSpan {
    pub fn new(start: usize, end: usize, literal: String) -> Self {
        Self {
            start,
            end,
            literal,
        }
    }
    pub fn length(&self) -> usize {
        self.end - self.start
    }
}
#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    span: TextSpan,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl Token {
    pub fn new(kind: TokenKind, span: TextSpan) -> Self {
        Self { kind, span }
    }
    pub fn get_kind(&self) -> TokenKind {
        self.kind.clone()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Lexer<'a> {
    input: &'a str,
    current_pos: usize,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NumericLiteralState {
    ZeroPrefixed,
    Hexadecimal,
    Binary,
    Octal,
    Decimal,
    FloatOrDouble,
    HexadecimalStart,
    BinaryStart,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumericLiteral {
    Integer(IntegerLiteral),
    Float(f32),
    Double(f64),
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntegerLiteral {
    Decimal(i64),
    Hexadecimal(i64),
    Binary(i64),
    Octal(i64),
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            current_pos: 0,
        }
    }

    pub fn lex_string(input: &str) -> LexerResult<Vec<Token>> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while let Some(token) = lexer.next_token()? {
            match token.kind {
                TokenKind::Whitespace | TokenKind::LineComment(_) | TokenKind::BlockComment(_) => {}
                _ => tokens.push(token),
            }
        }
        Ok(tokens)
    }
    pub fn next_token(&mut self) -> LexerResult<Option<Token>> {
        if self.current_pos > self.input.len() {
            return Ok(None);
        }
        if self.current_pos == self.input.len() {
            self.current_pos += 1;
            return Ok(Some(Token::new(
                TokenKind::EOF,
                TextSpan::new(0, 0, '\0'.to_string()),
            )));
        }
        let start = self.current_pos;
        let c = self.current_char().unwrap();
        let mut kind = TokenKind::EOF;
        if Lexer::is_numeric_literal_start(&c) {
            let number = self.consume_number()?;
            kind = TokenKind::Literal(Literal::Numeric(number));
        } else if Lexer::is_whitespace_start(&c) {
            self.consume_whitespace();
            kind = TokenKind::Whitespace;
        } else if Lexer::is_string_start(&c) {
            let string = self.consume_string()?;
            kind = TokenKind::Literal(Literal::String(string));
        } else if Lexer::is_char_start(&c) {
            let char = self.consume_char()?;
            kind = TokenKind::Literal(Literal::Char(char));
        } else if Lexer::is_identifier_start(&c) {
            let identifier = self.consume_identifier();
            match identifier.as_str() {
                "auto" => kind = TokenKind::Keyword(Keyword::Auto),
                "break" => kind = TokenKind::Keyword(Keyword::Break),
                "_Bool" => kind = TokenKind::Keyword(Keyword::_Bool),
                "case" => kind = TokenKind::Keyword(Keyword::Case),
                "char" => kind = TokenKind::Keyword(Keyword::Char),
                "_Complex" => kind = TokenKind::Keyword(Keyword::_Complex),
                "const" => kind = TokenKind::Keyword(Keyword::Const),
                "continue" => kind = TokenKind::Keyword(Keyword::Continue),
                "default" => kind = TokenKind::Keyword(Keyword::DefaultCase),
                "do" => kind = TokenKind::Keyword(Keyword::Do),
                "double" => kind = TokenKind::Keyword(Keyword::Double),
                "else" => kind = TokenKind::Keyword(Keyword::Else),
                "enum" => kind = TokenKind::Keyword(Keyword::Enum),
                "extern" => kind = TokenKind::Keyword(Keyword::Extern),
                "float" => kind = TokenKind::Keyword(Keyword::Float),
                "for" => kind = TokenKind::Keyword(Keyword::For),
                "goto" => kind = TokenKind::Keyword(Keyword::Goto),
                "if" => kind = TokenKind::Keyword(Keyword::If),
                "_Imaginary" => kind = TokenKind::Keyword(Keyword::_Imaginary),
                "inline" => kind = TokenKind::Keyword(Keyword::Inline),
                "int" => kind = TokenKind::Keyword(Keyword::Int),
                "long" => kind = TokenKind::Keyword(Keyword::Long),
                "register" => kind = TokenKind::Keyword(Keyword::Register),
                "restrict" => kind = TokenKind::Keyword(Keyword::Restrict),
                "return" => kind = TokenKind::Keyword(Keyword::Return),
                "short" => kind = TokenKind::Keyword(Keyword::Short),
                "signed" => kind = TokenKind::Keyword(Keyword::Signed),
                "sizeof" => kind = TokenKind::Keyword(Keyword::Sizeof),
                "static" => kind = TokenKind::Keyword(Keyword::Static),
                "struct" => kind = TokenKind::Keyword(Keyword::Struct),
                "switch" => kind = TokenKind::Keyword(Keyword::Switch),
                "typedef" => kind = TokenKind::Keyword(Keyword::Typedef),
                "union" => kind = TokenKind::Keyword(Keyword::Union),
                "unsigned" => kind = TokenKind::Keyword(Keyword::Unsigned),
                "void" => kind = TokenKind::Keyword(Keyword::Void),
                "volatile" => kind = TokenKind::Keyword(Keyword::Volatile),
                "while" => kind = TokenKind::Keyword(Keyword::While),
                _ => kind = TokenKind::Identifier(identifier),
            }
        } else {
            match c {
                '.' => {
                    self.consume();
                    match (self.current_char(), self.peek_next_char()) {
                        (Some('.'), Some('.')) => {
                            kind = TokenKind::Delimiter(Delimiter::Ellipsis);
                            self.consume();
                            self.consume();
                        }
                        _ => kind = TokenKind::Delimiter(Delimiter::Period),
                    }
                }
                ';' => {
                    kind = TokenKind::Delimiter(Delimiter::Semicolon);
                    self.consume();
                }
                '=' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::EqualEqual);
                            self.consume();
                        }
                        _ => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::Equals,
                            ))
                        }
                    }
                }
                '+' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::PlusEquals,
                            ));
                            self.consume();
                        }
                        Some('+') => {
                            kind = TokenKind::Operator(Operator::PlusPlus);
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::Plus),
                    }
                }
                '-' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::MinusEquals,
                            ));
                            self.consume();
                        }
                        Some('-') => {
                            kind = TokenKind::Operator(Operator::MinusMinus);
                            self.consume();
                        }
                        Some('>') => {
                            kind = TokenKind::Operator(Operator::Arrow);
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::Minus),
                    }
                }
                '*' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::AsteriskEquals,
                            ));
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::Asterisk),
                    }
                }
                '~' => {
                    kind = TokenKind::Operator(Operator::Tilde);
                    self.consume();
                }
                '/' => match self.peek_next_char() {
                    Some('/') => {
                        let comment = self.consume_line_comment();
                        kind = TokenKind::LineComment(comment);
                    }
                    Some('*') => {
                        let comment = self.consume_block_comment()?;
                        kind = TokenKind::BlockComment(comment);
                    }
                    Some('=') => {
                        kind = TokenKind::Operator(Operator::Assignment(
                            AssignmentOperator::SlashEquals,
                        ));
                        self.consume();
                        self.consume();
                    }
                    _ => {
                        kind = TokenKind::Operator(Operator::Slash);
                        self.consume();
                    }
                },
                '(' => {
                    kind = TokenKind::Delimiter(Delimiter::LeftParen);
                    self.consume();
                }
                ')' => {
                    kind = TokenKind::Delimiter(Delimiter::RightParen);
                    self.consume();
                }
                '{' => {
                    kind = TokenKind::Delimiter(Delimiter::LeftBrace);
                    self.consume();
                }
                '}' => {
                    kind = TokenKind::Delimiter(Delimiter::RightBrace);
                    self.consume();
                }
                '[' => {
                    kind = TokenKind::Delimiter(Delimiter::LeftBracket);
                    self.consume();
                }
                ']' => {
                    kind = TokenKind::Delimiter(Delimiter::RightBracket);
                    self.consume();
                }
                ':' => {
                    kind = TokenKind::Delimiter(Delimiter::Colon);
                    self.consume();
                }
                ',' => {
                    kind = TokenKind::Delimiter(Delimiter::Comma);
                    self.consume();
                }
                '!' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::ExclamationEqual);
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::Exclamation),
                    }
                }
                '%' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::PercentEquals,
                            ));
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::Percent),
                    }
                }
                '^' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::CaretEquals,
                            ));
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::Caret),
                    }
                }
                '&' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::AmpersandEquals,
                            ));
                            self.consume();
                        }
                        Some('&') => {
                            kind = TokenKind::Operator(Operator::AmpersandAmpersand);
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::Ampersand),
                    }
                }
                '?' => {
                    kind = TokenKind::Operator(Operator::Question);
                    self.consume();
                }
                '|' => {
                    self.consume();
                    match self.current_char() {
                        Some('=') => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::PipeEquals,
                            ));
                            self.consume();
                        }
                        Some('|') => {
                            kind = TokenKind::Operator(Operator::PipePipe);
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::Pipe),
                    }
                }
                '<' => {
                    self.consume();
                    match (self.current_char(), self.peek_next_char()) {
                        (Some('='), _) => {
                            kind = TokenKind::Operator(Operator::LessThanEquals);
                            self.consume();
                        }
                        (Some('<'), Some('=')) => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::LeftShiftEquals,
                            ));
                            self.consume();
                            self.consume();
                        }
                        (Some('<'), _) => {
                            kind = TokenKind::Operator(Operator::LeftShift);
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::LessThan),
                    }
                }
                '>' => {
                    self.consume();
                    match (self.current_char(), self.peek_next_char()) {
                        (Some('='), _) => {
                            kind = TokenKind::Operator(Operator::GreaterThanEquals);
                            self.consume();
                        }
                        (Some('>'), Some('=')) => {
                            kind = TokenKind::Operator(Operator::Assignment(
                                AssignmentOperator::RightShiftEquals,
                            ));
                            self.consume();
                            self.consume();
                        }
                        (Some('>'), _) => {
                            kind = TokenKind::Operator(Operator::RightShift);
                            self.consume();
                        }
                        _ => kind = TokenKind::Operator(Operator::GreaterThan),
                    }
                }
                c => {
                    self.consume_rest();
                    return Err(LexerError::InvalidCharacter(c));
                }
            }
        }
        let end = self.current_pos;
        let literal = self.input[start..end].to_string();
        let span = TextSpan::new(start, end, literal);
        Ok(Some(Token::new(kind, span)))
    }
    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.current_pos)
    }
    fn peek_next_char(&self) -> Option<char> {
        self.input.chars().nth(self.current_pos + 1)
    }
    fn consume(&mut self) -> Option<char> {
        let c = self.current_char();
        self.current_pos += 1;
        c
    }

    fn is_numeric_literal_start(c: &char) -> bool {
        c.is_digit(10)
    }
    fn is_whitespace_start(c: &char) -> bool {
        c.is_whitespace()
    }
    fn is_string_start(c: &char) -> bool {
        c == &'\"'
    }
    fn is_char_start(c: &char) -> bool {
        c == &'\''
    }
    fn is_identifier_start(c: &char) -> bool {
        c.is_alphabetic() || c == &'_'
    }

    fn is_whitespace(c: &char) -> bool {
        c.is_whitespace()
    }

    fn consume_number(&mut self) -> LexerResult<NumericLiteral> {
        let mut number_str = String::new();
        let first_char = self.consume().unwrap();
        number_str.push(first_char);
        let mut state = match first_char {
            '0' => NumericLiteralState::ZeroPrefixed,
            '1'..='9' => NumericLiteralState::Decimal,
            _ => unreachable!(),
        };
        while let Some(c) = match self.consume() {
            Some(ok) => Some(ok),
            None => {
                println!("{}", self.current_pos);
                if self.current_pos <= self.input.len() + 1 {
                    Some('\0')
                } else {
                    None
                }
            }
        } {
            match state {
                NumericLiteralState::ZeroPrefixed => match c {
                    'x' | 'X' => state = NumericLiteralState::HexadecimalStart,
                    'b' | 'B' => state = NumericLiteralState::BinaryStart,
                    '.' => state = NumericLiteralState::FloatOrDouble,
                    '0'..='7' => state = NumericLiteralState::Octal,
                    '8' | '9' => {
                        return Err(LexerError::InvalidNumericLiteral(
                            NumericLiteralError::InvalidDigit(c),
                        ))
                    }
                    _ => {
                        self.current_pos -= 1;
                        return Ok(NumericLiteral::Integer(IntegerLiteral::Decimal(0)));
                    }
                },
                NumericLiteralState::HexadecimalStart => match c {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => state = NumericLiteralState::Hexadecimal,
                    c => {
                        return Err(LexerError::InvalidNumericLiteral(
                            NumericLiteralError::InvalidDigit(c),
                        ))
                    }
                },
                NumericLiteralState::Hexadecimal => match c {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => state = NumericLiteralState::Hexadecimal,
                    _ => {
                        self.current_pos -= 1;
                        return i64::from_str_radix(&number_str[2..], 16)
                            .map(|number| {
                                NumericLiteral::Integer(IntegerLiteral::Hexadecimal(number))
                            })
                            .map_err(|err| {
                                LexerError::InvalidNumericLiteral(
                                    NumericLiteralError::ParseIntError(err),
                                )
                            });
                    }
                },
                NumericLiteralState::BinaryStart => match c {
                    '0' | '1' => state = NumericLiteralState::Binary,
                    c => {
                        return Err(LexerError::InvalidNumericLiteral(
                            NumericLiteralError::InvalidDigit(c),
                        ))
                    }
                },
                NumericLiteralState::Binary => match c {
                    '0' | '1' => state = NumericLiteralState::Binary,
                    _ => {
                        self.current_pos -= 1;
                        return i64::from_str_radix(&number_str[2..], 2)
                            .map(|number| NumericLiteral::Integer(IntegerLiteral::Binary(number)))
                            .map_err(|err| {
                                LexerError::InvalidNumericLiteral(
                                    NumericLiteralError::ParseIntError(err),
                                )
                            });
                    }
                },
                NumericLiteralState::Octal => match c {
                    '0'..='7' => state = NumericLiteralState::Octal,
                    _ => {
                        self.current_pos -= 1;
                        return i64::from_str_radix(&number_str[1..], 8)
                            .map(|number| NumericLiteral::Integer(IntegerLiteral::Octal(number)))
                            .map_err(|err| {
                                LexerError::InvalidNumericLiteral(
                                    NumericLiteralError::ParseIntError(err),
                                )
                            });
                    }
                },
                NumericLiteralState::Decimal => match c {
                    '0'..='9' => state = NumericLiteralState::Decimal,
                    '.' => state = NumericLiteralState::FloatOrDouble,
                    _ => {
                        self.current_pos -= 1;
                        return number_str
                            .parse()
                            .map(|number| NumericLiteral::Integer(IntegerLiteral::Decimal(number)))
                            .map_err(|err| {
                                LexerError::InvalidNumericLiteral(
                                    NumericLiteralError::ParseIntError(err),
                                )
                            });
                    }
                },
                NumericLiteralState::FloatOrDouble => match c {
                    '0'..='9' => state = NumericLiteralState::FloatOrDouble,
                    'f' | 'F' => {
                        return number_str
                            .parse::<f32>()
                            .map(|number| NumericLiteral::Float(number))
                            .map_err(|err| {
                                LexerError::InvalidNumericLiteral(
                                    NumericLiteralError::ParseFloatError(err),
                                )
                            });
                    }
                    _ => {
                        self.current_pos -= 1;
                        return number_str
                            .parse::<f64>()
                            .map(|number| NumericLiteral::Double(number))
                            .map_err(|err| {
                                LexerError::InvalidNumericLiteral(
                                    NumericLiteralError::ParseFloatError(err),
                                )
                            });
                    }
                },
            };
            number_str.push(c);
        }
        Err(LexerError::InvalidNumericLiteral(
            NumericLiteralError::UnexpectedEOF,
        ))
    }
    fn consume_whitespace(&mut self) {
        while let Some(c) = self.current_char() {
            if !Lexer::is_whitespace(&c) {
                break;
            }
            self.consume();
        }
    }
    fn consume_string(&mut self) -> LexerResult<String> {
        let mut string = String::new();
        let mut escaping = false;
        self.consume();
        while let Some(c) = self.consume() {
            string.push(c);
            if c == '\"' && !escaping {
                string.pop();
                return Ok(string);
            }
            escaping = c == '\\' && !escaping;
        }
        Err(LexerError::InvalidStringLiteral(
            StringCharLiteralError::Unterminated,
        ))
    }
    fn consume_char(&mut self) -> LexerResult<String> {
        let mut string = String::new();
        let mut escaping = false;
        self.consume();
        while let Some(c) = self.consume() {
            string.push(c);
            if c == '\'' && !escaping {
                string.pop();
                return Ok(string);
            }
            escaping = c == '\\' && !escaping;
        }
        Err(LexerError::InvalidCharLiteral(
            StringCharLiteralError::Unterminated,
        ))
    }
    fn consume_identifier(&mut self) -> String {
        let mut ident = String::new();
        ident.push(self.consume().unwrap());
        while let Some(c) = self.current_char() {
            match c {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {}
                _ => break,
            }
            ident.push(self.consume().unwrap());
        }
        ident
    }
    fn consume_rest(&mut self) {
        self.current_pos = self.input.len();
    }
    fn consume_line_comment(&mut self) -> String {
        let mut line_comment = String::new();
        self.consume();
        self.consume();
        while let Some(c) = self.current_char() {
            if c == '\n' || c == '\r' {
                break;
            }
            line_comment.push(self.consume().unwrap());
        }
        line_comment
    }
    fn consume_block_comment(&mut self) -> LexerResult<String> {
        let mut comment = String::new();
        self.consume();
        self.consume();
        while let Some(c) = self.consume() {
            if c == '/' {
                if let Some('*') = comment.chars().last() {
                    comment.pop();
                    return Ok(comment);
                }
            }
            comment.push(c);
        }
        Err(LexerError::InvalidBlockComment(
            BlockCommentError::Unterminated,
        ))
    }
}
