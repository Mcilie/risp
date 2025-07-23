#[derive(Debug, Clone, PartialEq)]
pub enum RispError {
    // Lexer errors
    UnexpectedCharacter(char),
    InvalidBoolean(String),

    // Parser errors
    UnexpectedToken(String),
    UnexpectedEndOfInput,
    MissingClosingParen,

    // Evaluator errors
    UnboundVariable(String),
    EmptyList,
    InvalidDefine(String),
    InvalidLambda(String),
    InvalidIf(String),
    ArityMismatch { expected: usize, got: usize },
    TypeMismatch { expected: String, got: String },
    NotCallable(String),

    // Built-in procedure errors
    DivisionByZero,
    InvalidArithmetic(String),

    // Library interface errors
    CannotConvertToInteger(String),
}

impl std::fmt::Display for RispError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            // Lexer errors
            RispError::UnexpectedCharacter(ch) => write!(f, "Unexpected character: '{}'", ch),
            RispError::InvalidBoolean(s) => write!(f, "Invalid boolean formation: '{}'", s),

            // Parser errors
            RispError::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            RispError::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            RispError::MissingClosingParen => write!(f, "Missing closing parenthesis"),

            // Evaluator errors
            RispError::UnboundVariable(name) => write!(f, "Unbound variable: {}", name),
            RispError::EmptyList => write!(f, "Cannot evaluate empty list"),
            RispError::InvalidDefine(msg) => write!(f, "Invalid define: {}", msg),
            RispError::InvalidLambda(msg) => write!(f, "Invalid lambda: {}", msg),
            RispError::InvalidIf(msg) => write!(f, "Invalid if: {}", msg),
            RispError::ArityMismatch { expected, got } => {
                write!(
                    f,
                    "Arity mismatch: expected {} arguments, got {}",
                    expected, got
                )
            }
            RispError::TypeMismatch { expected, got } => {
                write!(f, "Type mismatch: expected {}, got {}", expected, got)
            }
            RispError::NotCallable(value) => write!(f, "Cannot call non-function value: {}", value),

            // Built-in procedure errors
            RispError::DivisionByZero => write!(f, "Division by zero"),
            RispError::InvalidArithmetic(msg) => write!(f, "Invalid arithmetic: {}", msg),

            // Library interface errors
            RispError::CannotConvertToInteger(value) => {
                write!(f, "Cannot convert to integer: {}", value)
            }
        }
    }
}

impl std::error::Error for RispError {}
