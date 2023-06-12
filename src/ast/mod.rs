use std::{any::Any, fmt::Display, rc::Rc};

use crate::token::Token;

pub trait AsAny: 'static {
    fn as_any(&self) -> &dyn Any;
}

impl<T: 'static> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Statement {
    pub fn token_literal(&self) -> &str {
        match self {
            Statement::LetStatement(x) => x.token_literal(),
            Statement::ReturnStatement(x) => x.token_literal(),
            Statement::ExpressionStatement(x) => x.token_literal(),
            Statement::BlockStatement(x) => x.token_literal(),
        }
    }
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(x) => write!(f, "{}", x),
            Statement::ReturnStatement(x) => write!(f, "{}", x),
            Statement::ExpressionStatement(x) => write!(f, "{}", x),
            Statement::BlockStatement(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    InfixExpression(InfixExpression),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    Identifier(Identifier),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

impl Expression {
    pub fn token_literal(&self) -> &str {
        match self {
            Expression::InfixExpression(x) => x.token_literal(),
            Expression::IntegerLiteral(x) => x.token_literal(),
            Expression::PrefixExpression(x) => x.token_literal(),
            Expression::Boolean(x) => x.token_literal(),
            Expression::IfExpression(x) => x.token_literal(),
            Expression::Identifier(x) => x.token_literal(),
            Expression::FunctionLiteral(x) => x.token_literal(),
            Expression::CallExpression(x) => x.token_literal(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::InfixExpression(x) => write!(f, "{}", x),
            Expression::IntegerLiteral(x) => write!(f, "{}", x),
            Expression::PrefixExpression(x) => write!(f, "{}", x),
            Expression::Boolean(x) => write!(f, "{}", x),
            Expression::IfExpression(x) => write!(f, "{}", x),
            Expression::Identifier(x) => write!(f, "{}", x),
            Expression::FunctionLiteral(x) => write!(f, "{}", x),
            Expression::CallExpression(x) => write!(f, "{}", x),
        }
    }
}

pub trait NodeTrait {
    fn token_literal(&self) -> &str;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl NodeTrait for Program {
    fn token_literal(&self) -> &str {
        match self.statements.len() {
            0 => "",
            _ => self.statements.first().unwrap().token_literal(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Rc<Token>,
    pub name: Identifier,
    pub value: Expression,
}

impl NodeTrait for LetStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token_literal(),
            self.name,
            self.value
        )
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Rc<Token>,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal())
    }
}

impl NodeTrait for Identifier {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Rc<Token>,
    pub return_value: Option<Expression>,
}

impl NodeTrait for ReturnStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.return_value {
            Some(expr) => write!(f, "{} {}", self.token_literal(), expr),
            None => write!(f, "{}", self.token_literal()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Rc<Token>,
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

impl NodeTrait for ExpressionStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Rc<Token>,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl NodeTrait for IntegerLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Rc<Token>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl NodeTrait for PrefixExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Rc<Token>,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl NodeTrait for InfixExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Rc<Token>,
    pub value: bool,
}

impl NodeTrait for Boolean {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Rc<Token>,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl NodeTrait for IfExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Rc<Token>,
    pub statements: Vec<Statement>,
}

impl NodeTrait for BlockStatement {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Rc<Token>,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl NodeTrait for FunctionLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn ({}) {}",
            self.parameters
                .iter()
                .map(|p| p.token_literal())
                .collect::<Vec<_>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Rc<Token>,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl NodeTrait for CallExpression {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
