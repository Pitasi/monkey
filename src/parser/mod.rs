use std::rc::Rc;

use crate::{
    ast::{
        BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
        Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
        Program, ReturnStatement, Statement, StringLiteral,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

pub struct Parser {
    l: Lexer,

    cur_token: Rc<Token>,
    peek_token: Rc<Token>,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(l: Lexer) -> Self {
        let mut s = Self {
            l,
            cur_token: Rc::new(Token::EOF),
            peek_token: Rc::new(Token::EOF),
            errors: vec![],
        };
        s.next_token();
        s.next_token();
        s
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = Rc::new(self.l.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while *self.cur_token != Token::EOF {
            let stmt = self.parse_statement();

            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match *self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.cur_token.clone();

        if !self.expect_ident() {
            return None;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
        };

        if !self.expect_peek(Token::ASSIGN) {
            return None;
        }
        self.next_token();

        let value = self.parse_expression(Precedence::LOWEST);
        if value.is_none() {
            return None;
        }

        if !self.expect_peek(Token::SEMICOLON) {
            return None;
        }

        Some(Statement::LetStatement(LetStatement {
            token: let_token.clone(),
            name,
            value: value.unwrap(),
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let ret_token = self.cur_token.clone();
        self.next_token();

        let return_value = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(Token::SEMICOLON) {
            return None;
        }

        Some(Statement::ReturnStatement(ReturnStatement {
            token: ret_token,
            return_value,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        let expression = self.parse_expression(Precedence::LOWEST);
        if expression.is_none() {
            return None;
        }

        // optional semicolon
        if *self.peek_token == Token::SEMICOLON {
            self.next_token();
        }

        Some(Statement::ExpressionStatement(ExpressionStatement {
            token,
            expression: expression.unwrap(),
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left_exp = self.parse_prefix_expression();
        if left_exp.is_none() {
            self.errors.push(format!(
                "no prefix parse function for {:?} defined",
                *self.cur_token,
            ));
            return None;
        }

        while *self.peek_token != Token::SEMICOLON && precedence < self.peek_precedence() {
            if !self.can_parse_infix_expression(&*self.peek_token) {
                return left_exp;
            }

            self.next_token();

            left_exp = self.parse_infix_expression(left_exp.unwrap());
        }

        left_exp
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        match *self.cur_token {
            Token::IDENT(_) => Some(Expression::Identifier(self.parse_identifier_expression())),
            Token::INT(_) => self.parse_integer_literal_expression(),
            Token::TRUE | Token::FALSE => self.parse_boolean_expression(),
            Token::BANG | Token::MINUS => self.parse_prefix_operator_expression(),
            Token::LPAREN => self.parse_grouped_expression(),
            Token::IF => self.parse_if_expression(),
            Token::FUNCTION => self.parse_function_expression(),
            Token::STRING(_) => self.parse_string_literal_expression(),
            _ => None,
        }
    }

    fn can_parse_infix_expression(&self, token: &Token) -> bool {
        match token {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK
            | Token::EQ
            | Token::NotEq
            | Token::LT
            | Token::GT
            | Token::LPAREN => true,
            _ => false,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        match *self.cur_token {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK
            | Token::EQ
            | Token::NotEq
            | Token::LT
            | Token::GT => self.parse_infix_operator_expression(left),
            Token::LPAREN => self.parse_call_expression(left),
            _ => None,
        }
    }

    fn parse_infix_operator_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = String::from(token.literal());
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Some(Expression::InfixExpression(InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_call_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let arguments = self.parse_call_arguments()?;
        Some(Expression::CallExpression(CallExpression {
            token,
            function: Box::new(left),
            arguments,
        }))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut arguments = Vec::new();
        if *self.peek_token == Token::RPAREN {
            self.next_token();
            return Some(arguments);
        }

        self.next_token();

        arguments.push(self.parse_expression(Precedence::LOWEST)?);
        while *self.peek_token == Token::COMMA {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(Precedence::LOWEST)?);
        }

        if !self.expect_peek(Token::RPAREN) {
            return None;
        }

        Some(arguments)
    }

    fn parse_identifier_expression(&mut self) -> Identifier {
        let token = self.cur_token.clone();
        Identifier { token }
    }

    fn parse_integer_literal_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        let value = self.cur_token.literal().parse::<i64>();
        if value.is_err() {
            self.errors
                .push(format!("could not parse {} as integer", token.literal()));
            return None;
        }

        Some(Expression::IntegerLiteral(IntegerLiteral {
            token,
            value: value.unwrap(),
        }))
    }

    fn parse_boolean_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        let value = self.cur_token.literal().parse::<bool>();
        if value.is_err() {
            self.errors
                .push(format!("could not parse {} as boolean", token.literal()));
            return None;
        }

        Some(Expression::Boolean(Boolean {
            token,
            value: value.unwrap(),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST)?;
        self.expect_peek(Token::RPAREN);
        Some(exp)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        if !self.expect_peek(Token::LPAREN) {
            return None;
        }
        self.next_token();

        let condition = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(Token::RPAREN) {
            return None;
        }

        if !self.expect_peek(Token::LBRACE) {
            return None;
        }

        let consequence = self.parse_block();

        let mut alternative: Option<BlockStatement> = None;
        if *self.peek_token == Token::ELSE {
            self.next_token();

            if !self.expect_peek(Token::LBRACE) {
                return None;
            }

            alternative = Some(self.parse_block());
        }

        Some(Expression::IfExpression(IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        if !self.expect_peek(Token::LPAREN) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(Token::LBRACE) {
            return None;
        }

        let body = self.parse_block();

        Some(Expression::FunctionLiteral(FunctionLiteral {
            token,
            body,
            parameters,
        }))
    }

    fn parse_string_literal_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let value = token.literal().to_string();
        Some(Expression::StringLiteral(StringLiteral { token, value }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = Vec::new();
        if *self.peek_token == Token::RPAREN {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        identifiers.push(self.parse_identifier_expression());
        while *self.peek_token == Token::COMMA {
            self.next_token();
            self.next_token();
            identifiers.push(self.parse_identifier_expression());
        }

        if !self.expect_peek(Token::RPAREN) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_block(&mut self) -> BlockStatement {
        let token = self.cur_token.clone();
        self.next_token();

        let mut statements = Vec::new();
        while *self.cur_token != Token::RBRACE && *self.cur_token != Token::EOF {
            let s = self.parse_statement();
            if let Some(s) = s {
                statements.push(s);
            }
            self.next_token();
        }

        BlockStatement { token, statements }
    }

    fn parse_prefix_operator_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = token.literal().to_string();

        self.next_token();

        let right_expr = self.parse_expression(Precedence::PREFIX)?;

        Some(Expression::PrefixExpression(PrefixExpression {
            token,
            operator,
            right: Box::new(right_expr),
        }))
    }

    fn expect_ident(&mut self) -> bool {
        if let Token::IDENT(_) = *self.peek_token {
            self.next_token();
            return true;
        }
        self.peek_error(Token::IDENT(String::from("")));
        return false;
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if t == *self.peek_token {
            self.next_token();
            return true;
        }
        self.peek_error(t);
        return false;
    }

    fn peek_error(&mut self, t: Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?}",
            t, self.peek_token
        ));
    }

    fn peek_precedence(&self) -> Precedence {
        precedence(&*self.peek_token)
    }

    fn cur_precedence(&self) -> Precedence {
        precedence(&*self.cur_token)
    }
}

fn precedence(t: &Token) -> Precedence {
    match t {
        Token::EQ => Precedence::EQUALS,
        Token::NotEq => Precedence::EQUALS,
        Token::LT => Precedence::LESSGREATER,
        Token::GT => Precedence::LESSGREATER,
        Token::PLUS => Precedence::SUM,
        Token::MINUS => Precedence::SUM,
        Token::SLASH => Precedence::PRODUCT,
        Token::ASTERISK => Precedence::PRODUCT,
        Token::LPAREN => Precedence::CALL,
        _ => Precedence::LOWEST,
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        ast::{Expression, Identifier, LetStatement, Program, Statement},
        lexer::Lexer,
        token::Token,
    };

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 3);

        test_let_statement(&program.statements[0], "x");
        test_let_statement(&program.statements[1], "y");
        test_let_statement(&program.statements[2], "foobar");
    }

    fn check_parser_errors(p: &Parser) {
        if p.errors.len() == 0 {
            return;
        }

        eprintln!("parser errors: {:#?}", p.errors);
        assert!(false);
    }

    fn test_let_statement(x: &Statement, expected_ident: &str) {
        match x {
            Statement::LetStatement(letstm) => {
                assert_eq!(letstm.name.token.literal(), expected_ident);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 838383;
";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 3);

        test_return_statement(&program.statements[0]);
        test_return_statement(&program.statements[1]);
        test_return_statement(&program.statements[2]);
    }

    fn test_return_statement(x: &Statement) {
        match x {
            Statement::ReturnStatement(stm) => {
                assert_eq!(*stm.token, Token::RETURN);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                token: Rc::new(Token::LET),
                name: Identifier {
                    token: Rc::new(Token::IDENT("myVar".to_string())),
                },
                value: Expression::Identifier(Identifier {
                    token: Rc::new(Token::IDENT("anotherVar".to_string())),
                }),
            })],
        };
        let expected = "let myVar = anotherVar;\n";

        assert_eq!(program.to_string(), expected);
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        let stm = &program.statements[0];
        match stm {
            Statement::ExpressionStatement(stm) => {
                check_identifier_literal(&stm.expression, "foobar");
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        let stm = &program.statements[0];
        match stm {
            Statement::ExpressionStatement(stm) => {
                check_integer_literal(&stm.expression, 5);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_prefix_expression() {
        let tests = [
            ("!5;", "!", Lit::Int(5)),
            ("-15;", "-", Lit::Int(15)),
            ("!true;", "!", Lit::Bool(true)),
            ("!false;", "!", Lit::Bool(false)),
        ];

        for (input, operator, value) in tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);

            let stm = &program.statements[0];
            let exp = match stm {
                Statement::ExpressionStatement(stm) => stm,
                _ => panic!("not expression statement"),
            };
            match &exp.expression {
                Expression::PrefixExpression(prefix_exp) => {
                    assert_eq!(prefix_exp.operator, operator);
                    check_literal_expression(&prefix_exp.right, value);
                }
                _ => assert!(false),
            }
        }
    }

    fn check_integer_literal(exp: &Expression, value: i64) {
        match exp {
            Expression::IntegerLiteral(integer) => assert_eq!(integer.value, value),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests = [
            ("5 + 5;", Lit::Int(5), "+", Lit::Int(5)),
            ("5 - 5;", Lit::Int(5), "-", Lit::Int(5)),
            ("5 * 5;", Lit::Int(5), "*", Lit::Int(5)),
            ("5 / 5;", Lit::Int(5), "/", Lit::Int(5)),
            ("5 > 5;", Lit::Int(5), ">", Lit::Int(5)),
            ("5 < 5;", Lit::Int(5), "<", Lit::Int(5)),
            ("5 == 5;", Lit::Int(5), "==", Lit::Int(5)),
            ("5 != 5;", Lit::Int(5), "!=", Lit::Int(5)),
            ("alice * bob;", Lit::Ident("alice"), "*", Lit::Ident("bob")),
            ("true == true", Lit::Bool(true), "==", Lit::Bool(true)),
            ("true != false", Lit::Bool(true), "!=", Lit::Bool(false)),
            ("false == false", Lit::Bool(false), "==", Lit::Bool(false)),
        ];

        for (input, left, operator, right) in tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);

            let stm = match &program.statements[0] {
                Statement::ExpressionStatement(stm) => stm,
                _ => panic!("not expression statement"),
            };

            check_infix_expression(&stm.expression, left, operator, right);
        }
    }

    fn check_identifier_literal(exp: &Expression, value: &str) {
        match exp {
            Expression::Identifier(ident) => assert_eq!(ident.token.literal(), value),
            _ => assert!(false),
        }
    }

    #[derive(Clone, Copy)]
    enum Lit<'a> {
        Int(i64),
        Ident(&'a str),
        Bool(bool),
        String(&'a str),
    }

    fn check_literal_expression(exp: &Expression, expected_value: Lit) {
        match expected_value {
            Lit::Int(value) => check_integer_literal(exp, value),
            Lit::Ident(value) => check_identifier_literal(exp, value),
            Lit::Bool(value) => check_boolean_literal(exp, value),
            Lit::String(s) => match exp {
                Expression::StringLiteral(string) => assert_eq!(string.value, s),
                _ => assert!(false),
            },
        }
    }

    fn check_infix_expression(exp: &Expression, left: Lit, operator: &str, right: Lit) {
        match exp {
            Expression::InfixExpression(infix_exp) => {
                assert_eq!(infix_exp.operator, operator);
                check_literal_expression(&*infix_exp.left, left);
                check_literal_expression(&*infix_exp.right, right);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)\n((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            ("\"foo\" + \"bar\"", "(\"foo\" + \"bar\")"),
        ];

        for (input, expected) in tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.to_string(), String::from(expected) + "\n");
        }
    }

    #[test]
    fn test_boolean_literal_expression() {
        let input = "true;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        let stm = match &program.statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };
        check_boolean_literal(&stm.expression, true);
    }

    #[test]
    fn test_string_literal_expression() {
        let inputs = ["hello world", "foobar", ""];

        for input in inputs {
            let l = Lexer::new(format!("\"{}\"", input).as_str());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);

            let stm = match &program.statements[0] {
                Statement::ExpressionStatement(stm) => stm,
                _ => panic!("not expression statement"),
            };
            check_literal_expression(&stm.expression, Lit::String(input));
        }
    }

    fn check_boolean_literal(exp: &Expression, value: bool) {
        match exp {
            Expression::Boolean(boolean) => assert_eq!(boolean.value, value),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        let stm = match &program.statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };

        let exp = match &stm.expression {
            Expression::IfExpression(exp) => exp,
            _ => panic!("not expression statement"),
        };

        check_infix_expression(&exp.condition, Lit::Ident("x"), "<", Lit::Ident("y"));

        assert_eq!(exp.consequence.statements.len(), 1);
        let consequence_stm = match &exp.consequence.statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };
        check_identifier_literal(&consequence_stm.expression, "x");

        assert!(exp.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        let stm = match &program.statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };

        let exp = match &stm.expression {
            Expression::IfExpression(exp) => exp,
            _ => panic!("not expression statement"),
        };

        check_infix_expression(&exp.condition, Lit::Ident("x"), "<", Lit::Ident("y"));

        assert_eq!(exp.consequence.statements.len(), 1);
        let consequence_stm = match &exp.consequence.statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };
        check_identifier_literal(&consequence_stm.expression, "x");

        assert!(exp.alternative.is_some());
        let alt = exp.alternative.as_ref().unwrap();
        assert_eq!(alt.statements.len(), 1);
        let alternative_stm = match &exp.alternative.as_ref().unwrap().statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };
        check_identifier_literal(&alternative_stm.expression, "y");
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        let stm = match &program.statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };

        let exp = match &stm.expression {
            Expression::FunctionLiteral(exp) => exp,
            _ => panic!("not function literal"),
        };

        check_identifier_literal(&Expression::Identifier(exp.parameters[0].clone()), "x");
        check_identifier_literal(&Expression::Identifier(exp.parameters[1].clone()), "y");

        assert_eq!(exp.body.statements.len(), 1);
        let body_stm = match &exp.body.statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };

        check_infix_expression(&body_stm.expression, Lit::Ident("x"), "+", Lit::Ident("y"));
    }

    #[test]
    fn test_function_literal_params() {
        let input = [
            ("fn() {}", vec![]),
            ("fn(x) {}", vec!["x"]),
            ("fn(x, y) {}", vec!["x", "y"]),
        ];

        for (input, expected_params) in input {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);

            let stm = match &program.statements[0] {
                Statement::ExpressionStatement(stm) => stm,
                _ => panic!("not expression statement"),
            };

            let exp = match &stm.expression {
                Expression::FunctionLiteral(exp) => exp,
                _ => panic!("not function literal"),
            };

            assert_eq!(exp.parameters.len(), expected_params.len());
            for (i, ident) in exp.parameters.iter().enumerate() {
                check_identifier_literal(
                    &Expression::Identifier(ident.clone()),
                    expected_params[i],
                );
            }

            assert_eq!(exp.body.statements.len(), 0);
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(2, 4)";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        let stm = match &program.statements[0] {
            Statement::ExpressionStatement(stm) => stm,
            _ => panic!("not expression statement"),
        };

        let exp = match &stm.expression {
            Expression::CallExpression(exp) => exp,
            _ => panic!("not call expression"),
        };

        check_identifier_literal(&*exp.function, "add");
        assert_eq!(exp.arguments.len(), 2);
        check_integer_literal(&exp.arguments[0], 2);
        check_integer_literal(&exp.arguments[1], 4);
    }

    #[test]
    fn test_call_expression_params() {
        let input = [
            ("add()", vec![]),
            ("add(1)", vec![Lit::Int(1)]),
            ("add(1,2)", vec![Lit::Int(1), Lit::Int(2)]),
        ];

        for (input, expected_params) in input {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);

            let stm = match &program.statements[0] {
                Statement::ExpressionStatement(stm) => stm,
                _ => panic!("not expression statement"),
            };

            let exp = match &stm.expression {
                Expression::CallExpression(exp) => exp,
                _ => panic!("not call expression"),
            };

            assert_eq!(exp.arguments.len(), expected_params.len());
            for (i, ident) in exp.arguments.iter().enumerate() {
                check_literal_expression(&ident, expected_params[i]);
            }
        }
    }
}
