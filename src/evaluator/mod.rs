use crate::{
    ast::{
        CallExpression, Expression, FunctionLiteral, Identifier, IfExpression, InfixExpression,
        Node, PrefixExpression, Statement,
    },
    object::{
        environ::Environment,
        object::{obj_type, Boolean, Error, Function, Integer, Null, Object, ReturnValue},
    },
};

pub fn eval(node: &Node, environ: &mut Environment) -> Object {
    match node {
        Node::Program(x) => {
            let mut res = Object::Null(Null {});
            for stm in x.statements.iter() {
                res = eval_statement(stm, environ);
                match res {
                    Object::Return(ret) => return *ret.value,
                    Object::Error(_) => return res,
                    _ => (),
                };
            }
            res
        }
        Node::Statement(x) => eval_statement(x, environ),
        Node::Expression(x) => eval_expression(x, environ),
    }
}

pub fn eval_expression(exp: &Expression, environ: &mut Environment) -> Object {
    match exp {
        Expression::IntegerLiteral(x) => Object::Integer(Integer { value: x.value }),
        Expression::Boolean(x) => Object::Boolean(Boolean { value: x.value }),
        Expression::PrefixExpression(x) => eval_prefix_expression(x, environ),
        Expression::InfixExpression(x) => eval_infix_expression(x, environ),
        Expression::IfExpression(x) => eval_if_expression(x, environ),
        Expression::Identifier(x) => eval_identifier(x, environ),
        Expression::FunctionLiteral(x) => eval_function_literal(x, environ),
        Expression::CallExpression(x) => eval_call_expression(x, environ),
    }
}

pub fn eval_prefix_expression(exp: &PrefixExpression, environ: &mut Environment) -> Object {
    match (exp.operator.as_str(), eval_expression(&*exp.right, environ)) {
        (_, right @ Object::Error(_)) => right,
        ("!", Object::Integer(Integer { value: 0 })) => Object::Boolean(Boolean { value: true }),
        ("!", Object::Integer(Integer { value: _ })) => Object::Boolean(Boolean { value: false }),
        ("!", Object::Boolean(Boolean { value: b })) => Object::Boolean(Boolean { value: !b }),
        ("+", Object::Integer(Integer { value: n })) => Object::Integer(Integer { value: n }),
        ("-", Object::Integer(Integer { value: n })) => Object::Integer(Integer { value: -n }),
        (op, val) => Object::Error(Error {
            message: format!("unknown operator: {}{}", op, obj_type(&val)),
        }),
    }
}

pub fn eval_infix_expression(exp: &InfixExpression, environ: &mut Environment) -> Object {
    match (
        exp.operator.as_str(),
        eval_expression(&*exp.left, environ),
        eval_expression(&*exp.right, environ),
    ) {
        (_, left @ Object::Error(_), _) => left,
        (_, _, right @ Object::Error(_)) => right,
        ("+", Object::Integer(Integer { value: l }), Object::Integer(Integer { value: r })) => {
            Object::Integer(Integer { value: l + r })
        }
        ("-", Object::Integer(Integer { value: l }), Object::Integer(Integer { value: r })) => {
            Object::Integer(Integer { value: l - r })
        }
        ("*", Object::Integer(Integer { value: l }), Object::Integer(Integer { value: r })) => {
            Object::Integer(Integer { value: l * r })
        }
        ("/", Object::Integer(Integer { value: l }), Object::Integer(Integer { value: r })) => {
            Object::Integer(Integer { value: l / r })
        }
        ("<", Object::Integer(Integer { value: l }), Object::Integer(Integer { value: r })) => {
            Object::Boolean(Boolean { value: l < r })
        }
        (">", Object::Integer(Integer { value: l }), Object::Integer(Integer { value: r })) => {
            Object::Boolean(Boolean { value: l > r })
        }
        ("==", Object::Integer(Integer { value: l }), Object::Integer(Integer { value: r })) => {
            Object::Boolean(Boolean { value: l == r })
        }
        ("!=", Object::Integer(Integer { value: l }), Object::Integer(Integer { value: r })) => {
            Object::Boolean(Boolean { value: l != r })
        }
        ("==", Object::Boolean(Boolean { value: l }), Object::Boolean(Boolean { value: r })) => {
            Object::Boolean(Boolean { value: l == r })
        }
        ("!=", Object::Boolean(Boolean { value: l }), Object::Boolean(Boolean { value: r })) => {
            Object::Boolean(Boolean { value: l != r })
        }
        ("==", _, _) => Object::Boolean(Boolean { value: false }),
        ("!=", _, _) => Object::Boolean(Boolean { value: true }),
        (_, l, r) => Object::Error(Error {
            message: format!(
                "unknown operator: {} {} {}",
                obj_type(&l),
                exp.operator,
                obj_type(&r)
            ),
        }),
    }
}

pub fn eval_if_expression(exp: &IfExpression, environ: &mut Environment) -> Object {
    match eval_expression(&exp.condition, environ) {
        err @ Object::Error(_) => err,
        Object::Boolean(Boolean { value: false }) | Object::Null(_) => {
            if let Some(alternative) = &exp.alternative {
                eval_statement(&Statement::BlockStatement(alternative.clone()), environ)
            } else {
                Object::Null(Null {})
            }
        }
        _ => eval_statement(&Statement::BlockStatement(exp.consequence.clone()), environ),
    }
}

pub fn eval_identifier(exp: &Identifier, environ: &mut Environment) -> Object {
    match environ.get(&exp.token.literal()) {
        Some(val) => val.clone(),
        None => Object::Error(Error {
            message: format!("identifier not found: {}", exp.token.literal()),
        }),
    }
}

pub fn eval_function_literal(exp: &FunctionLiteral, environ: &mut Environment) -> Object {
    Object::Function(Function {
        parameters: exp.parameters.clone(),
        body: exp.body.clone(),
        env: environ.clone(),
    })
}

pub fn eval_call_expression(exp: &CallExpression, environ: &mut Environment) -> Object {
    let f = eval_expression(&*exp.function, environ);
    match f {
        Object::Error(_) => return f,
        Object::Function(f) => {
            if f.parameters.len() != exp.arguments.len() {
                return Object::Error(Error {
                    message: format!(
                        "wrong number of arguments: want={}, got={}",
                        f.parameters.len(),
                        exp.arguments.len()
                    ),
                });
            }

            let args = eval_expressions(&exp.arguments, environ);
            match args {
                Err(err) => return err,
                Ok(args) => {
                    let mut local_env = Environment::new_enclosed_environment(f.env.clone());
                    for (i, param) in f.parameters.iter().enumerate() {
                        local_env.set(param.token.literal().to_string(), args[i].clone());
                    }

                    eval_statement(&Statement::BlockStatement(f.body), &mut local_env)
                }
            }
        }
        _ => Object::Error(Error {
            message: format!("not a function: {}", obj_type(&f)),
        }),
    }
}

pub fn eval_expressions(
    exps: &[Expression],
    environ: &mut Environment,
) -> Result<Vec<Object>, Object> {
    let mut res = vec![];
    for exp in exps.iter() {
        let val = eval_expression(exp, environ);
        match val {
            Object::Error(_) => return Err(val),
            _ => (),
        };
        res.push(val);
    }
    Ok(res)
}

pub fn eval_statement(stmt: &Statement, environ: &mut Environment) -> Object {
    match stmt {
        Statement::ExpressionStatement(x) => eval_expression(&x.expression, environ),
        Statement::BlockStatement(x) => {
            let mut res = Object::Null(Null {});
            for stm in x.statements.iter() {
                res = eval_statement(stm, environ);
                match res {
                    Object::Return(_) => return res,
                    Object::Error(_) => return res,
                    _ => (),
                };
            }
            res
        }
        Statement::ReturnStatement(x) => {
            if let Some(return_value) = &x.return_value {
                match eval_expression(return_value, environ) {
                    err @ Object::Error(_) => err,
                    val => Object::Return(ReturnValue {
                        value: Box::new(val),
                    }),
                }
            } else {
                Object::Null(Null {})
            }
        }
        Statement::LetStatement(stm) => {
            let val = eval_expression(&stm.value, environ);
            if let Object::Error(_) = val {
                return val;
            }

            environ.set(stm.name.token.literal().to_string(), val);

            Object::Null(Null {})
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_eval_integer_expression() {
        let inputs = [("5", 5)];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_integer_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let inputs = [("true", true), ("false", false)];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_boolean_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_eval_int_prefix_operator() {
        let inputs = [("-5", -5), ("--5", 5), ("+3", 3)];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_integer_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_eval_bool_result() {
        let inputs = [
            ("!true", false),
            ("!false", true),
            ("!!true", true),
            ("!0", true),
            ("!5", false),
            ("!(1+3)", false),
            ("5 > 4", true),
            ("5 < 4", false),
            ("5 == 5", true),
            ("5 != 5", false),
            ("15 != 5", true),
        ];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_boolean_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_eval_int_result() {
        let inputs = [
            ("1+2", 3),
            ("1+2+3", 6),
            ("1+2-3-1", -1),
            ("if (5 < 4) { 10 } else { 0 }", 0),
            ("if (5 > 4) { 10 } else { 0 }", 10),
            ("if (5) { 10 } else { 0 }", 10),
        ];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_integer_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_eval_null_result() {
        let inputs = ["if (5 < 0) { 10 }"];
        for input in inputs {
            let evaluated = test_eval(input);
            check_null_object(&evaluated);
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
if (10 > 1) {
    if (10 > 1) {
        return 10;
    }

    return 1;
}
            ";
        let evaluated = test_eval(input);
        check_integer_object(&evaluated, 10, input);
    }

    #[test]
    fn test_error_handling() {
        let inputs = [
            ("5 + true;", "unknown operator: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "unknown operator: INTEGER + BOOLEAN"),
            ("-true;", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "
             if (10 > 1) {
                 if (10 > 1) {
                     return true + false;
                 }
                 return 1;
             }
             ",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_error_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_let_statement() {
        let inputs = [
            ("let a = 5; a;", 5),
            ("let a = 5; let b = a; b;", 5),
            (
                "
            let a = 5;
            let b = 10;
            a + b;
            ",
                15,
            ),
        ];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_integer_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);
        match evaluated {
            Object::Function(f) => {
                assert_eq!(f.parameters.len(), 1);
                assert_eq!(f.parameters[0].to_string(), "x");
                assert_eq!(f.body.to_string(), "(x + 2)\n");
            }
            _ => assert!(false, "object is not a function, input was: {}", input),
        }
    }

    #[test]
    fn test_function_application() {
        let inputs = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
            (
                "
            let newAdder = fn(x) { fn(y) { x + y }; };
            let addTwo = newAdder(2);
            addTwo(2);
             ",
                4,
            ),
        ];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_integer_object(&evaluated, expected, input);
        }
    }

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let node = Parser::new(l).parse_program();
        eval(&Node::Program(node), &mut Environment::new())
    }

    fn check_boolean_object(obj: &Object, expected: bool, input: &str) {
        match obj {
            Object::Boolean(b) => assert_eq!(b.value, expected),
            _ => assert!(
                false,
                "object is not a bool, input was: {}, result was {:?}",
                input, obj
            ),
        }
    }

    fn check_integer_object(obj: &Object, expected: i64, input: &str) {
        match obj {
            Object::Integer(i) => assert_eq!(i.value, expected, "input was: {}", input),
            _ => assert!(false, "object is not an integer, input was: {}", input),
        }
    }

    fn check_null_object(obj: &Object) {
        match obj {
            Object::Null(_) => (),
            _ => panic!("object is not null"),
        }
    }

    fn check_error_object(obj: &Object, expected: &str, input: &str) {
        match obj {
            Object::Error(err) => assert_eq!(err.message, expected, "input was: {}", input),
            _ => assert!(false, "object is not error, input was: {}", input),
        }
    }
}
