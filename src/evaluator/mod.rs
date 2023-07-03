use crate::{
    ast::{
        ArrayLiteral, CallExpression, Expression, FunctionLiteral, HashLiteral, Identifier,
        IfExpression, IndexExpression, InfixExpression, Node, PrefixExpression, Statement,
    },
    map::InefficientMap,
    object::{
        environ::Environment,
        object::{Err, Function, Object},
    },
};

pub fn eval(node: &Node, environ: &mut Environment) -> Object {
    match node {
        Node::Program(x) => {
            let mut res = Object::Null;
            for stm in x.statements.iter() {
                res = eval_statement(stm, environ);
                match res {
                    Object::Return(ret) => return *ret,
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
    use Expression::*;
    match exp {
        IntegerLiteral(x) => Object::Integer(x.value),
        Boolean(x) => Object::Boolean(x.value),
        StringLiteral(s) => Object::String(s.value.clone()),
        PrefixExpression(x) => eval_prefix_expression(x, environ),
        InfixExpression(x) => eval_infix_expression(x, environ),
        IfExpression(x) => eval_if_expression(x, environ),
        Identifier(x) => eval_identifier(x, environ),
        FunctionLiteral(x) => eval_function_literal(x, environ),
        CallExpression(x) => eval_call_expression(x, environ),
        ArrayLiteral(x) => eval_array_literal(x, environ),
        IndexExpression(x) => eval_index_expression(x, environ),
        HashLiteral(x) => eval_hash_literal(x, environ),
    }
}

pub fn eval_prefix_expression(exp: &PrefixExpression, environ: &mut Environment) -> Object {
    use Object::*;
    match (exp.operator.as_str(), eval_expression(&*exp.right, environ)) {
        (_, right @ Error(_)) => right,
        ("!", Integer(0)) => Boolean(true),
        ("!", Integer(_)) => Boolean(false),
        ("!", Boolean(b)) => Boolean(!b),
        ("+", Integer(n)) => Integer(n),
        ("-", Integer(n)) => Integer(-n),
        (op, val) => Error(Err::UnknownPrefixOperator(op.to_string(), Box::new(val))),
    }
}

pub fn eval_infix_expression(exp: &InfixExpression, environ: &mut Environment) -> Object {
    use Object::*;
    match (
        exp.operator.as_str(),
        eval_expression(&*exp.left, environ),
        eval_expression(&*exp.right, environ),
    ) {
        (_, left @ Error(_), _) => left,
        (_, _, right @ Error(_)) => right,
        ("+", Integer(l), Integer(r)) => Integer(l + r),
        ("+", String(l), String(r)) => String(l + &r),
        ("-", Integer(l), Integer(r)) => Integer(l - r),
        ("*", Integer(l), Integer(r)) => Integer(l * r),
        ("/", Integer(l), Integer(r)) => Integer(l / r),
        ("<", Integer(l), Integer(r)) => Boolean(l < r),
        (">", Integer(l), Integer(r)) => Boolean(l > r),
        ("==", Integer(l), Integer(r)) => Boolean(l == r),
        ("!=", Integer(l), Integer(r)) => Boolean(l != r),
        ("==", Boolean(l), Boolean(r)) => Boolean(l == r),
        ("!=", Boolean(l), Boolean(r)) => Boolean(l != r),
        ("==", _, _) => Boolean(false),
        ("!=", _, _) => Boolean(true),
        (op, l, r) => Error(Err::UnknownInfixOperator(
            op.to_string(),
            Box::new(l),
            Box::new(r),
        )),
    }
}

pub fn eval_if_expression(exp: &IfExpression, environ: &mut Environment) -> Object {
    match eval_expression(&exp.condition, environ) {
        err @ Object::Error(_) => err,
        Object::Boolean(false) | Object::Null => {
            if let Some(alternative) = &exp.alternative {
                eval_statement(&Statement::BlockStatement(alternative.clone()), environ)
            } else {
                Object::Null
            }
        }
        _ => eval_statement(&Statement::BlockStatement(exp.consequence.clone()), environ),
    }
}

pub fn eval_identifier(exp: &Identifier, environ: &mut Environment) -> Object {
    match environ.get(&exp.token.literal()) {
        Some(val) => val.clone(),
        None => Object::Error(Err::IdentifierNotFound(exp.clone())),
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
                return Object::Error(Err::WrongArgumentsCount(
                    f.parameters.len(),
                    exp.arguments.len(),
                ));
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
        Object::Builtin(b) => {
            let args = eval_expressions(&exp.arguments, environ);
            match args {
                Err(err) => return err,
                Ok(args) => b(args),
            }
        }
        _ => Object::Error(Err::NotAFunction(Box::new(f))),
    }
}

pub fn eval_array_literal(exp: &ArrayLiteral, environ: &mut Environment) -> Object {
    let elements = eval_expressions(&exp.elements, environ);
    match elements {
        Err(err) => err,
        Ok(elements) => Object::Array(elements),
    }
}

pub fn eval_index_expression(exp: &IndexExpression, environ: &mut Environment) -> Object {
    let left = eval_expression(&*exp.left, environ);
    let index = eval_expression(&*exp.index, environ);
    match left {
        Object::Array(a) => match index {
            Object::Integer(i) => {
                if i < 0 || i >= a.len() as i64 {
                    return Object::Null;
                }
                a[i as usize].clone()
            }
            _ => Object::Error(Err::IndexNotInteger(Box::new(index))),
        },
        Object::HashMap(m) => m.get(&index).unwrap_or(&Object::Null).clone(),
        _ => Object::Error(Err::UnsupportedIndexOperator(Box::new(left))),
    }
}

pub fn eval_hash_literal(exp: &HashLiteral, environ: &mut Environment) -> Object {
    let mut m = InefficientMap::new();
    exp.pairs
        .iter()
        .map(|(k, v)| (eval_expression(k, environ), eval_expression(v, environ)))
        .for_each(|(k, v)| match k {
            Object::Error(_) => (),
            _ => {
                m.insert(k, v);
            }
        });

    Object::HashMap(m)
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
            let mut res = Object::Null;
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
                    val => Object::Return(Box::new(val)),
                }
            } else {
                Object::Null
            }
        }
        Statement::LetStatement(stm) => {
            let val = eval_expression(&stm.value, environ);
            if let Object::Error(_) = val {
                return val;
            }

            environ.set(stm.name.token.literal().to_string(), val);

            Object::Null
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

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
            ("len(\"\")", 0),
            ("len(\"foo\")", 3),
            ("first([1,2,3])", 1),
            ("last([1,2,3])", 3),
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
    fn test_eval_string_result() {
        let inputs = [("\"foobar\"", "foobar"), ("\"foo\" + \"bar\"", "foobar")];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_string_object(&evaluated, expected, input);
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

    #[test]
    fn test_closures() {
        let input = "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);
            ";
        let evaluated = test_eval(input);
        check_integer_object(&evaluated, 4, input);
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";
        let evaluated = test_eval(input);
        check_string_object(&evaluated, "Hello World!", input);
    }

    #[test]
    fn test_builtin_functions() {
        let inputs = [
            ("len(\"\")", 0),
            ("len(\"four\")", 4),
            ("len(\"hello world\")", 11),
        ];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_integer_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);
        check_array_object(&evaluated, vec![1, 4, 6], input);
    }

    #[test]
    fn test_array_index_expression() {
        let input = "[1, 2 * 2, 3 + 3][1]";
        let evaluated = test_eval(input);
        check_integer_object(&evaluated, 4, input);
    }

    #[test]
    fn test_eval_array_result() {
        let inputs = [
            ("rest([1,2,3])", vec![2, 3]),
            ("push([1,2,3], 4)", vec![1, 2, 3, 4]),
        ];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_array_object(&evaluated, expected, input);
        }
    }

    #[test]
    fn test_eval_hashmap_result() {
        let inputs = [
            ("{}", HashMap::new()),
            ("{\"one\": 1}", HashMap::from([("one".to_string(), 1)])),
            (
                "{\"one\": 1, \"two\": 2}",
                HashMap::from([("one".to_string(), 1), ("two".to_string(), 2)]),
            ),
        ];
        for (input, expected) in inputs {
            let evaluated = test_eval(input);
            check_hashmap_object(&evaluated, expected, input);
        }
    }

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let node = Parser::new(l).parse_program();
        eval(&Node::Program(node), &mut Environment::new())
    }

    fn check_boolean_object(obj: &Object, expected: bool, input: &str) {
        match obj {
            Object::Boolean(b) => assert_eq!(b, &expected),
            _ => assert!(
                false,
                "object is not a bool, input was: {}, result was {:?}",
                input, obj
            ),
        }
    }

    fn check_integer_object(obj: &Object, expected: i64, input: &str) {
        match obj {
            Object::Integer(i) => assert_eq!(i, &expected, "input was: {}", input),
            _ => assert!(false, "object is not an integer, input was: {}", input),
        }
    }

    fn check_null_object(obj: &Object) {
        match obj {
            Object::Null => (),
            _ => panic!("object is not null"),
        }
    }

    fn check_string_object(obj: &Object, expected: &str, input: &str) {
        match obj {
            Object::String(s) => {
                assert_eq!(s, &expected, "input was: {}, result was: {:?}", input, obj)
            }
            _ => assert!(
                false,
                "object is not an integer, input was: {}, result was {:?}",
                input, obj
            ),
        }
    }

    fn check_error_object(obj: &Object, expected: &str, input: &str) {
        match obj {
            Object::Error(err) => assert_eq!(err.to_string(), expected, "input was: {}", input),
            _ => assert!(false, "object is not error, input was: {}", input),
        }
    }

    fn check_array_object(obj: &Object, expected: Vec<i64>, input: &str) {
        match obj {
            Object::Array(arr) => {
                assert_eq!(arr.len(), expected.len());
                for (i, el) in arr.iter().enumerate() {
                    check_integer_object(el, expected[i], input);
                }
            }
            _ => assert!(false, "object is not an array, input was: {}", input),
        }
    }

    fn check_hashmap_object(obj: &Object, expected: HashMap<String, i64>, input: &str) {
        match obj {
            Object::HashMap(map) => {
                assert_eq!(map.len(), expected.len());
                for (k, expected_v) in expected.iter() {
                    let v = map.get(&Object::String(k.to_string())).unwrap();
                    check_integer_object(v, *expected_v, input);
                }
            }
            _ => assert!(false, "object is not an array, input was: {}", input),
        }
    }
}
