#[allow(dead_code, unused, unused_variables, unused_imports)]
#[cfg(test)]
pub mod tests {
    use crate::interpret::{ActivationRecord, Interpreter};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticAnalyzer;
    use crate::token::{Token, TokenType};
    use crate::{interpret, rc, set_token};
    use num_traits::Bounded;
    use std::cell::RefCell;
    use std::fs::File;
    use std::io::Read;
    use std::ops::Deref;
    use std::rc::Rc;

    #[test]
    fn test_stack() {
        let str = "PROGRAM Test;
VAR
    a : integer;
BEGIN 
    a := 125.0+12+15/12.5;
END."
            .to_string();
        let tokens = Lexer::new(str).get_tokens();
        let parse = Parser::new(&tokens).parser();
        let e = SemanticAnalyzer::new()._visit(rc!(parse.clone()));

        let mut interpret = Interpreter::new()
            .set_symbol_table(e)
            .set_parser(rc!(parse));

        interpret.interpret();

        let d = interpret.call_stack.recodes_debug.get(0).unwrap().clone();
        let m = d.borrow().members.clone();

        assert_eq!(138, m.get("a").unwrap().get_integer_const());
    }

    #[test]
    fn test_parser() {
        let case: Vec<(_, i64)> = vec![
            ("33", 33),
            ("2 + 7 * 4", 30),
            ("7 - 8 DIV 4", 5),
            ("14 + 2 * 3 - 6 DIV 2", 17),
            ("7 + 3 * (10 DIV (12 DIV (3 + 1) - 1))", 22),
            (
                "7 + 3 * (10 DIV (12 DIV (3 + 1) - 1)) DIV (2 + 3) - 5 - 3 + (8)",
                10,
            ),
            ("7 + (((3 + 2)))", 12),
            ("- 3", -3),
            ("+ 3", 3),
            ("5 - - - + - 3", 8),
            ("5 - - - + - (3 + 4) - +2", 10),
        ];
        let interpret = |input: &str, var: i64| {
            let input = format!(
                "
        PROGRAM Test;
        VAR
            a : INTEGER;
        BEGIN
            a := {}
        END.
        ",
                var
            );
            let tokens = Lexer::new(input).get_tokens();
            let parse = Parser::new(&tokens).parser();
            let e = SemanticAnalyzer::new()._visit(rc!(parse.clone()));
            Interpreter::new()
                .set_parser(rc!(parse))
                .set_symbol_table(e)
        };

        case.iter().for_each(|(x, y)| {
            let mut i = interpret(x, *y);
            i.interpret();
            let d = i.call_stack.recodes_debug.get(0).unwrap().clone();
            let m = d.borrow().members.clone();

            assert_eq!(*y, m.get("a").unwrap().get_integer_const());
        });
    }

    #[test]
    fn test_parse2() {
        let case = vec![
            ("3.24", 3.24),
            ("2.14 + 7 * 4", 30.14),
            ("7.14 - 8 / 4", 5.14),
        ];

        let interpret = |s: (&str, f64)| {
            let input = format!(
                "
        PROGRAM Test;
        VAR
            a : REAL;
        BEGIN
            a := {}
        END.
        ",
                s.0
            );
            let tokens = Lexer::new(input).get_tokens();
            let parse = Parser::new(&tokens).parser();
            let e = SemanticAnalyzer::new()._visit(rc!(parse.clone()));
            Interpreter::new()
                .set_parser(rc!(parse))
                .set_symbol_table(e)
        };

        case.iter().for_each(|(x, y)| {
            let mut i = interpret((x, *y));
            i.interpret();
            let d = i.call_stack.recodes_debug.get(0).unwrap().clone();
            let m = d.borrow().members.clone();

            assert_eq!(*y, m.get("a").unwrap().get_real_const());
        });
    }

    const INPUT: &str = r#"
PROGRAM Part12;
VAR
    number : INTEGER;
    a, b   : INTEGER;
    y      : REAL;
    z      : STRING;

PROCEDURE P1;
VAR
    a : REAL;
    k : INTEGER;
    PROCEDURE P2;
    VAR
        a, z : INTEGER;
    BEGIN {P2}
        z := 777;
    END;  {P2}
BEGIN {P1}

END;  {P1}

BEGIN {Part12}
    number := 2;
    a := number ;
    b := 10 * a + 10 * number DIV 4;
    y := 20 / 7 + 3.24;
    z := "hello world";
END.  {Part12}"#;

    #[test]
    fn test_statements() {
        let tokens = Lexer::new(INPUT).get_tokens();
        let parse = Parser::new(&tokens).parser();
        let e = SemanticAnalyzer::new()._visit(rc!(parse.clone()));
        let mut i = Interpreter::new()
            .set_parser(rc!(parse))
            .set_symbol_table(e);

        i.interpret();

        let d = i.call_stack.recodes_debug.get(0).unwrap().clone();
        let m = d.borrow().members.clone();

        let a = m.get("a").unwrap().get_integer_const();
        let b = m.get("b").unwrap().get_integer_const();
        let z = m.get("z").unwrap().get_string();
        let number = m.get("number").unwrap().get_integer_const();
        let y = m.get("y").unwrap().get_real_const();

        assert_eq!(2, number);
        assert_eq!(a, number);
        assert_eq!(10 * a + 10 * number / 4, b);
        assert_eq!(20.0 / 7.0 + 3.24, y);
        assert_eq!("hello world".to_string(), z);
    }

    const INPUT2: &str = "program Main;
    var b, x, y : real;
    var z : integer;

    procedure AlphaA(a : integer);
        var b : integer;

        procedure Beta(c : integer);
            var y : integer;

        procedure Gamma(c : integer; b: real; z: integer);
            var x : integer;
        begin { Gamma }
            x := a + b + c + x + y + z;
        end;  { Gamma }

        begin { Beta }

        end;  { Beta }

    begin { AlphaA }

    end;  { AlphaA }

    procedure AlphaB(a : integer);
        var c : real;
    begin { AlphaB }
        c := a + b;
    end;  { AlphaB }

begin { Main }
end.  { Main }
";
    #[test]
    fn test_input2() {
        let tokens = Lexer::new(INPUT2).get_tokens();
        let k = SemanticAnalyzer::new()._visit(rc!(Parser::new(&tokens).parser()));
        k.iter().for_each(|s| {
            eprintln!("{}\n", s.borrow().to_string());
        });
    }

    #[test]
    #[should_panic]
    fn test_symbol_not_found() {
        let path = "./src/tests/dupiderror.pas";

        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");

            let tokens = Lexer::new(buff).get_tokens();

            SemanticAnalyzer::new()._visit(rc!(Parser::new(&tokens).parser()));
        }
    }

    const UNEXPECTED_TOKEN: &str = "
        PROGRAM Test;
        VAR
            a : REAL {error here}
        BEGIN
            a := 1.5 {TODO missing semi should be panic}
        END.
        ";

    #[test]
    #[should_panic]
    fn test_unexpected_token() {
        let tokens = Lexer::new(UNEXPECTED_TOKEN).get_tokens();
        Parser::new(&tokens).parser();
    }

    const WRONG_PARAMS_NUM: &str = "\
program Main;
procedure Alpha(a : integer; b : integer; c: integer; d: integer);
var x : integer;
begin
    x := (a + b ) * 2;
end;
begin { Main }
    Alpha(3 + 5, 7); {error here}
end.  { Main }
";

    #[test]
    #[should_panic]
    fn test_proccall_error() {
        let tokens = Lexer::new(WRONG_PARAMS_NUM).get_tokens();
        SemanticAnalyzer::new()._visit(rc!(Parser::new(&tokens).parser()));
    }

    #[test]
    fn test_proccall() {
        let path = "./src/tests/test2.pas";
        let test_cases = vec![("a", 8), ("b", 7), ("x", 30)];

        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");

            let tokens = Lexer::new(buff).get_tokens();
            let ast = rc!(Parser::new(&tokens).parser());
            let e = SemanticAnalyzer::new()._visit(ast.clone());
            let mut i = Interpreter::new().set_symbol_table(e).set_parser(ast);
            i._interpret();

            let d = i.call_stack.recodes_debug.get(1).unwrap().clone();
            let m = d.borrow().members.clone();

            test_cases.iter().for_each(|(x, y)| {
                let value = m.get(*x).unwrap().get_integer_const();
                assert_eq!(*y, value, "test_proccall: {} == {}", *y, value);
            });
        } else {
            panic!("file {} can not open.", path);
        }
    }

    #[test]
    fn test_proccall2() {
        let path = "./src/tests/test.pas";
        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");

            let tokens = Lexer::new(buff).get_tokens();
            let ast = Rc::new(RefCell::new(Parser::new(&tokens).parser()));

            let e = SemanticAnalyzer::new()._visit(ast.clone());

            let mut i = Interpreter::new().set_symbol_table(e).set_parser(ast);

            i._interpret();

            let alpha = i.call_stack.recodes_debug.get(1).unwrap().clone();
            let alpha = alpha.borrow().members.clone();

            let a = alpha.get("a").unwrap().get_integer_const();
            let b = alpha.get("b").unwrap().get_integer_const();
            let x = alpha.get("x").unwrap().get_integer_const();
            assert_eq!(8, a);
            assert_eq!(7, b);
            assert_eq!(30, x);

            let beta = i.call_stack.recodes_debug.get(2).unwrap().clone();
            let beta = beta.borrow().members.clone();
            let a = beta.get("a").unwrap().get_integer_const();
            let b = beta.get("b").unwrap().get_integer_const();
            let x = beta.get("x").unwrap().get_integer_const();
            assert_eq!(5, a);
            assert_eq!(10, b);
            assert_eq!(70, x);

            let s = i.call_stack.recodes_debug.get(0).unwrap().clone();
            let s = s.borrow().members.clone().get("s").unwrap().get_string();
            assert_eq!("hello world!".to_string(), s);
        } else {
            panic!("file {} can not open.", path);
        }
    }
}
