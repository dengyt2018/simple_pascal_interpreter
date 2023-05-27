#[allow(dead_code, unused, unused_variables, unused_imports)]
#[cfg(test)]
pub mod tests {
    use crate::interpret::Interpreter;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticAnalyzer;
    use crate::set_token;
    use crate::token::{Token, TokenType};
    use num_traits::Bounded;
    use std::cell::RefCell;
    use std::fs::File;
    use std::io::Read;
    use std::rc::Rc;

    #[test]
    fn test_stack() {
        let str = "PROGRAM Test;
VAR
    a : INTEGER;
BEGIN 
    a := 125+12;
END."
            .to_string();
        let tokens = Lexer::new(str).get_tokens();

        let mut interpreter =
            Interpreter::new().set_parser(Rc::new(RefCell::new(Parser::new(&tokens).parser())));
        interpreter.interpret();
        let mut a = i64::max_value();
        if let Some(ar) = interpreter.call_stack.recodes_debug.get(0) {
            if let Some(var) = ar.borrow().get_item("a") {
                a = *var as i64;
            }
        }
        assert_eq!(137, a);
    }

    #[test]
    fn test_parser() {
        let case = vec![
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
        let interpret = |input: &str, var: i32| {
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
            Interpreter::new().set_parser(Rc::new(RefCell::new(Parser::new(&tokens).parser())))
        };
        case.iter().for_each(|(input, var)| {
            let mut i = interpret(input, *var);
            i.interpret();
            let mut a = i32::max_value();
            if let Some(ar) = i.call_stack.recodes_debug.get(0) {
                if let Some(v) = ar.borrow().get_item("a") {
                    a = *v as i32;
                }
            }
            assert_eq!(a, *var);
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
            Interpreter::new().set_parser(Rc::new(RefCell::new(Parser::new(&tokens).parser())))
        };
        case.iter().for_each(|x| {
            let mut i = interpret((x.0, x.1));
            i.interpret();
            let mut a = 0.0;
            if let Some(ar) = i.call_stack.recodes_debug.get(0) {
                if let Some(v) = ar.borrow().get_item("a") {
                    a = *v;
                }
            }
            assert_eq!(a, x.1);
        });
    }

    const INPUT: &str = "
PROGRAM Part12;
VAR
    number : INTEGER;
    a, b   : INTEGER;
    y      : REAL;

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
    y := 20 / 7 + 3.24
END.  {Part12}";

    #[test]
    fn test_statements() {
        let tokens = Lexer::new(INPUT).get_tokens();
        let mut i =
            Interpreter::new().set_parser(Rc::new(RefCell::new(Parser::new(&tokens).parser())));
        i.interpret();

        if let Some(ar) = i.call_stack.recodes_debug.get(0) {
            let a = *ar.borrow().get_item("a").unwrap();
            let b = *ar.borrow().get_item("b").unwrap();
            let y = *ar.borrow().get_item("y").unwrap();
            let number = *ar.borrow().get_item("number").unwrap();

            assert_eq!(a as i64, 2);
            assert_eq!(b as i64, 25);
            assert_eq!(number as i64, 2);
            assert_eq!(y, ((20.0 / 7.0) + 3.24));
        }
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
        let k =
            SemanticAnalyzer::new()._visit(Rc::new(RefCell::new(Parser::new(&tokens).parser())));
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

            SemanticAnalyzer::new()._visit(Rc::new(RefCell::new(Parser::new(&tokens).parser())));
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
        SemanticAnalyzer::new()._visit(Rc::new(RefCell::new(Parser::new(&tokens).parser())));
    }

    #[test]
    fn test_proccall() {
        let path = "./src/tests/test2.pas";
        let test_cases = vec![("a", 8), ("b", 7), ("x", 30)];

        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");

            let tokens = Lexer::new(buff).get_tokens();

            let ast = Rc::new(RefCell::new(Parser::new(&tokens).parser()));

            let e = SemanticAnalyzer::new()._visit(ast.clone());

            let mut i = Interpreter::new().set_symbol_table(e).set_parser(ast);

            i._interpret();

            test_cases.iter().for_each(|x| {
                assert_eq!(
                    x.1,
                    *i.call_stack.recodes_debug[1]
                        .borrow()
                        .get_item(x.0)
                        .unwrap() as i32
                )
            });
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

            let test_cases = vec![("a", 8), ("b", 7), ("x", 30)];
            test_cases.iter().for_each(|x| {
                assert_eq!(
                    x.1,
                    *i.call_stack.recodes_debug[1]
                        .borrow()
                        .get_item(x.0)
                        .unwrap() as i32
                )
            });

            let test_cases2 = vec![("a", 5), ("b", 10), ("x", 70)];
            test_cases2.iter().for_each(|x| {
                assert_eq!(
                    x.1,
                    *i.call_stack.recodes_debug[2]
                        .borrow()
                        .get_item(x.0)
                        .unwrap() as i32
                )
            });

            eprintln!("{:#?}", i.call_stack.recodes_debug);
        }
    }
}
