#[allow(dead_code, unused, unused_variables, unused_imports)]
#[cfg(test)]
mod tests {
    use crate::spi19::pascal_parser::*;
    use num_traits::Bounded;
    use std::cell::RefCell;
    use std::fs::File;
    use std::io::Read;
    use std::rc::Rc;

    #[test]
    fn test_19_lexer() {
        let str = "PROGRAM Part12;
VAR
    a, c : INTEGER;

PROCEDURE P1;
VAR
    a, c : REAL;
    k: INTEGER;
    PROCEDURE P2(a : INTEGER);
        VAR
            a, z : INTEGER;
        BEGIN {P2}
            z := 777;
        END; {P2}

BEGIN {P1}

END; {P1}
BEGIN {Part12}
    a := 10;
END. {Part12}"
            .to_string();

        let mut lexer = Lexer::new(str);
        let tokens = vec![
            Token::new(TokenType::Program, "PROGRAM").set_lineno_column(1, 8),
            Token::new(TokenType::Id, "Part12").set_lineno_column(1, 15),
            Token::new(TokenType::Semi, ";").set_lineno_column(1, 16),
            Token::new(TokenType::Var, "VAR").set_lineno_column(2, 4),
            Token::new(TokenType::Id, "a").set_lineno_column(3, 6),
            Token::new(TokenType::Comma, ",").set_lineno_column(3, 7),
            Token::new(TokenType::Id, "c").set_lineno_column(3, 9),
            Token::new(TokenType::Colon, ":").set_lineno_column(3, 11),
            Token::new(TokenType::Integer, "INTEGER").set_lineno_column(3, 19),
            Token::new(TokenType::Semi, ";").set_lineno_column(3, 20),
            Token::new(TokenType::Procedure, "PROCEDURE").set_lineno_column(5, 10),
            Token::new(TokenType::Id, "P1").set_lineno_column(5, 13),
            Token::new(TokenType::Semi, ";").set_lineno_column(5, 14),
            Token::new(TokenType::Var, "VAR").set_lineno_column(6, 4),
            Token::new(TokenType::Id, "a").set_lineno_column(7, 6),
            Token::new(TokenType::Comma, ",").set_lineno_column(7, 7),
            Token::new(TokenType::Id, "c").set_lineno_column(7, 9),
            Token::new(TokenType::Colon, ":").set_lineno_column(7, 11),
            Token::new(TokenType::Real, "REAL").set_lineno_column(7, 16),
            Token::new(TokenType::Semi, ";").set_lineno_column(7, 17),
            Token::new(TokenType::Id, "k").set_lineno_column(8, 6),
            Token::new(TokenType::Colon, ":").set_lineno_column(8, 7),
            Token::new(TokenType::Integer, "INTEGER").set_lineno_column(8, 15),
            Token::new(TokenType::Semi, ";").set_lineno_column(8, 16),
            Token::new(TokenType::Procedure, "PROCEDURE").set_lineno_column(9, 14),
            Token::new(TokenType::Id, "P2").set_lineno_column(9, 17),
            Token::new(TokenType::Lbrack, "(").set_lineno_column(9, 18),
            Token::new(TokenType::Id, "a").set_lineno_column(9, 19),
            Token::new(TokenType::Colon, ":").set_lineno_column(9, 21),
            Token::new(TokenType::Integer, "INTEGER").set_lineno_column(9, 29),
            Token::new(TokenType::Rbrack, ")").set_lineno_column(9, 30),
            Token::new(TokenType::Semi, ";").set_lineno_column(9, 31),
            Token::new(TokenType::Var, "VAR").set_lineno_column(10, 12),
            Token::new(TokenType::Id, "a").set_lineno_column(11, 14),
            Token::new(TokenType::Comma, ",").set_lineno_column(11, 15),
            Token::new(TokenType::Id, "z").set_lineno_column(11, 17),
            Token::new(TokenType::Colon, ":").set_lineno_column(11, 19),
            Token::new(TokenType::Integer, "INTEGER").set_lineno_column(11, 27),
            Token::new(TokenType::Semi, ";").set_lineno_column(11, 28),
            Token::new(TokenType::Begin, "BEGIN").set_lineno_column(12, 14),
            Token::new(TokenType::Id, "z").set_lineno_column(13, 14),
            Token::new(TokenType::Assign, ":=").set_lineno_column(13, 17),
            Token::new(TokenType::IntegerConst, "777").set_lineno_column(13, 21),
            Token::new(TokenType::Semi, ";").set_lineno_column(13, 22),
            Token::new(TokenType::End, "END").set_lineno_column(14, 12),
            Token::new(TokenType::Semi, ";").set_lineno_column(14, 13),
            Token::new(TokenType::Begin, "BEGIN").set_lineno_column(16, 6),
            Token::new(TokenType::End, "END").set_lineno_column(18, 4),
            Token::new(TokenType::Semi, ";").set_lineno_column(18, 5),
            Token::new(TokenType::Begin, "BEGIN").set_lineno_column(19, 6),
            Token::new(TokenType::Id, "a").set_lineno_column(20, 6),
            Token::new(TokenType::Assign, ":=").set_lineno_column(20, 9),
            Token::new(TokenType::IntegerConst, "10").set_lineno_column(20, 12),
            Token::new(TokenType::Semi, ";").set_lineno_column(20, 13),
            Token::new(TokenType::End, "END").set_lineno_column(21, 4),
            Token::new(TokenType::Dot, ".").set_lineno_column(21, 5),
            Token::new(TokenType::Eof, "EOF").set_lineno_column(21, 13),
        ];

        tokens
            .iter()
            .for_each(|token| assert_eq!(token, &lexer.get_next_token()));
    }

    #[test]
    fn test_stack() {
        let str = "PROGRAM Test;
VAR
    a : INTEGER;
BEGIN 
    a := 125+12;
END."
            .to_string();
        let mut interpreter = Interpreter::new()
            .set_parser(Rc::new(RefCell::new(Parser::new(Lexer::new(str)).parser())));
        interpreter.interpret();
        let mut a = i64::MAX;
        if let Some(ar) = interpreter.call_stack.recodes_debug.first() {
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

            Interpreter::new().set_parser(Rc::new(RefCell::new(
                Parser::new(Lexer::new(input)).parser(),
            )))
        };
        case.iter().for_each(|(input, var)| {
            let mut i = interpret(input, *var);
            i.interpret();
            let mut a = i32::MAX;
            if let Some(ar) = i.call_stack.recodes_debug.first() {
                if let Some(v) = ar.borrow().get_item("a") {
                    a = *v as i32;
                }
            }
            assert_eq!(a, *var);
        });
    }

    #[test]
    fn test_parse2() {
        let case = [("3.24", 3.24),
            ("2.14 + 7 * 4", 30.14),
            ("7.14 - 8 / 4", 5.14)];

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

            Interpreter::new().set_parser(Rc::new(RefCell::new(
                Parser::new(Lexer::new(input)).parser(),
            )))
        };
        case.iter().for_each(|x| {
            let mut i = interpret((x.0, x.1));
            i.interpret();
            let mut a = 0.0;
            if let Some(ar) = i.call_stack.recodes_debug.first() {
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
        let mut i = Interpreter::new().set_parser(Rc::new(RefCell::new(
            Parser::new(Lexer::new(INPUT)).parser(),
        )));
        i.interpret();

        if let Some(ar) = i.call_stack.recodes_debug.first() {
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
        let k = SemanticAnalyzer::new()._visit(Rc::new(RefCell::new(
            Parser::new(Lexer::new(INPUT2)).parser(),
        )));
        k.iter().for_each(|s| {
            eprintln!("{}\n", s.borrow());
        });
    }

    #[test]
    #[should_panic]
    fn test_symbol_not_found() {
        let path = "./dupiderror.pas";
        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");
            SemanticAnalyzer::new()._visit(Rc::new(RefCell::new(
                Parser::new(Lexer::new(buff)).parser(),
            )));
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
        Parser::new(Lexer::new(UNEXPECTED_TOKEN)).parser();
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
        SemanticAnalyzer::new()._visit(Rc::new(RefCell::new(
            Parser::new(Lexer::new(WRONG_PARAMS_NUM)).parser(),
        )));
    }

    #[test]
    fn test_proccall() {
        let path = "./part18.pas";
        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");
            let ast = Rc::new(RefCell::new(Parser::new(Lexer::new(buff.clone())).parser()));

            let e = SemanticAnalyzer::new()._visit(ast.clone());

            let mut i = Interpreter::new().set_symbol_table(e).set_parser(ast);

            i._interpret();
            let test_cases = [("a", 8), ("b", 7), ("x", 30)];
            test_cases.iter().for_each(|x| {
                assert_eq!(
                    x.1,
                    *i.call_stack.recodes_debug[1]
                        .borrow()
                        .get_item(x.0)
                        .unwrap() as i32
                )
            });

            //eprintln!("{:#?}", i.call_stack.recodes_debug);
        }
    }

    #[test]
    fn test_proccall2() {
        let path = "./part19.pas";
        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");
            let ast = Rc::new(RefCell::new(Parser::new(Lexer::new(buff.clone())).parser()));

            let e = SemanticAnalyzer::new()._visit(ast.clone());

            let mut i = Interpreter::new().set_symbol_table(e).set_parser(ast);

            i._interpret();

            let test_cases = [("a", 8), ("b", 7), ("x", 30)];
            test_cases.iter().for_each(|x| {
                assert_eq!(
                    x.1,
                    *i.call_stack.recodes_debug[1]
                        .borrow()
                        .get_item(x.0)
                        .unwrap() as i32
                )
            });

            let test_cases2 = [("a", 5), ("b", 10), ("x", 70)];
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
