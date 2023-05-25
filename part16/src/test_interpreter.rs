#[allow(dead_code, unused, unused_variables, unused_imports)]
#[cfg(test)]
mod tests {
    use crate::spi16::pascal_parser::{
        Interpreter, Lexer, Parser, SemanticAnalyzer, Token, TokenType,
    };
    use std::fs::File;
    use std::io::Read;

    #[test]
    fn test_16_lexer() {
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
    fn test_ast_tree() {
        let str = "PROGRAM Test;
VAR
    a : INTEGER;
BEGIN 
    a := 125+12;
END."
            .to_string();
        let k = Interpreter::<f64>::new(Parser::new(Lexer::new(str))).interpret();
        let a = *k.get("a").unwrap();
        assert_eq!(137.0, a);
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
        let interpret = |s: (&str, i64)| {
            let input = format!(
                "
        PROGRAM Test;
        VAR
            a : INTEGER;
        BEGIN
            a := {}
        END.
        ",
                s.0
            );

            let results = Interpreter::<f64>::new(Parser::new(Lexer::new(input))).interpret();
            if let Some(result) = results.get(s.0) {
                assert_eq!(s.1, *result as i64);
            }
        };
        case.iter().for_each(|x| {
            interpret(*x);
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

            let results = Interpreter::<f64>::new(Parser::new(Lexer::new(input))).interpret();
            if let Some(result) = results.get(s.0) {
                assert_eq!(s.1, *result);
            }
        };
        case.iter().for_each(|x| {
            interpret(*x);
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
    fn test_statements() {
        let results = Interpreter::<f64>::new(Parser::new(Lexer::new(INPUT))).interpret();
        assert_eq!(results.len(), 4);
        assert_eq!(2, *results.get("number").unwrap() as i64);
        assert_eq!(2, *results.get("a").unwrap() as i64);
        assert_eq!(25, *results.get("b").unwrap() as i64);
        assert_eq!(20_f64 / 7_f64 + 3.24, *results.get("y").unwrap());
    }

    #[test]
    fn test_input2() {
        let k = SemanticAnalyzer::new()._visit(Parser::new(Lexer::new(INPUT2)).parser());
        k.iter().for_each(|s| {
            eprintln!("{}\n", s.borrow().to_string());
        });
    }

    #[test]
    #[should_panic]
    fn test_symbol_not_found() {
        let path = "./dupiderror.pas";
        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");
            SemanticAnalyzer::new()._visit(Parser::new(Lexer::new(buff)).parser());
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

    #[test]
    fn test_proccall() {
        let path = "./part16.pas";
        if let Ok(mut file) = File::open(path) {
            let mut buff = String::new();
            file.read_to_string(&mut buff).expect("read file error");
            SemanticAnalyzer::new()._visit(Parser::new(Lexer::new(buff)).parser());
        }
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
        SemanticAnalyzer::new()._visit(Parser::new(Lexer::new(WRONG_PARAMS_NUM)).parser());
    }
}
