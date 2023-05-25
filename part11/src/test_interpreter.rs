#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read;

    use crate::spi11::pascal_parser::*;

    #[test]
    fn test_11_lexer() {
        let str = "PROGRAM Test;
VAR
    a, c : INTEGER;
    number: REAL;
BEGIN
        a := 125 DIV 3.14 / {pass comment} 12;
        number := 2.5 * 2.0; 
END."
            .to_string();

        let mut lexer = Lexer::new(str);
        let tokens = vec![
            Token::new(TokenType::Program, "PROGRAM"),
            Token::new(TokenType::Id, "Test"),
            Token::new(TokenType::Semi, ";"),
            Token::new(TokenType::Var, "VAR"),
            Token::new(TokenType::Id, "a"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Id, "c"),
            Token::new(TokenType::Colon, ":"),
            Token::new(TokenType::Integer, "INTEGER"),
            Token::new(TokenType::Semi, ";"),
            Token::new(TokenType::Id, "number"),
            Token::new(TokenType::Colon, ":"),
            Token::new(TokenType::Real, "REAL"),
            Token::new(TokenType::Semi, ";"),
            Token::new(TokenType::Begin, "BEGIN"),
            Token::new(TokenType::Id, "a"),
            Token::new(TokenType::Assign, ":="),
            Token::new(TokenType::IntegerConst, "125"),
            Token::new(TokenType::IntegerDiv, "DIV"),
            Token::new(TokenType::RealConst, "3.14"),
            Token::new(TokenType::FloatDiv, "/"),
            Token::new(TokenType::IntegerConst, "12"),
            Token::new(TokenType::Semi, ";"),
            Token::new(TokenType::Id, "number"),
            Token::new(TokenType::Assign, ":="),
            Token::new(TokenType::RealConst, "2.5"),
            Token::new(TokenType::Mul, "*"),
            Token::new(TokenType::RealConst, "2.0"),
            Token::new(TokenType::Semi, ";"),
            Token::new(TokenType::End, "END"),
            Token::new(TokenType::Dot, "."),
            Token::new(TokenType::Eof, "EOF"),
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

    #[test]
    fn test_statements() {
        let input = "
PROGRAM Part10;
VAR
    number     : INTEGER;
    a, b, c, x : INTEGER;
    y          : REAL;

BEGIN {Part10}
    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number DIV 4;
        c := a - - b
    END;
    x := 11;
    y := 20 / 7 + 3.24;
END.  {Part10}
"
        .to_string();

        let results = Interpreter::<f64>::new(Parser::new(Lexer::new(input))).interpret();
        assert_eq!(results.len(), 6);
        assert_eq!(2, *results.get("number").unwrap() as i64);
        assert_eq!(2, *results.get("a").unwrap() as i64);
        assert_eq!(25, *results.get("b").unwrap() as i64);
        assert_eq!(27, *results.get("c").unwrap() as i64);
        assert_eq!(11, *results.get("x").unwrap() as i64);
        assert_eq!(20_f64 / 7_f64 + 3.24, *results.get("y").unwrap());
    }

    #[test]
    fn test_symbols() {
        let input = "
PROGRAM Part10;
VAR
    number     : INTEGER;
    a, b, c, x : INTEGER;
    y          : REAL;

BEGIN {Part10}
    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number DIV 4;
        c := a - - b
    END;
    x := 11;
    y := 20 / 7 + 3.24;
END.  {Part10}
"
        .to_string();

        let node = Parser::new(Lexer::new(input)).parser();

        SymbolTableBuilder::new()._visit(node);
    }

    #[test]

    fn test_file() {
        let path = "./part11.pas";
        do_test_file(path);
    }

    #[test]
    #[should_panic]
    fn test_file2() {
        let path = "./nameerror2.pas";
        do_test_file(path);
    }

    #[test]
    #[should_panic]
    fn test_file3() {
        let path = "./nameerror1.pas";
        do_test_file(path);
    }

    fn do_test_file(path: &str) -> String {
        let mut k = "".to_string();
        if let Ok(mut file) = File::open(path) {
            let mut input = String::new();
            file.read_to_string(&mut input).expect("File can't open.");

            k = SymbolTableBuilder::new()._visit(Parser::new(Lexer::new(input)).parser());
        }
        k
    }
}
