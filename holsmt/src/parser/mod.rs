pub mod parser;

#[cfg(test)]
mod tests {
    use crate::ast::{rc::Rc, Type};

    use super::parser::TypeParser;

    #[test]
    fn test_valid_type_variable() {
        assert!(TypeParser::new().parse("'_").is_ok());
        assert!(TypeParser::new().parse("'a").is_ok());
        assert!(TypeParser::new().parse("'A").is_ok());
        assert!(TypeParser::new().parse("'_a").is_ok());
        assert!(TypeParser::new().parse("'_A").is_ok());
        assert!(TypeParser::new().parse("'_1").is_ok());
        assert!(TypeParser::new().parse("'a1").is_ok());
        assert!(TypeParser::new().parse("'A1").is_ok());
        assert!(TypeParser::new().parse("'_abc_a_b_c_1").is_ok());
        assert!(TypeParser::new().parse("('a)").is_ok());
        assert_eq!(
            TypeParser::new().parse("('a)").unwrap(),
            Type::new_TVar("a")
        );
    }

    #[test]
    fn test_valid_simple_type_variable() {
        assert!(TypeParser::new().parse("?a_1").is_ok());
    }

    #[test]
    fn test_valid_fun_type_variable() {
        assert!(TypeParser::new().parse("'a ⇒ 'b").is_ok());
        assert!(TypeParser::new().parse("('a => 'b ⇒ 'c)").is_ok());
        // Because Rc judges whether it is equal according to the address,
        // it is not equal here, and display is used to simply judge whether it is the same
        assert_eq!(
            format!("{}", TypeParser::new().parse("('a => 'b)").unwrap()),
            format!(
                "{}",
                Type::new_TConst(
                    "fun",
                    vec![Rc::new(Type::new_TVar("a")), Rc::new(Type::new_TVar("b"))],
                )
            )
        );
    }
}
