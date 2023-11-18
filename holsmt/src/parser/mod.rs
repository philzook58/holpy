pub mod parser;

#[cfg(test)]
mod tests {
    use crate::ast::{rc::Rc, Type};

    use super::parser::TypeParser;

    #[test]
    fn test_valid_type_variable() {
        let test_data = vec![
            ("'_", Type::new_tvar("_")),
            ("'a", Type::new_tvar("a")),
            ("'A", Type::new_tvar("A")),
            ("'_a", Type::new_tvar("_a")),
            ("'_A", Type::new_tvar("_A")),
            ("'_1", Type::new_tvar("_1")),
            ("'a1", Type::new_tvar("a1")),
            ("'A1", Type::new_tvar("A1")),
            ("'_abc_a_b_c_1", Type::new_tvar("_abc_a_b_c_1")),
            ("?_abc_a_b_c_1", Type::new_stvar("_abc_a_b_c_1")),
            ("(((?_abc_a_b_c_1)))", Type::new_stvar("_abc_a_b_c_1")),
        ];
        for (input, target) in test_data {
            assert_eq!(TypeParser::new().parse(input).unwrap(), target);
        }
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
                Type::new_tconst(
                    "fun",
                    vec![Rc::new(Type::new_tvar("a")), Rc::new(Type::new_tvar("b"))],
                )
            )
        );
    }
}
