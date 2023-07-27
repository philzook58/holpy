use crate::ast::{rc::Rc, term::Term, Type};

// todo what is borrow::Cow?
use std::{borrow::Cow, fmt, io};

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::STVar(name) => write!(f, "?'{}", name),
            Type::TVar(name) => write!(f, "'{}", name),
            Type::TConst(name, args) => {
                if args.is_empty() {
                    write!(f, "{}", name)
                } else if args.len() == 1 {
                    if args[0].is_fun() {
                        write!(f, "({}) {}", args[0], name)
                    } else {
                        write!(f, "{} {}", args[0], name)
                    }
                } else if self.is_fun() {
                    if args[0].is_fun() {
                        write!(f, "({}) => {}", args[0], args[1])
                    } else {
                        write!(f, "{} => {}", args[0], args[1])
                    }
                } else {
                    write!(
                        f,
                        "({}) {}",
                        args.iter()
                            .map(|x| format!("{}", x))
                            .collect::<Vec<String>>()
                            .join(", "),
                        name,
                    )
                }
            }
        }
    }
}
