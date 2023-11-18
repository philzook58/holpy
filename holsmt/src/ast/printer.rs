use crate::ast::{rc::Rc, term::Term, Type};

// todo what is borrow::Cow?
use std::fmt;

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

impl fmt::Display for Rc<Term> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn helper(t: &Rc<Term>, bd_vars: &mut Vec<String>) -> String {
            match t.as_ref() {
                Term::SVar(name, _) => format!("?{}", name),
                Term::Var(name, _) | Term::Const(name, _) => format!("{}", name),
                Term::Comb(fun, arg) => {
                    let str_fun = match fun.is_abs() {
                        true => format!("({})", helper(fun, bd_vars)),
                        false => helper(fun, bd_vars),
                    };
                    let str_arg = match arg.is_comb(None, None) || arg.is_abs() {
                        true => format!("({})", helper(arg, bd_vars)),
                        false => helper(arg, bd_vars),
                    };
                    format!("{} {}", str_fun, str_arg)
                }
                Term::Abs(var_name, _, body) => {
                    let mut new_bd_vars = vec![var_name.clone(), bd_vars.clone().concat()];
                    let body_repr = helper(body, &mut new_bd_vars);
                    format!("%{}. {}", var_name, body_repr)
                }
                Term::Bound(n) => {
                    if *n >= bd_vars.len() {
                        format!(":B{}", n)
                    } else {
                        format!("{}", bd_vars[*n])
                    }
                }
            }
        }

        let mut bd_vars = Vec::new();
        write!(f, "{}", helper(self, &mut bd_vars))
    }
}
