use crate::ast::{error::TypeError, rc::Rc, term::Term, Type};

// todo what is borrow::Cow?
use std::{
    borrow::Cow,
    fmt::{self, format},
    io,
};

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
        // todo

        fn helper(t: &Rc<Term>, bd_vars: &mut Vec<String>) -> String {
            if t.is_svar() {
                let (name, _) = t.get_name_type().unwrap();
                return format!("?{}", name);
            } else if t.is_var() || t.is_const(None) {
                let (name, _) = t.get_name_type().unwrap();
                return format!("{}", name);
            } else if t.is_comb(None, None) {
                // todo
                let (fun, arg) = t.get_fun_and_arg().unwrap();
                let str_fun: String;
                let str_arg: String;
                if fun.is_abs() {
                    str_fun = format!("({})", helper(fun, bd_vars));
                } else {
                    str_fun = helper(fun, bd_vars);
                }
                if arg.is_comb(None, None) || arg.is_abs() {
                    str_arg = format!("({})", helper(arg, bd_vars));
                } else {
                    str_arg = helper(arg, bd_vars);
                }
                return format!("{} {}", str_fun, str_arg);
            } else if t.is_abs() {
                // todo
                let (var_name, _, body) = t.get_varname_varty_body().unwrap();
                // println!("var_name: {}", var_name);
                // println!("bd_vars: {:?}", bd_vars);
                let mut new_bd_vars = vec![var_name.clone(), bd_vars.clone().concat()];
                // println!("new_bd_vars: {:?}", new_bd_vars);
                let body_repr = helper(body, &mut new_bd_vars);
                return format!("%{}. {}", var_name, body_repr);
            } else if t.is_bound() {
                let n: usize = t.get_n().unwrap();
                if n >= bd_vars.len() {
                    return format!(":B{}", n);
                } else {
                    return format!("{}", bd_vars[n]);
                }
            } else {
                !unreachable!()
            }
        }
        let mut bd_vars = Vec::new();
        write!(f, "{}", helper(self, &mut bd_vars))
    }
}
