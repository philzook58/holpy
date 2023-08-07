pub mod error;
pub mod pool;
pub mod printer;
pub mod rc;
pub mod term;

use ahash::{AHashMap, AHashSet};
use error::TypeError;
use rc::Rc;
use std::collections::HashMap;
use term::Term;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    STVar(String), // 1 + 24

    TVar(String), // 1 + 24

    TConst(String, Vec<Rc<Type>>), // 1 + 24 + 24 == 49
}

impl Type {
    /// Constructs a new `Type::STVar`.
    pub fn new_TVar(name: impl Into<String>) -> Self {
        Type::TVar(name.into())
    }

    /// Constructs a new `Type::STVar`.
    pub fn new_STVar(name: impl Into<String>) -> Self {
        Type::STVar(name.into())
    }

    /// Constructs a new `Type::TConst`.
    pub fn new_TConst(name: impl Into<String>, args: Vec<Rc<Type>>) -> Self {
        Type::TConst(name.into(), args)
    }

    /// Returns `true` if the type is a schematic type variable.
    pub fn is_stvar(&self) -> bool {
        matches!(self, Type::STVar(_))
    }

    /// Returns `true` if the type is a type variable.
    pub fn is_tvar(&self) -> bool {
        matches!(self, Type::TVar(_))
    }

    /// Returns `true` if the type is a type constant.
    pub fn is_tconst(&self) -> bool {
        matches!(self, Type::TConst(_, _))
    }

    /// Returns `true` if the type is a function type.
    pub fn is_fun(&self) -> bool {
        match self {
            Type::TConst(name, _) => name == "fun",
            _ => false,
        }
    }

    /// Given a type of form a => b, return a.
    pub fn domain_type(&self) -> Result<Rc<Type>, TypeError> {
        let res = match self {
            Type::TConst(_, args) => Some(rc::Rc::clone(&args[0])),
            _ => None,
        };
        res.ok_or_else(|| TypeError::NotFun)
    }

    /// Given a type of form a => b, return b.
    pub fn range_type(&self) -> Result<Rc<Type>, TypeError> {
        let res = match self {
            Type::TConst(_, args) => Some(rc::Rc::clone(&args[1])),
            _ => None,
        };
        res.ok_or_else(|| TypeError::NotFun)
    }

    fn get_args(&self) -> Option<&Vec<Rc<Type>>> {
        match self {
            Type::TConst(_, args) => Some(args),
            _ => None,
        }
    }

    /// Returns the size of the type.
    pub fn size(&self) -> Result<usize, TypeError> {
        if self.is_stvar() || self.is_tvar() {
            Ok(1)
        } else if self.is_tconst() {
            let args = self.get_args().unwrap();
            let mut res = 1usize;
            for x in args {
                res += x.size()?;
            }
            Ok(res)
        } else {
            // todo raise exception
            Err(TypeError::NotFun)
        }
    }

    fn get_name(&self) -> &String {
        match self {
            Type::STVar(name) => name,
            Type::TVar(name) => name,
            Type::TConst(name, _) => name,
        }
    }

    pub fn is_numeral_type(&self) -> bool {
        matches!(
            self,
            Type::TConst(name, _) if name == "nat" || name == "int" || name == "real"
        )
    }

    /// Implement a `repr` method for the `Type` enum to get a string representation of the type.
    pub fn repr(&self) -> String {
        match self {
            Type::STVar(name) => format!("STVar({})", name),
            Type::TVar(name) => format!("TVar({})", name),
            Type::TConst(name, args) => {
                let args_repr: Vec<String> = args.iter().map(|arg| arg.repr()).collect();
                format!("TConst({}, [{}])", name, args_repr.join(", "))
            }
        }
    }
}

impl Rc<Type> {
    /// Given a type of form a_1 => ... => a_n, b, return the pair
    /// [a_1, ... a_n], b.
    pub fn strip_type(&self) -> Result<(Vec<Rc<Type>>, Rc<Type>), TypeError> {
        if self.is_fun() {
            let (domain, range) = (self.domain_type()?, self.range_type()?);
            let (mut domains, final_range) = range.strip_type()?;
            domains.push(domain);
            Ok((domains, final_range))
        } else {
            // todo modify clone
            return Ok((Vec::new(), rc::Rc::clone(self)));
        }
    }

    /// Simultaneously substitute for the type variables using tyinst.
    /// Parameters
    /// ==========
    /// tyinst : TyInst
    ///     Type instantiation to be substituted.
    pub fn subst(
        &self,
        tyinst: &HashMap<String, Rc<Type>>,
        push_cache: &dyn Fn(Type) -> Rc<Type>,
    ) -> Result<Rc<Type>, TypeError> {
        if self.is_stvar() {
            match tyinst.get(self.get_name()) {
                Some(value) => Ok(rc::Rc::clone(value)),
                None => Ok(rc::Rc::clone(self)),
            }
        } else if self.is_tvar() {
            Ok(rc::Rc::clone(self))
        } else if self.is_tconst() {
            let args = self.get_args().unwrap();
            // todo modify unwrap
            let substituted_args = args
                .iter()
                .map(|T| T.subst(&tyinst, push_cache).unwrap())
                .collect();
            // todo add cache
            let ty = Type::new_TConst(self.get_name(), substituted_args);
            Ok(rc::Rc::clone(&push_cache(ty)))
        } else {
            Err(TypeError::Default)
        }
    }

    fn match_incr(
        &self,
        T: &Rc<Type>,
        tyinst: &mut HashMap<String, Rc<Type>>,
    ) -> Result<(), TypeError> {
        if self.is_stvar() {
            // todo calculate hashval twice?
            if tyinst.contains_key(self.get_name()) {
                let value = tyinst.get(self.get_name()).unwrap();
                if value != T {
                    Err(TypeError::MatchError(format!(
                        "Unable to match {} with {}",
                        T, value
                    )))
                } else {
                    Ok(())
                }
            } else {
                tyinst.insert(self.get_name().clone(), rc::Rc::clone(T));
                Ok(())
            }
        } else if self.is_tvar() {
            if self != T {
                Err(TypeError::MatchError(format!(
                    "Unable to match {} with {}",
                    self, T,
                )))
            } else {
                Ok(())
            }
        } else if self.is_tconst() {
            if !T.is_tconst() || T.get_name() != self.get_name() {
                Err(TypeError::MatchError(format!(
                    "Unable to match {} with {}",
                    self, T,
                )))
            } else {
                // todo
                // Implement the iteration over args and call match_incr recursively
                let args_zip = self
                    .get_args()
                    .unwrap()
                    .iter()
                    .zip(T.get_args().unwrap().iter());
                for (arg, argT) in args_zip {
                    arg.match_incr(argT, tyinst)?;
                }
                Ok(())
            }
        } else {
            Err(TypeError::Default)
        }
    }

    pub fn _match(&self, T: &Rc<Type>) -> Result<HashMap<String, Rc<Type>>, TypeError> {
        let mut res: HashMap<String, Rc<Type>> = HashMap::new();
        self.match_incr(&T, &mut res)?;
        Ok(res)
    }

    /// Return the list of schematic type variables.
    pub fn get_stvars(&self) -> Vec<Rc<Type>> {
        let mut res: Vec<Rc<Type>> = Vec::new();

        fn collect(T: &Rc<Type>, res: &mut Vec<Rc<Type>>) {
            if T.is_stvar() {
                if !res.contains(T) {
                    res.push(rc::Rc::clone(T));
                }
            } else if T.is_tvar() {
                // Do nothing for TVar
            } else {
                let args = T.get_args().unwrap();
                for arg in args {
                    collect(arg, res);
                }
            }
        }

        collect(self, &mut res);
        res
    }

    /// Return the list of type variables.
    pub fn get_tvars(&self) -> Vec<Rc<Type>> {
        let mut res: Vec<Rc<Type>> = Vec::new();

        fn collect(T: &Rc<Type>, res: &mut Vec<Rc<Type>>) {
            if T.is_tvar() {
                // todo O(n)
                if !res.contains(T) {
                    res.push(rc::Rc::clone(T));
                }
            } else if T.is_stvar() {
                // Do nothing for STVar
            } else {
                let args = T.get_args().unwrap();
                for arg in args {
                    collect(arg, res);
                }
            }
        }
        collect(self, &mut res);
        res
    }

    /// Return the list of schematic type variables and type variables appearing in self.
    pub fn get_tsubs(&self) -> Vec<Rc<Type>> {
        let mut res: Vec<Rc<Type>> = Vec::new();

        fn collect(T: &Rc<Type>, res: &mut Vec<Rc<Type>>) {
            if T.is_tconst() {
                let args = T.get_args().unwrap();
                for arg in args {
                    collect(arg, res);
                }
            }
            // todo O(n)
            if !res.contains(T) {
                res.push(rc::Rc::clone(T));
            }
        }
        collect(self, &mut res);
        res
    }

    // todo, reallocation memory?
    pub fn convert_stvar(&self) -> Result<Type, TypeError> {
        // if self.is_stvar() {
        //     Err(TypeError::ConvertSTVar)
        // } else if self.is_tvar() {
        //     Ok(Type::STVar(self.get_name().clone()))
        // } else if self.is_tconst() {
        //     let args = self.get_args().unwrap();
        //     let converted_args: Result<Vec<Type>, TypeError> =
        //         args.iter().map(|arg| arg.convert_stvar()).collect();
        //     Ok(Type::TConst(self.get_name().clone(), converted_args?))
        // } else {
        //     Err(TypeError::ConvertSTVar)
        // }
        !unimplemented!()
    }
}

// A marker trait that allows the type to be used as keys in hash maps.
impl Eq for Rc<Type> {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn test_print() {
        let BoolType = Type::TConst("bool".to_string(), vec![]);
        println!("{}", BoolType);
        println!("{}", BoolType.is_tconst());
        let NatType = Type::TConst("nat".to_string(), vec![]);
        println!("{}", NatType);
        let fun = Type::TConst(
            "fun".to_string(),
            vec![Rc::new(NatType.clone()), Rc::new(BoolType.clone())],
        );
        println!("{}", fun);
        println!("{}", fun.is_fun());
        let a = Type::new_TVar("a");
        println!("{}", a);
        println!("{}", Type::STVar("a".to_string()))
    }

    #[test]
    fn print_size() {
        println!("Alignment of Type enum: {} bytes", mem::align_of::<Type>());
        println!("Type: {}", mem::size_of::<Type>());
        println!("Rc<Type> size: {}", mem::size_of::<Rc<Type>>());
        println!("String size: {}", mem::size_of::<String>());
        println!("Vec<Rc<Type>> size: {}", mem::size_of::<Vec<Rc<Type>>>());
    }
}
