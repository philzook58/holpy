pub mod error;
pub mod pool;
pub mod printer;
pub mod rc;
pub mod term;

use error::TypeError;
use rc::Rc;
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

    /// Given a type of form a_1 => ... => a_n, b, return the pair
    /// [a_1, ... a_n], b.
    // todo
    pub fn strip_type(&self) -> Result<(Vec<Rc<Type>>, Rc<Type>), TypeError> {
        if self.is_fun() {
            let (domain, range) = (self.domain_type()?, self.range_type()?);
            let (mut domains, final_range) = range.strip_type()?;
            domains.push(domain);
            Ok((domains, final_range))
        } else {
            // todo modify clone
            return Ok((Vec::new(), rc::Rc::new(self.clone())));
        }
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

    /// Simultaneously substitute for the type variables using tyinst.
    /// Parameters
    /// ==========
    /// tyinst : TyInst
    ///     Type instantiation to be substituted.
    pub fn subst(&self, tyinst: Option<Type>) -> Type {
        unimplemented!()
    }

    pub fn match_incr() {
        !unimplemented!()
    }

    pub fn _match() {
        !unimplemented!()
    }

    pub fn get_stvars(&self) {
        !unimplemented!()
    }

    pub fn get_tvars(&self) {
        !unimplemented!()
    }

    pub fn get_tsubs(&self) {
        !unimplemented!()
    }

    pub fn convert_stvar(&self) {
        !unimplemented!()
    }

    pub fn is_numeral_type(&self) {
        !unimplemented!()
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

impl Rc<Type> {}

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
