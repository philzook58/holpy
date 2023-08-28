pub mod error;
pub mod pool;
pub mod printer;
pub mod rc;
pub mod term;
mod utils;

use error::HolrsError;
use rc::Rc;
use std::collections::HashMap;
use term::Term;

use self::{error::HolrsResult, pool::TermPool};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Schematic type variable.
    STVar(String), // 1 + 24

    /// Type variable.
    TVar(String), // 1 + 24

    /// Type constant, applied to a list of arguments.
    TConst(String, Vec<Rc<Type>>), // 1 + 24 + 24 == 49
}

impl Type {
    /// Constructs a new `Type::STVar`.
    pub fn new_tvar(name: impl Into<String>) -> Self {
        Type::TVar(name.into())
    }

    /// Constructs a new `Type::STVar`.
    pub fn new_stvar(name: impl Into<String>) -> Self {
        Type::STVar(name.into())
    }

    /// Constructs a new `Type::TConst`.
    pub fn new_tconst(name: impl Into<String>, args: Vec<Rc<Type>>) -> Self {
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
        matches!(self,Type::TConst(name, _) if name == "fun")
    }

    /// Returns `true` if the type is a numeral type.
    pub fn is_numeral_type(&self) -> bool {
        matches!(self,Type::TConst(name, _) if name == "nat" || name == "int" || name == "real")
    }

    /// Returns `true` if the type is a real type.
    pub fn is_real(&self) -> bool {
        matches!(self, Type::TConst(name, _) if name == "real")
    }

    /// Returns `true` if the type is a integer type.
    pub fn is_int(&self) -> bool {
        matches!(self, Type::TConst(name, _) if name == "int")
    }

    /// Returns `true` if the type is a natural number type.
    pub fn is_nat(&self) -> bool {
        matches!(self, Type::TConst(name, _) if name == "nat")
    }

    /// Given a type of form a => b, return a.
    pub fn domain_type(&self) -> HolrsResult<Rc<Type>> {
        let res = match self {
            Type::TConst(_, args) => Some(rc::Rc::clone(&args[0])),
            _ => None,
        };
        res.ok_or_else(|| HolrsError::TypeCheckError("domain_type".to_string()))
    }

    /// Given a type of form a => b, return b.
    pub fn range_type(&self) -> HolrsResult<Rc<Type>> {
        let res = match self {
            Type::TConst(_, args) => Some(rc::Rc::clone(&args[1])),
            _ => None,
        };
        res.ok_or_else(|| HolrsError::TypeCheckError("range_type".to_string()))
    }

    /// Returns the size of the type.
    pub fn size(&self) -> usize {
        match self {
            Type::TVar(_) | Type::STVar(_) => 1,
            Type::TConst(_, args) => {
                let mut res = 1usize;
                for x in args {
                    res += x.size();
                }
                res
            }
        }
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

    fn get_args(&self) -> Option<&Vec<Rc<Type>>> {
        match self {
            Type::TConst(_, args) => Some(args),
            _ => None,
        }
    }

    fn as_tconst(&self) -> Option<(&String, &Vec<Rc<Type>>)> {
        match self {
            Type::TConst(name, args) => Some((name, args)),
            _ => None,
        }
    }

    fn get_name(&self) -> &String {
        match self {
            Type::STVar(name) | Type::TVar(name) | Type::TConst(name, _) => name,
        }
    }
}

impl Rc<Type> {
    /// Given a type of form a_1 => ... => a_n, b, return the pair
    /// [a_1, ... a_n], b.
    pub fn strip_type(&self) -> HolrsResult<(Vec<Rc<Type>>, Rc<Type>)> {
        if self.is_fun() {
            let (domain, range) = (self.domain_type()?, self.range_type()?);
            let (mut domains, final_range) = range.strip_type()?;
            domains.push(domain);
            Ok((domains, final_range))
        } else {
            return Ok((Vec::new(), Rc::clone(self)));
        }
    }

    /// Simultaneously substitute for the type variables using tyinst.
    /// Parameters
    /// ==========
    /// tyinst : TyInst
    ///     Type instantiation to be substituted.
    pub fn subst_with_pool(
        &self,
        tyinst: &HashMap<String, Rc<Type>>,
        // push_cache: &dyn Fn(Type) -> Rc<Type>,
        cache: &mut TermPool,
    ) -> Rc<Type> {
        match self.as_ref() {
            Type::STVar(name) => {
                if let Some(value) = tyinst.get(name) {
                    Rc::clone(value)
                } else {
                    Rc::clone(self)
                }
            }
            Type::TVar(_) => Rc::clone(self),
            Type::TConst(name, args) => {
                let substituted_args = args
                    .iter()
                    .map(|ty| ty.subst_with_pool(&tyinst, cache))
                    .collect();
                let ty = Type::new_tconst(name, substituted_args);
                Rc::clone(&cache.add_type(ty))
            }
        }
    }

    pub fn subst(
        &self,
        tyinst: &HashMap<String, Rc<Type>>,
        push_cache: &dyn Fn(Type) -> Rc<Type>,
    ) -> Rc<Type> {
        match self.as_ref() {
            Type::STVar(name) => {
                if let Some(value) = tyinst.get(name) {
                    Rc::clone(value)
                } else {
                    Rc::clone(self)
                }
            }
            Type::TVar(_) => Rc::clone(self),
            Type::TConst(name, args) => {
                let substituted_args = args
                    .iter()
                    .map(|ty| ty.subst(&tyinst, push_cache))
                    .collect();
                let ty = Type::new_tconst(name, substituted_args);
                Rc::clone(&push_cache(ty))
            }
        }
    }

    fn match_incr(&self, T: &Rc<Type>, tyinst: &mut HashMap<String, Rc<Type>>) -> HolrsResult<()> {
        if self.is_stvar() {
            // todo calculate hashval twice?
            if tyinst.contains_key(self.get_name()) {
                let value = tyinst.get(self.get_name()).unwrap();
                if value != T {
                    Err(HolrsError::TypeMatchError(format!(
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
                Err(HolrsError::TypeMatchError(format!(
                    "Unable to match {} with {}",
                    self, T,
                )))
            } else {
                Ok(())
            }
        } else if self.is_tconst() {
            if !T.is_tconst() || T.get_name() != self.get_name() {
                Err(HolrsError::TypeMatchError(format!(
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
            Err(HolrsError::TypeError("".to_string()))
        }
    }

    pub fn _match(&self, ty: &Rc<Type>) -> HolrsResult<HashMap<String, Rc<Type>>> {
        let mut res: HashMap<String, Rc<Type>> = HashMap::new();
        self.match_incr(&ty, &mut res)?;
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

    pub fn convert_stvar(
        &self,
        push_type_cache: &dyn Fn(Type) -> Rc<Type>,
    ) -> HolrsResult<Rc<Type>> {
        match self.as_ref() {
            Type::STVar(_) => Err(HolrsError::TypeError("convert_stvar".to_string())),
            Type::TVar(name) => {
                let svar = Type::new_stvar(name.clone());
                Ok(push_type_cache(svar))
            }
            Type::TConst(name, args) => {
                let mut converted_args: Vec<Rc<Type>> = vec![];
                for arg in args {
                    converted_args.push(arg.convert_stvar(push_type_cache)?);
                }
                let tconst = Type::new_tconst(name.clone(), converted_args);
                Ok(push_type_cache(tconst))
            }
        }
    }
}

// A marker trait that allows the type to be used as keys in hash maps.
impl Eq for Rc<Type> {}

pub fn new_tfun(mut args: Vec<Rc<Type>>, cache: &mut TermPool) -> Rc<Type> {
    let mut res = args.pop().unwrap();
    for arg in args.iter().rev() {
        let name = "fun".to_string();
        let tconst = Type::new_tconst(name, vec![Rc::clone(arg), res]);
        res = cache.add_type(tconst);
    }
    res
}

#[cfg(test)]
mod tests {
    use super::{pool::TermPool, *};

    #[test]
    fn test_repr_type() {
        let mut pool = TermPool::new();
        let t_a = pool.add_type(Type::new_tvar("a"));
        let t_b = pool.add_type(Type::new_tvar("b"));

        let test_data = vec![
            (Rc::clone(&t_a), "TVar(a)"),
            (
                pool.add_type(Type::new_tconst("bool", Vec::new())),
                "TConst(bool, [])",
            ),
            (
                pool.add_type(Type::new_tconst("list", vec![Rc::clone(&t_a)])),
                "TConst(list, [TVar(a)])",
            ),
            (
                pool.add_type(Type::new_tconst(
                    "tree",
                    vec![Rc::clone(&t_a), Rc::clone(&t_b)],
                )),
                "TConst(tree, [TVar(a), TVar(b)])",
            ),
            (
                pool.add_type(Type::new_tconst(
                    "fun",
                    vec![Rc::clone(&t_a), Rc::clone(&t_b)],
                )),
                "TConst(fun, [TVar(a), TVar(b)])",
            ),
        ];
        for (ty, repr_ty) in test_data {
            assert_eq!(ty.repr().as_str(), repr_ty);
        }
    }

    #[test]
    fn test_size() {
        let mut pool = TermPool::new();
        let t_a = pool.add_type(Type::new_tvar("a"));
        let t_b = pool.add_type(Type::new_tvar("b"));
        let t_c = pool.add_type(Type::new_tvar("c"));
        let st_a = pool.add_type(Type::new_stvar("a"));
        let fun = pool.add_type(Type::new_tconst(
            "fun",
            vec![Rc::clone(&t_a), Rc::clone(&t_b)],
        ));
        let test_data = vec![
            (Rc::clone(&t_a), 1),
            (Rc::clone(&st_a), 1),
            (
                pool.add_type(Type::new_tconst(
                    "fun",
                    vec![Rc::clone(&t_b), Rc::clone(&t_c)],
                )),
                3,
            ),
            (pool.add_type(Type::new_tconst("bool", Vec::new())), 1),
            (
                pool.add_type(Type::new_tconst(
                    "fun",
                    vec![Rc::clone(&fun), Rc::clone(&st_a)],
                )),
                5,
            ),
        ];
        for (t, expected_size) in test_data {
            assert_eq!(t.size(), expected_size);
        }
    }

    #[test]
    fn test_subst() {
        let mut pool = TermPool::new();
        let t_a = pool.add_type(Type::new_tvar("a"));
        let t_b = pool.add_type(Type::new_tvar("b"));
        let st_a = pool.add_type(Type::new_stvar("a"));
        let st_b = pool.add_type(Type::new_stvar("b"));
        let test_data = vec![
            (Rc::clone(&st_a), Rc::clone(&t_b)),
            (Rc::clone(&st_b), Rc::clone(&t_a)),
            (
                Rc::clone(&new_tfun(
                    vec![Rc::clone(&st_a), Rc::clone(&t_b)],
                    &mut pool,
                )),
                Rc::clone(&new_tfun(vec![Rc::clone(&t_b), Rc::clone(&t_b)], &mut pool)),
            ),
            (
                Rc::clone(&new_tfun(
                    vec![Rc::clone(&st_a), Rc::clone(&st_b)],
                    &mut pool,
                )),
                Rc::clone(&new_tfun(vec![Rc::clone(&t_b), Rc::clone(&t_a)], &mut pool)),
            ),
            (
                pool.add_type(Type::new_tconst("list", vec![Rc::clone(&st_a)])),
                pool.add_type(Type::new_tconst("list", vec![Rc::clone(&t_b)])),
            ),
        ];
        let tyinst: HashMap<String, Rc<Type>> = HashMap::from_iter(vec![
            ("a".to_string(), Rc::clone(&t_b)),
            ("b".to_string(), Rc::clone(&t_a)),
        ]);
        for (t, res) in test_data {
            assert_eq!(t.subst_with_pool(&tyinst, &mut pool), res);
        }
    }

    #[test]
    fn test_match() {
        let mut pool = TermPool::new();
        let t_a = pool.add_type(Type::new_tvar("a"));
        let t_b = pool.add_type(Type::new_tvar("b"));
        let st_a = pool.add_type(Type::new_stvar("a"));
        let st_b = pool.add_type(Type::new_stvar("b"));
        let booltype = pool.add_type(Type::new_tconst("bool", Vec::new()));
        let test_data: Vec<(Rc<Type>, Rc<Type>, HashMap<String, Rc<Type>>)> = vec![
            (
                Rc::clone(&st_a),
                Rc::clone(&t_b),
                HashMap::from_iter(vec![("a".to_string(), Rc::clone(&t_b))]),
            ),
            (
                Rc::clone(&new_tfun(
                    vec![Rc::clone(&st_a), Rc::clone(&st_b)],
                    &mut pool,
                )),
                Rc::clone(&new_tfun(vec![Rc::clone(&t_b), Rc::clone(&t_a)], &mut pool)),
                HashMap::from_iter(vec![
                    ("a".to_string(), Rc::clone(&t_b)),
                    ("b".to_string(), Rc::clone(&t_a)),
                ]),
            ),
            (
                Rc::clone(&st_a),
                Rc::clone(&booltype),
                HashMap::from_iter(vec![("a".to_string(), Rc::clone(&booltype))]),
            ),
            (
                Rc::clone(&new_tfun(
                    vec![Rc::clone(&st_a), Rc::clone(&booltype)],
                    &mut pool,
                )),
                Rc::clone(&new_tfun(
                    vec![Rc::clone(&booltype), Rc::clone(&booltype)],
                    &mut pool,
                )),
                HashMap::from_iter(vec![("a".to_string(), Rc::clone(&booltype))]),
            ),
        ];
        for (pat, ty, tyinst) in test_data {
            assert_eq!(pat._match(&ty).unwrap(), tyinst);
        }
    }
}
