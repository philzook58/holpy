use super::rc::Rc;
use super::Type;
use crate::ast::TypeError;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    /// Schematic variable with given name and type.
    /// name: SVar.0, T: SVar.1
    SVar(String, Rc<Type>),

    /// Variable with given name and type.
    /// name: Var.0, T: Var.1
    Var(String, Rc<Type>),

    /// Constant with given name and type.
    /// name: Const.0, T: Const.1
    Const(String, Rc<Type>),

    /// The function f applied to a, written as f a (of f(a))
    /// fun: Comb.0, arg: Comb.1
    /// fun: Term,  todo, fun, arg is term or type?
    Comb(Rc<Term>, Rc<Term>),

    /// Abstraction. x is the suggested name of the bound variable,
    /// and T is the type of the bound variable. body is the body
    /// of the abstraction.
    /// This is written as %x::T. body, where the type T is usually omitted.
    /// var_name: Abs.0, var_T: Abs.1, body: Abs.2
    Abs(String, Rc<Type>, Rc<Term>),

    /// Bound variable with de Bruijn index n.
    /// n: Bound.0
    Bound(usize),
}

impl Term {
    /// Constructs a new `Term::SVar`.
    pub fn new_svar(name: String, ty: Rc<Type>) -> Self {
        Self::SVar(name, ty)
    }

    /// Constructs a new `Term::Var`.
    pub fn new_var(name: String, ty: Rc<Type>) -> Self {
        Self::Var(name, ty)
    }

    /// Constructs a new `Term::Const`.
    pub fn new_const(name: String, ty: Rc<Type>) -> Self {
        Self::Const(name, ty)
    }

    /// Constructs a new `Term::Comb`.
    pub fn new_comb(fun: Rc<Term>, arg: Rc<Term>) -> Self {
        Self::Comb(fun, arg)
    }

    /// Constructs a new `Term::Abs`.
    pub fn new_abs(var_name: String, var_ty: Rc<Type>, body: Rc<Term>) -> Self {
        Self::Abs(var_name, var_ty, body)
    }

    /// Constructs a new `Term::Bound`.
    pub fn new_bound(n: usize) -> Self {
        Self::Bound(n)
    }

    /// Returns `true` if the term is a schematic variable.
    pub fn is_svar(&self) -> bool {
        matches!(self, Term::SVar(_, _))
    }

    /// Returns `true` if the term is a variable.
    pub fn is_var(&self) -> bool {
        matches!(self, Term::Var(_, _))
    }

    /// Return whether the term is a constant.
    ///
    /// `name`: Optional string. If given, test whether the term has that name.
    pub fn is_const(&self, name: Option<String>) -> bool {
        match name {
            Some(name) => matches!(self, Term::Const(name, _)),
            None => matches!(self, Term::Const(_, _)),
        }
    }

    /// Returns `true` if the term is a combinator.
    /// Return whether the term is a combination.
    ///
    /// `name`: optional str. If given, test whether the head of the term has that name.
    ///
    /// `nargs`: optional int. Must be given together with name. If given, test whether the
    /// head is applied to exactly that many arguments.
    pub fn is_comb(&self, name: Option<String>, nargs: Option<usize>) -> bool {
        if let Term::Comb(fun, _) = self {
            if let Some(name_val) = name {
                let mut t = fun;
                let mut count = 1;
                while let Term::Comb(t_fun, _) = &**t {
                    t = t_fun;
                    count += 1;
                }
                if let Term::Const(t_name, _) = &**t {
                    return t_name == &name_val && (nargs.is_none() || count == nargs.unwrap());
                }
                return false;
            } else {
                return true;
            }
        }
        false
    }

    /// Return whether the term is an abstraction.
    #[inline]
    pub fn is_abs(&self) -> bool {
        matches!(self, Term::Abs(_, _, _))
    }

    /// Return whether the term is a bound variable.
    #[inline]
    pub fn is_bound(&self) -> bool {
        matches!(self, Term::Bound(_))
    }

    /// Return the size of the term.
    // todo why return a TypeError?
    pub fn size(&self) -> usize {
        match self {
            Term::SVar(_, _) | Term::Var(_, _) | Term::Const(_, _) => 1,
            Term::Comb(fun, arg) => fun.size() + arg.size(),
            Term::Abs(_, _, body) => 1 + body.size(),
            Term::Bound(_) => 1,
        }
    }

    pub fn get_name_type(&self) -> Option<(&String, &Rc<Type>)> {
        match self {
            Term::SVar(name, ty) | Term::Var(name, ty) | Term::Const(name, ty) => Some((name, ty)),
            _ => None,
        }
    }

    /// Return the de Bruijn index of the bound variable.
    pub fn get_n(&self) -> Option<usize> {
        match self {
            Term::Bound(n) => Some(*n),
            _ => None,
        }
    }

    pub fn get_varname_varty_body(&self) -> Option<(&String, &Rc<Type>, &Rc<Term>)> {
        match self {
            Term::Abs(var_name, var_ty, body) => Some((var_name, var_ty, body)),
            _ => None,
        }
    }

    pub fn get_fun_and_arg(&self) -> Option<(&Rc<Term>, &Rc<Term>)> {
        match self {
            Term::Comb(fun, arg) => Some((fun, arg)),
            _ => None,
        }
    }

    /// Implement a `repr` method for the `Type` enum to get a string representation of the type.
    pub fn repr(&self) -> String {
        match self {
            Term::SVar(name, ty) => format!("SVar({}, {})", name, ty),
            Term::Var(name, ty) => format!("Var({}, {})", name, ty),
            Term::Const(name, ty) => format!("Const({}, {})", name, ty),
            Term::Comb(fun, arg) => format!("Comb({}, {})", fun.repr(), arg.repr()),
            Term::Abs(var_name, var_ty, body) => {
                format!("Abs({}, {}, {})", var_name, var_ty, body.repr())
            }
            Term::Bound(n) => format!("Bound({})", n),
        }
    }

    fn eq(&self, other: &Self) -> bool {
        // todo, type eq using deep_eq
        match (self, other) {
            (Term::SVar(name1, ty1), Term::SVar(name2, ty2)) => name1 == name2 && ty1 == ty2,
            (Term::Var(name1, ty1), Term::Var(name2, ty2)) => name1 == name2 && ty1 == ty2,
            (Term::Const(name1, ty1), Term::Const(name2, ty2)) => name1 == name2 && ty1 == ty2,
            (Term::Comb(fun1, arg1), Term::Comb(fun2, arg2)) => fun1 == fun2 && arg1 == arg2,
            (Term::Abs(_, var_ty1, body1), Term::Abs(_, var_ty2, body2)) => {
                var_ty1 == var_ty2 && body1 == body2
            }
            (Term::Bound(n1), Term::Bound(n2)) => n1 == n2,
            _ => false,
        }
    }

    /// Whether t is of the form A & B.
    pub fn is_conj(&self) -> bool {
        self.is_comb(Some("conj".to_string()), Some(2))
    }

    /// Whether t is of the form A | B.
    pub fn is_disj(&self) -> bool {
        self.is_comb(Some("disj".to_string()), Some(2))
    }

    /// Whether self is of the form !x. P x.
    pub fn is_forall(&self) -> bool {
        self.is_comb(Some("all".to_string()), Some(1))
    }

    /// Whether self is of the form ?x. P x.
    pub fn is_exists(&self) -> bool {
        self.is_comb(Some("exists".to_string()), Some(1))
    }

    /// Whether self is of the form (let x = t in body).
    pub fn is_let(&self) -> bool {
        self.is_comb(Some("let".to_string()), Some(2))
    }

    /// Whether self is of the form A = A.
    pub fn is_equal(&self) -> bool {
        self.is_comb(Some("equal".to_string()), Some(2))
    }

    /// Whether self is of the form _VAR v.
    pub fn is_VAR(&self) -> bool {
        let (_, arg) = self.get_fun_and_arg().unwrap();
        self.is_comb(Some("_VAR".to_string()), Some(1)) && arg.is_var()
    }

    // todo, get_variant_name
    pub fn dest_abs(&self, var_name: Option<String>) -> () {
        assert!(self.is_abs(), "dest_abs");
        let (_, _, body) = self.get_varname_varty_body().unwrap();
        let var_names: Vec<&String> = body
            .get_vars()
            .iter()
            .map(|v| v.get_name_type().unwrap().0)
            .collect();
        let var_name = match var_name {
            Some(var_name) => var_name,
            None => self.get_varname_varty_body().unwrap().0.clone(),
        };
    }

    // def dest_abs(self, var_name=None):
    // """Given self of form %x. body, return pair (x, body).

    // If var_name is None, the name recorded in the abstraction is used
    // as the suggested name. Otherwise var_name is used as suggested name.

    // It is guaranteed that v does not repeat names with any variables
    // in the body.

    // """
    // assert self.is_abs(), 'dest_abs'
    // var_names = [v.name for v in self.body.get_vars()]
    // if var_name is None:
    //     var_name = self.var_name
    // nm = name.get_variant_name(var_name, var_names)
    // v = Var(nm, self.var_T)
    // body = self.subst_bound(v)

    // return v, body

    /// Whether self is in standard binary form.
    ///
    /// Note binary form means no of_nat is applied.
    pub fn is_binary(&self) -> bool {
        if self.is_zero() || self.is_one() {
            true
        } else if self.is_comb(Some("bit0".to_string()), Some(1))
            || self.is_comb(Some("bit1".to_string()), Some(1))
        {
            let (_, arg) = self.get_fun_and_arg().unwrap();
            arg.is_binary()
        } else {
            false
        }
    }

    pub fn dest_binary(&self) {
        unimplemented!("dest_binary")
    }

    pub fn is_nat(&self) -> bool {
        let (_, ty) = self.get_name_type().unwrap();
        ty.is_nat()
    }

    pub fn is_int(&self) -> bool {
        let (_, ty) = self.get_name_type().unwrap();
        ty.is_int()
    }

    pub fn is_real(&self) -> bool {
        let (_, ty) = self.get_name_type().unwrap();
        ty.is_real()
    }

    pub fn is_zero(&self) -> bool {
        self.is_const(Some("zero".to_string()))
    }

    pub fn is_one(&self) -> bool {
        self.is_const(Some("one".to_string()))
    }

    pub fn is_plus(&self) -> bool {
        self.is_comb(Some("plus".to_string()), Some(2))
    }

    pub fn is_minus(&self) -> bool {
        self.is_comb(Some("minus".to_string()), Some(2))
    }

    pub fn is_uminus(&self) -> bool {
        self.is_comb(Some("uminus".to_string()), Some(1))
    }

    pub fn is_times(&self) -> bool {
        self.is_comb(Some("times".to_string()), Some(2))
    }

    pub fn is_divides(&self) -> bool {
        self.is_comb(Some("real_divide".to_string()), Some(2))
    }

    pub fn is_real_inverse(&self) -> bool {
        let (_, arg) = self.get_fun_and_arg().unwrap();
        let (_, ty) = arg.get_name_type().unwrap();
        // todo
        self.is_comb(Some("real_inverse".to_string()), Some(1)) && ty.is_real()
    }

    pub fn is_nat_power(&self) -> bool {
        let (_, arg) = self.get_fun_and_arg().unwrap();
        let (_, ty) = arg.get_name_type().unwrap();
        // todo
        self.is_comb(Some("power".to_string()), Some(2)) && ty.is_nat()
    }

    pub fn is_real_power(&self) -> bool {
        let (_, arg) = self.get_fun_and_arg().unwrap();
        let (_, ty) = arg.get_name_type().unwrap();
        // todo
        self.is_comb(Some("power".to_string()), Some(2)) && ty.is_real()
    }

    pub fn is_nat_number(&self) -> bool {
        let (_, arg) = self.get_fun_and_arg().unwrap();
        self.is_zero()
            || self.is_one()
            || (self.is_comb(Some("of_nat".to_string()), Some(1)) && arg.is_binary())
    }

    pub fn is_constant(&self) -> bool {
        unimplemented!()
    }

    pub fn is_less_eq(&self) -> bool {
        self.is_comb(Some("less_eq".to_string()), Some(2))
    }

    pub fn is_less(&self) -> bool {
        self.is_comb(Some("less".to_string()), Some(2))
    }

    pub fn is_greater_eq(&self) -> bool {
        self.is_comb(Some("greater_eq".to_string()), Some(2))
    }

    pub fn is_greater(&self) -> bool {
        self.is_comb(Some("greater".to_string()), Some(2))
    }
}

// A marker trait that allows the term to be used as keys in hash maps.
impl Eq for Rc<Term> {}

impl Rc<Term> {
    pub fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }

    // todo, add annotation
    pub fn get_absBindVar(&self) -> Vec<String> {
        let mut res = vec![];

        // todo, implement by iteratoring
        fn f(t: &Rc<Term>, res: &mut Vec<String>) {
            if t.is_abs() {
                let (var_name, _, _) = t.get_varname_varty_body().unwrap();
                res.push(var_name.clone());
            } else if t.is_comb(None, None) {
                let (fun, arg) = t.get_fun_and_arg().unwrap();
                f(fun, res);
                f(arg, res);
            } else {
                return;
            }
        }
        f(self, &mut res);
        res
    }

    /// Returns type of the term with minimal type checking.
    pub fn get_type(&self, push_type_cache: &fn(Type) -> Rc<Type>) -> Result<Rc<Type>, TypeError> {
        /// Helper function. bd_vars is the list of types of the bound variables.
        fn rec(
            t: &Rc<Term>,
            bd_vars: &mut Vec<Rc<Type>>,
            push_type_cache: &fn(Type) -> Rc<Type>,
        ) -> Result<Rc<Type>, TypeError> {
            if t.is_svar() || t.is_var() || t.is_const(None) {
                // todo
                let ty = match &**t {
                    Term::SVar(_, ty) | Term::Var(_, ty) | Term::Const(_, ty) => ty,
                    _ => unreachable!(),
                };
                Ok(Rc::clone(ty))
            } else if t.is_comb(None, None) {
                let (fun, _) = t.get_fun_and_arg().unwrap();
                let type_fun = rec(fun, bd_vars, push_type_cache)?;
                if type_fun.is_fun() {
                    return Ok(type_fun.range_type()?);
                } else {
                    Err(TypeError::Custom(
                        "function type expected in application".to_string(),
                    ))
                }
            } else if t.is_abs() {
                // todo, construct a TConst
                let (_, var_T, body) = t.get_varname_varty_body().unwrap();
                let mut new_bd_vars = vec![Rc::clone(var_T)];
                new_bd_vars.append(&mut bd_vars.clone());
                let args = vec![
                    Rc::clone(var_T),
                    rec(body, &mut new_bd_vars, push_type_cache)?,
                ];
                let tfun = Type::new_TConst("fun", args);
                let res = push_type_cache(tfun);
                return Ok(res);
            } else if t.is_bound() {
                let n = t.get_n().unwrap();
                if n >= bd_vars.len() {
                    Err(TypeError::Custom("open term".to_string()))
                } else {
                    Ok(Rc::clone(&bd_vars[n]))
                }
            } else {
                unimplemented!()
            }
        }
        let mut vec_vars = vec![];
        rec(self, &mut vec_vars, push_type_cache)
    }

    /// Whether t is an open term.
    pub fn is_open(&self) -> bool {
        fn rec(t: &Rc<Term>, n: usize) -> bool {
            if t.is_svar() || t.is_var() || t.is_const(None) {
                false
            } else if t.is_comb(None, None) {
                let (fun, arg) = t.get_fun_and_arg().unwrap();
                rec(fun, n) || rec(arg, n)
            } else if t.is_abs() {
                let (_, _, body) = t.get_varname_varty_body().unwrap();
                rec(body, n + 1)
            } else if t.is_bound() {
                let t_n = t.get_n().unwrap();
                t_n >= n
            } else {
                unreachable!()
            }
        }
        rec(self, 0)
    }

    /// Substitute the type ty for the schematic variable svar in the term t.
    pub fn subst_type(
        &self,
        tyinst: &HashMap<String, Rc<Type>>,
        push_term_cache: &fn(Term) -> Rc<Term>,
        push_type_cache: &fn(Type) -> Rc<Type>,
    ) -> Rc<Term> {
        if self.is_svar() {
            let (name, ty) = self.get_name_type().unwrap();
            let svar = Term::new_svar(name.clone(), ty.subst(&tyinst, &push_type_cache).unwrap());
            push_term_cache(svar)
        } else if self.is_var() {
            let (name, ty) = self.get_name_type().unwrap();
            let svar = Term::new_var(name.clone(), ty.subst(&tyinst, &push_type_cache).unwrap());
            push_term_cache(svar)
        } else if self.is_const(None) {
            let (name, ty) = self.get_name_type().unwrap();
            let svar = Term::new_const(name.clone(), ty.subst(&tyinst, &push_type_cache).unwrap());
            push_term_cache(svar)
        } else if self.is_comb(None, None) {
            let (fun, arg) = self.get_fun_and_arg().unwrap();
            let comb = Term::new_comb(
                fun.subst_type(tyinst, push_term_cache, push_type_cache),
                arg.subst_type(tyinst, push_term_cache, push_type_cache),
            );
            push_term_cache(comb)
        } else if self.is_abs() {
            let (var_name, var_T, body) = self.get_varname_varty_body().unwrap();
            let abs = Term::new_abs(
                var_name.clone(),
                var_T.subst(tyinst, &push_type_cache).unwrap(),
                body.subst_type(tyinst, push_term_cache, push_type_cache),
            );
            push_term_cache(abs)
        } else if self.is_bound() {
            return Rc::clone(self);
        } else {
            !unreachable!()
        }
    }

    /// Perform substitution on type variables.
    pub fn subst_type_inplace(&self, tyinst: &HashMap<String, Rc<Type>>) -> Rc<Term> {
        unimplemented!()
    }

    /// Perform substitution on term variables.
    /// Parameters
    /// ==========
    /// inst : Inst
    ///     Instantiation to be substituted.
    pub fn subst(&self, inst: HashMap<String, Rc<Term>>) -> Rc<Term> {
        // let svars = self.get_svars();
        // for svar in svars {
        //     let (name, ty) = svar.get_name_type().unwrap();
        //     if inst.contains_key(name) {
        //         let inst_T = inst.get(name).unwrap();
        //         let tyinst: inst.get_tyinst().unwrap();
        //         ty.match_incr(inst_T, tyinst)
        //     }
        // }

        // let cache = HashMap::new();
        // fn rec(&Rc<Term>) {

        // }
        unimplemented!()
    }

    /// Given a term f t1 t2 ... tn, returns (f, [t1, t2, ..., tn]).
    pub fn strip_comb(&self) -> (&Rc<Term>, Vec<Rc<Term>>) {
        let mut t = self;
        let mut args = vec![];
        while t.is_comb(None, None) {
            let (fun, arg) = t.get_fun_and_arg().unwrap();
            args.push(Rc::clone(arg));
            t = fun;
        }
        (t, args)
    }

    /// Given a term !x1 x2 ... xn. body, returns ([x1, x2, ..., xn], body)
    pub fn strip_forall(&self) -> (Vec<Rc<Term>>, &Rc<Term>) {
        unimplemented!()
    }

    pub fn subst_bound(&self, t: Rc<Term>) -> Rc<Term> {
        unimplemented!("subst_bound")
    }

    pub fn beta_conv(&self) -> Rc<Term> {
        unimplemented!("beta_conv")
    }

    /// Normalize self using beta-conversion.
    pub fn beta_norm(&self) -> Rc<Term> {
        // if self.is_svar() || self.is_var() || self.is_const(None) || self.is_bound() {
        //     Rc::clone(&self)
        // } else if self.is_comb(None, None) {
        //     let (fun, arg) = self.get_fun_and_arg().unwrap();
        //     let f = fun.beta_norm();
        //     let x = arg.beta_norm();
        //     if f.is_abs() {
        //         let (_, _, body) = f.get_varname_varty_body().unwrap();
        //         body.subst(&HashMap::new())
        //     } else {
        //         Term::new_comb(f, x)
        //     }
        // } else if self.is_abs() {
        //     let (_, _, body) = self.get_varname_varty_body().unwrap();
        //     Term::new_abs(
        //         "x".to_string(),
        //         Rc::new(Type::new_TVar("a".to_string())),
        //         body.beta_norm(),
        //     )
        // } else {
        //     unreachable!("beta_norm")
        // }
        unimplemented!("beta_norm")
    }

    /// Whether the variable t occurs in self.
    pub fn occurs_var(&self, t: &Rc<Term>) -> bool {
        if self.is_svar() {
            false
        } else if self.is_var() {
            // todo, need check
            self.eq(t)
        } else if self.is_const(None) {
            false
        } else if self.is_comb(None, None) {
            let (fun, arg) = self.get_fun_and_arg().unwrap();
            fun.occurs_var(t) || arg.occurs_var(t)
        } else if self.is_abs() {
            let (_, _, body) = self.get_varname_varty_body().unwrap();
            body.occurs_var(t)
        } else if self.is_bound() {
            false
        } else {
            unreachable!()
        }
    }

    /// Abstract over the variable t. The result is ready to become
    /// the body of an Abs term.
    pub fn abstract_over(&self, t: &Rc<Term>) -> Rc<Term> {
        unimplemented!()
    }

    /// Perform type-checking and return the type of self.
    pub fn checked_get_type(&self) -> Rc<Type> {
        unimplemented!()
    }

    pub fn convert_svar(
        &self,
        push_term_cache: &fn(Term) -> Rc<Term>,
    ) -> Result<Rc<Term>, TypeError> {
        if self.is_svar() {
            Err(TypeError::Custom(
                "convert_svar: term already contains SVar.".to_string(),
            ))
        } else if self.is_var() {
            let (name, ty) = self.get_name_type().unwrap();
            let svar = Term::new_svar(name.clone(), ty.convert_stvar()?);
            Ok(push_term_cache(svar))
        } else if self.is_const(None) {
            let (name, ty) = self.get_name_type().unwrap();
            let _const = Term::new_const(name.clone(), ty.convert_stvar()?);
            Ok(push_term_cache(_const))
        } else if self.is_comb(None, None) {
            let (fun, arg) = self.get_fun_and_arg().unwrap();
            let comb = Term::new_comb(
                fun.convert_svar(push_term_cache)?,
                arg.convert_svar(push_term_cache)?,
            );
            Ok(push_term_cache(comb))
        } else if self.is_abs() {
            let (var_name, var_T, body) = self.get_varname_varty_body().unwrap();
            let abs = Term::new_abs(
                var_name.clone(),
                var_T.convert_stvar()?,
                body.convert_svar(push_term_cache)?,
            );
            Ok(push_term_cache(abs))
        } else if self.is_bound() {
            Ok(Rc::clone(self))
        } else {
            Err(TypeError::Default)
        }
    }

    pub fn dest_number(&self) {
        unimplemented!()
    }

    /// Returns a vector of schematic variables (svars) contained in the term and its subterms.
    ///
    /// This function recursively traverses the provided term and its subterms,
    /// extracting schematic variables and adding them to the result vector.
    /// The extracted svars are deduplicated to ensure each svar appears only once in the result.
    ///
    /// # Returns
    ///
    /// A vector containing the schematic variables found in the term and its subterms.
    ///
    /// # Arguments
    ///
    /// - `self`: A reference to the `Term` on which the function is called.
    pub fn get_svars(&self) -> Vec<Rc<Term>> {
        let mut res: Vec<Rc<Term>> = Vec::new();
        let mut found: HashSet<Rc<Term>> = HashSet::new();

        fn rec(t: &Rc<Term>, res: &mut Vec<Rc<Term>>, found: &mut HashSet<Rc<Term>>) {
            if t.is_svar() {
                if !found.contains(t) {
                    res.push(Rc::clone(t));
                    found.insert(Rc::clone(t));
                }
            } else if t.is_comb(None, None) {
                let (fun, arg) = t.get_fun_and_arg().unwrap();
                rec(fun, res, found);
                rec(arg, res, found);
            } else if t.is_abs() {
                let (_, _, body) = t.get_varname_varty_body().unwrap();
                rec(body, res, found);
            } else {
                return;
            }
        }

        rec(&self, &mut res, &mut found);
        res
    }

    /// Returns a vector of variables contained in the term and its subterms.
    ///
    /// This function recursively traverses the provided term and its subterms,
    /// extracting variables and adding them to the result vector. The extracted
    /// variables are deduplicated to ensure each variable appears only once in the result.
    ///
    /// # Returns
    ///
    /// A vector containing the variables found in the term and its subterms.
    ///
    /// # Arguments
    ///
    /// - `self`: The `Term` on which the function is called. Consumes the term
    ///   since variables are not mutable.
    pub fn get_vars(&self) -> Vec<Rc<Term>> {
        let mut res: Vec<Rc<Term>> = Vec::new();
        let mut found: HashSet<Rc<Term>> = HashSet::new();

        /// A recursive helper function to traverse the term and its subterms for variables.
        ///
        /// If the term is a variable, it is added to the result vector. If the term is
        /// a combination or an abstraction, the function is recursively called on its
        /// subterms.
        ///
        /// # Arguments
        ///
        /// - `t`: The term to be traversed.
        /// - `res`: A mutable reference to the result vector.
        /// - `found`: A mutable reference to the set of found variables.
        fn rec(t: &Rc<Term>, res: &mut Vec<Rc<Term>>, found: &mut HashSet<Rc<Term>>) {
            if t.is_var() {
                if !found.contains(t) {
                    res.push(Rc::clone(t));
                    found.insert(Rc::clone(t));
                }
            } else if t.is_comb(None, None) {
                let (fun, arg) = t.get_fun_and_arg().unwrap();
                rec(fun, res, found);
                rec(arg, res, found);
            } else if t.is_abs() {
                let (_, _, body) = t.get_varname_varty_body().unwrap();
                rec(body, res, found);
            }
        }
        rec(&self, &mut res, &mut found);
        res
    }

    /// Returns a vector of constants contained in the term and its subterms.
    ///
    /// This function recursively traverses the provided term and its subterms,
    /// extracting constants and adding them to the result vector. The extracted
    /// constants are deduplicated to ensure each constant appears only once in the result.
    ///
    /// # Returns
    ///
    /// A vector containing the constants found in the term and its subterms.
    ///
    /// # Arguments
    ///
    /// - `self`: A reference to the `Term` on which the function is called.
    ///
    /// # Example
    ///
    /// ```
    /// let term = create_some_term(); // Create a term somehow
    /// let consts = term.get_consts();
    /// println!("{:?}", consts);
    /// ```
    pub fn get_consts(&self) -> Vec<Rc<Term>> {
        let mut res: Vec<Rc<Term>> = Vec::new();
        let mut found: HashSet<Rc<Term>> = HashSet::new();

        fn rec(t: &Rc<Term>, res: &mut Vec<Rc<Term>>, found: &mut HashSet<Rc<Term>>) {
            if t.is_const(None) {
                if !found.contains(t) {
                    res.push(Rc::clone(t));
                    found.insert(Rc::clone(t));
                }
            } else if t.is_comb(None, None) {
                let (fun, arg) = t.get_fun_and_arg().unwrap();
                rec(fun, res, found);
                rec(arg, res, found);
            } else if t.is_abs() {
                let (_, _, body) = t.get_varname_varty_body().unwrap();
                rec(body, res, found);
            } else {
                return;
            }
        }

        rec(self, &mut res, &mut found);
        res
    }

    /// Return whether self any variables.
    pub fn has_var(&self) -> bool {
        if self.is_var() {
            true
        } else if self.is_comb(None, None) {
            let (fun, arg) = self.get_fun_and_arg().unwrap();
            fun.has_var() || arg.has_var()
        } else if self.is_abs() {
            let (_, _, body) = self.get_varname_varty_body().unwrap();
            body.has_var()
        } else {
            false
        }
    }

    /// Return whether self contains any of the variables in vs.
    pub fn has_vars(&self, vs: &Vec<Rc<Term>>) -> bool {
        if self.is_var() {
            vs.contains(self)
        } else if self.is_comb(None, None) {
            let (fun, arg) = self.get_fun_and_arg().unwrap();
            fun.has_vars(vs) || arg.has_vars(vs)
        } else if self.is_abs() {
            let (_, _, body) = self.get_varname_varty_body().unwrap();
            body.has_vars(vs)
        } else {
            false
        }
    }

    /// Return the list of schematic type variables.
    pub fn get_stvars(&self) -> Vec<Rc<Type>> {
        let mut res: Vec<Rc<Type>> = Vec::new();

        fn rec(t: &Rc<Term>, res: &mut Vec<Rc<Type>>) {
            if t.is_var() || t.is_const(None) {
                let (_, ty) = t.get_name_type().unwrap();
                for stvar in ty.get_stvars() {
                    if !res.contains(&stvar) {
                        res.push(Rc::clone(&stvar));
                    }
                }
            } else if t.is_comb(None, None) {
                let (fun, arg) = t.get_fun_and_arg().unwrap();
                rec(fun, res);
                rec(arg, res);
            } else if t.is_abs() {
                let (_, var_T, body) = t.get_varname_varty_body().unwrap();
                for stvar in var_T.get_stvars() {
                    if !res.contains(&stvar) {
                        res.push(Rc::clone(&stvar));
                    }
                }
                rec(body, res);
            } else {
                return;
            }
        }

        rec(self, &mut res);
        res
    }
}
