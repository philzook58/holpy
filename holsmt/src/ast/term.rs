use super::rc::Rc;
use super::Type;
use crate::push_type_to_cache;

use crate::ast::error::{HolrsError, HolrsResult};
use crate::ast::utils::get_variant_name;

use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    hash::Hash,
};

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
    pub fn is_abs(&self) -> bool {
        matches!(self, Term::Abs(_, _, _))
    }

    /// Return whether the term is a bound variable.
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

    /// Given a term f a b, return a.
    pub fn get_arg1(&self) -> Option<&Rc<Term>> {
        let (fun, _) = self.get_fun_and_arg().unwrap();
        let (_, arg) = fun.get_fun_and_arg().unwrap();
        Some(arg)
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

    pub fn as_bound(&self) -> Option<usize> {
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

    pub fn as_abs(&self) -> Option<(&String, &Rc<Type>, &Rc<Term>)> {
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

    pub fn as_comb(&self) -> Option<(&Rc<Term>, &Rc<Term>)> {
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

    /// Whether self is of form ~A.
    pub fn is_not(&self) -> bool {
        self.is_comb(Some("neg".to_string()), Some(1))
    }

    /// Whether self is of the form A --> B.
    pub fn is_implies(&self) -> bool {
        self.is_comb(Some("implies".to_string()), Some(2))
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

    /// Whether self is of the form A = B.
    pub fn is_equals(&self) -> bool {
        self.is_comb(Some("equals".to_string()), Some(2))
    }

    /// Whether self is of the form A <(=) B or A >(=) B
    pub fn is_compares(&self) -> bool {
        self.is_less() || self.is_less_eq() || self.is_greater() || self.is_greater_eq()
    }

    /// Whether self is of the form A = A.
    pub fn is_reflexive(&self) -> bool {
        let (_, arg) = self.get_fun_and_arg().unwrap();
        let arg1 = arg.get_arg1().unwrap();
        self.is_equals() && arg1.eq(arg)
    }

    /// Whether self is of the form _VAR v.
    pub fn is_VAR(&self) -> bool {
        let (_, arg) = self.get_fun_and_arg().unwrap();
        self.is_comb(Some("_VAR".to_string()), Some(1)) && arg.is_var()
    }

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
    /// Given a term f t1 t2 ... tn, returns f.
    // class property
    pub fn get_head(&self) -> Rc<Term> {
        let mut t = self;
        while let Term::Comb(fun, _) = t.as_ref() {
            t = fun;
        }
        Rc::clone(t)
    }

    pub fn is_constant(&self) -> bool {
        unimplemented!("is_constant")
    }

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
    pub fn get_type(&self, push_type_cache: &dyn Fn(Type) -> Rc<Type>) -> HolrsResult<Rc<Type>> {
        /// Helper function. bd_vars is the list of types of the bound variables.
        fn rec(
            t: &Rc<Term>,
            bd_vars: &mut Vec<Rc<Type>>,
            push_type_cache: &dyn Fn(Type) -> Rc<Type>,
        ) -> HolrsResult<Rc<Type>> {
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
                    Err(HolrsError::TypeCheckError(
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
                let tfun = Type::new_tconst("fun", args);
                let res = push_type_cache(tfun);
                return Ok(res);
            } else if t.is_bound() {
                let n = t.get_n().unwrap();
                if n >= bd_vars.len() {
                    Err(HolrsError::TypeCheckError("open term".to_string()))
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
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
        push_type_cache: &dyn Fn(Type) -> Rc<Type>,
    ) -> Rc<Term> {
        match self.as_ref() {
            Term::SVar(name, ty) => {
                let svar = Term::new_svar(name.clone(), ty.subst(tyinst, push_type_cache));
                push_term_cache(svar)
            }
            Term::Var(name, ty) => {
                let svar = Term::new_var(name.clone(), ty.subst(tyinst, push_type_cache));
                push_term_cache(svar)
            }
            Term::Const(name, ty) => {
                let svar = Term::new_const(name.clone(), ty.subst(tyinst, push_type_cache));
                push_term_cache(svar)
            }
            Term::Comb(fun, arg) => {
                let comb = Term::new_comb(
                    fun.subst_type(tyinst, push_term_cache, push_type_cache),
                    arg.subst_type(tyinst, push_term_cache, push_type_cache),
                );
                push_term_cache(comb)
            }
            Term::Abs(var_name, var_ty, body) => {
                let abs = Term::new_abs(
                    var_name.clone(),
                    var_ty.subst(tyinst, push_type_cache),
                    body.subst_type(tyinst, push_term_cache, push_type_cache),
                );
                push_term_cache(abs)
            }
            Term::Bound(_) => Rc::clone(self),
        }
    }

    /// Perform substitution on type variables.
    pub fn subst_type_inplace(&mut self, _tyinst: &HashMap<String, Rc<Type>>) {
        unimplemented!()
    }

    /// Perform substitution on term variables.
    /// Parameters
    /// ==========
    /// inst : Inst
    ///     Instantiation to be substituted.
    pub fn subst(
        &self,
        inst: HashMap<String, Rc<Term>>,
        mut tyinst: HashMap<String, Rc<Type>>,
        var_inst: HashMap<String, Rc<Term>>,
        abs_name_inst: HashMap<String, Rc<Term>>,
        push_type_cache: &dyn Fn(Type) -> Rc<Type>,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> HolrsResult<Rc<Term>> {
        let svars = self.get_svars();
        for v in svars {
            let (name, ty) = v.get_name_type().unwrap();
            let inst_ty = match inst.get(name) {
                Some(ty) => ty.get_type(push_type_cache),
                None => Err(HolrsError::TermError(format!(
                    "subst: type {} cannot match",
                    ty
                ))),
            }?;

            let result = ty.match_incr(&inst_ty, &mut tyinst);
            // convert TypeCheckError to TermError
            if result.is_err() {
                return Err(HolrsError::TermError(format!(
                    "subst: type {} cannot match {}",
                    ty, inst_ty
                )));
            }
        }

        // Cache for rec funtion
        let mut cache: HashSet<Rc<Term>> = HashSet::new();

        // Now apply substitution recursively
        fn rec(
            t: &Rc<Term>,
            cache: &mut HashSet<Rc<Term>>,
            inst: &HashMap<String, Rc<Term>>,
            var_inst: &HashMap<String, Rc<Term>>,
            abs_name_inst: &HashMap<String, Rc<Term>>,
            push_term_cache: &dyn Fn(Term) -> Rc<Term>,
            push_type_cache: &dyn Fn(Type) -> Rc<Type>,
        ) -> Rc<Term> {
            match t.as_ref() {
                Term::SVar(name, _) => match inst.get(name) {
                    Some(ret) => Rc::clone(ret),
                    None => Rc::clone(t),
                },
                Term::Var(name, _) => match var_inst.get(name) {
                    Some(ret) => Rc::clone(ret),
                    None => Rc::clone(t),
                },
                Term::Const { .. } | &Term::Bound { .. } => Rc::clone(t),
                _ => match cache.get(t) {
                    Some(ret) => Rc::clone(ret),
                    None => match t.as_ref() {
                        Term::Comb(fun, arg) => {
                            let fun_t = rec(
                                fun,
                                cache,
                                inst,
                                var_inst,
                                abs_name_inst,
                                push_term_cache,
                                push_type_cache,
                            );
                            let arg_t = rec(
                                arg,
                                cache,
                                inst,
                                var_inst,
                                abs_name_inst,
                                push_term_cache,
                                push_type_cache,
                            );
                            let res: Rc<Term> = match fun_t.eq(fun) && arg_t.eq(arg) {
                                true => Rc::clone(t),
                                false => {
                                    let comb = Term::new_comb(fun_t, arg_t);
                                    push_term_cache(comb)
                                }
                            };
                            cache.insert(Rc::clone(&res));
                            // cache.insert(res.as_ref().clone(), Rc::clone(&res));
                            res
                        }
                        Term::Abs(var_name, var_ty, body) => {
                            let new_var_name = match abs_name_inst.get(var_name) {
                                Some(ret) => ret.get_name_type().unwrap().0.clone(),
                                None => var_name.clone(),
                            };
                            let body_t = rec(
                                body,
                                cache,
                                inst,
                                var_inst,
                                abs_name_inst,
                                push_term_cache,
                                push_type_cache,
                            );
                            let res = match body_t.eq(body) && new_var_name.eq(var_name) {
                                true => Rc::clone(t),
                                false => {
                                    let abs =
                                        Term::new_abs(new_var_name, Rc::clone(var_ty), body_t);
                                    push_term_cache(abs)
                                }
                            };
                            cache.insert(Rc::clone(&res));
                            // cache.insert(res.as_ref().clone(), Rc::clone(&res));
                            res
                        }
                        _ => unreachable!(),
                    },
                },
            }
        }

        let mut t = Rc::clone(self);
        if !tyinst.is_empty() {
            t = t.subst_type(&tyinst, push_term_cache, push_type_cache);
        }
        Ok(rec(
            &t,
            &mut cache,
            &inst,
            &var_inst,
            &abs_name_inst,
            push_term_cache,
            push_type_cache,
        ))
    }

    /// Given a term f t1 t2 ... tn, returns (f, [t1, t2, ..., tn]).
    pub fn strip_comb(&self) -> (Rc<Term>, Vec<Rc<Term>>) {
        let mut t = self;
        let mut args = vec![];
        while t.is_comb(None, None) {
            let (fun, arg) = t.get_fun_and_arg().unwrap();
            args.push(Rc::clone(arg));
            t = fun;
        }
        let args_reverse = args.iter().rev().map(|x| Rc::clone(x)).collect();
        (Rc::clone(t), args_reverse)
    }

    /// Given a term !x1 x2 ... xn. body, returns ([x1, x2, ..., xn], body)
    pub fn strip_forall(
        &self,
        num: Option<i32>,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> (Vec<Rc<Term>>, Rc<Term>) {
        let mut args = vec![];
        let mut t = Rc::clone(self);
        // todo
        let mut local_num = num.unwrap_or(i32::MAX); // Set to a large value
        while t.is_forall() && local_num > 0 {
            let body = t.get_fun_and_arg().unwrap().1;
            let (var_name, var_ty, _) = body.get_varname_varty_body().unwrap();
            let v = Term::new_var(var_name.clone(), Rc::clone(var_ty));
            let v_ref = push_term_cache(v);
            args.push(Rc::clone(&v_ref));
            t = body.subst_bound(&v_ref, push_term_cache).unwrap();
            local_num -= 1;
        }
        (args, t)
    }

    /// Given a term !x1 x2 ... xn. body, returns ([x1, x2, ..., xn], body)
    pub fn strip_exists(
        &self,
        num: Option<i32>,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> (Vec<Rc<Term>>, Rc<Term>) {
        let mut args = vec![];
        let mut t = Rc::clone(self);
        // todo
        let mut local_num = num.unwrap_or(i32::MAX); // Set to a large value
        while t.is_exists() && local_num > 0 {
            let body = t.get_fun_and_arg().unwrap().1;
            let (var_name, var_ty, _) = body.get_varname_varty_body().unwrap();
            let v = Term::new_var(var_name.clone(), Rc::clone(var_ty));
            let v_ref = push_term_cache(v);
            args.push(Rc::clone(&v_ref));
            t = body.subst_bound(&v_ref, push_term_cache).unwrap();
            local_num -= 1;
        }
        (args, t)
    }

    pub fn strip_quant(
        &self,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> (Vec<Rc<Term>>, Rc<Term>) {
        // todo, performance
        fn helper(
            tm: &Rc<Term>,
            quant_vars: &mut Vec<Rc<Term>>,
            push_term_cache: &dyn Fn(Term) -> Rc<Term>,
        ) -> (Vec<Rc<Term>>, Rc<Term>) {
            if tm.is_forall() {
                let (vars, bd) = tm.strip_forall(None, push_term_cache);
                quant_vars.append(&mut vars.clone());
                helper(&bd, quant_vars, push_term_cache)
            } else if tm.is_exists() {
                let (vars, bd) = tm.strip_exists(None, push_term_cache);
                quant_vars.append(&mut vars.clone());
                helper(&bd, quant_vars, push_term_cache)
            } else {
                (quant_vars.clone(), Rc::clone(tm))
            }
        }

        let mut quant_vars = vec![];
        helper(self, &mut quant_vars, push_term_cache)
    }

    /// Given a term f t1 t2 ... tn, return the list [t1, ..., tn].
    // class property
    pub fn get_args(&self) -> Vec<Rc<Term>> {
        let (_, args) = self.strip_comb();
        args
    }

    /// Whether self is of the form f t1 t2
    pub fn is_binop(&self) -> bool {
        self.get_args().len() == 2
    }

    /// Given s1 --> ... --> sn --> t, return ([s1, ..., sn], t).
    pub fn strip_implies(&self) -> (Vec<Rc<Term>>, Rc<Term>) {
        // todo, preformance
        if self.is_implies() {
            let (mut rest, c) = self.get_fun_and_arg().unwrap().1.strip_implies();
            let arg1 = self.get_arg1().unwrap();
            let mut res = vec![Rc::clone(arg1)];
            res.append(&mut rest);
            (res, c)
        } else {
            (vec![], Rc::clone(self))
        }
    }

    /// Given s1 & ... & sn, return [s1, ..., sn].
    pub fn strip_conj(&self) -> Vec<Rc<Term>> {
        let mut t = self;
        let mut res: Vec<Rc<Term>> = Vec::new();
        while t.is_conj() {
            let t_arg1 = t.get_arg1().unwrap();
            res.push(Rc::clone(t_arg1));
            t = t.get_fun_and_arg().unwrap().1;
        }
        res.push(Rc::clone(t));
        res
    }

    /// Given s1 | ... | sn, return [s1, ..., sn].
    pub fn strip_disj(&self) -> Vec<Rc<Term>> {
        let mut t = self;
        let mut res: Vec<Rc<Term>> = Vec::new();
        while t.is_disj() {
            let t_arg1 = t.get_arg1().unwrap();
            res.push(Rc::clone(t_arg1));
            t = t.get_fun_and_arg().unwrap().1;
        }
        res.push(Rc::clone(t));
        res
    }

    /// Given self of form %x. body, return pair (x, body).
    ///
    /// If var_name is None, the name recorded in the abstraction is used
    /// as the suggested name. Otherwise var_name is used as suggested name.
    ///
    /// It is guaranteed that v does not repeat names with any variables
    /// in the body.
    pub fn dest_abs(
        &self,
        var_name: Option<String>,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> HolrsResult<(Rc<Term>, Rc<Term>)> {
        assert!(self.is_abs(), "dest_abs");
        let (abs_name, var_ty, body) = self.get_varname_varty_body().unwrap();
        let var_names: Vec<String> = body
            .get_vars()
            .iter()
            .map(|v| v.get_name_type().unwrap().0.clone())
            .collect();
        let var_name = match var_name {
            Some(var_name) => var_name,
            None => abs_name.clone(),
        };
        let nm = get_variant_name(&var_name, &var_names);
        let var = Term::new_var(nm, Rc::clone(var_ty));
        let var_ref = push_term_cache(var);
        let new_body = &self.subst_bound(&var_ref, push_term_cache)?;

        return Ok((var_ref, Rc::clone(new_body)));
    }

    /// Given a term of the form (let x = t in body), return (x, t, body)
    pub fn dest_let(
        &self,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> (Rc<Term>, Rc<Term>, Rc<Term>) {
        let (_, arg) = self.get_fun_and_arg().unwrap();
        let (x, body) = arg.dest_abs(None, push_term_cache).unwrap();
        let arg1 = self.get_arg1().unwrap();
        return (x, Rc::clone(arg1), body);
    }

    /// Given a term of the form
    ///
    /// let x1 = t1 ... xn = tn in body
    ///
    /// return the list of pairs (x1, t1), ... (xn, tn) together with body
    pub fn strip_let(
        &self,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> (Vec<(Rc<Term>, Rc<Term>)>, Rc<Term>) {
        let mut res_list: Vec<(Rc<Term>, Rc<Term>)> = Vec::new();
        let mut t = Rc::clone(self);
        while t.is_let() {
            let (_, arg) = t.get_fun_and_arg().unwrap();
            let (x, body) = arg.dest_abs(None, push_term_cache).unwrap();
            let t_arg1 = arg.get_arg1().unwrap();
            res_list.push((x, Rc::clone(t_arg1)));
            t = body;
        }
        (res_list, t)
    }

    /// Increase loose bound variables in self by inc.
    fn incr_boundvars(&self, inc: usize, push_term_cache: &dyn Fn(Term) -> Rc<Term>) -> Rc<Term> {
        // helper function
        fn rec(
            t: &Rc<Term>,
            lev: usize,
            inc: usize,
            push_term_cache: &dyn Fn(Term) -> Rc<Term>,
        ) -> Rc<Term> {
            match t.as_ref() {
                Term::SVar(_, _) | Term::Var(_, _) | Term::Const(_, _) => Rc::clone(t),
                Term::Comb(fun, arg) => {
                    let fun_t = rec(fun, lev, inc, push_term_cache);
                    let arg_t = rec(arg, lev, inc, push_term_cache);
                    if fun_t.eq(fun) && arg_t.eq(arg) {
                        Rc::clone(t)
                    } else {
                        let comb = Term::new_comb(fun_t, arg_t);
                        push_term_cache(comb)
                    }
                }
                Term::Abs(var_name, var_ty, body) => {
                    let body_t = rec(body, lev + 1, inc, push_term_cache);
                    if body_t.eq(body) {
                        Rc::clone(t)
                    } else {
                        let abs = Term::new_abs(var_name.clone(), Rc::clone(var_ty), body_t);
                        push_term_cache(abs)
                    }
                }
                Term::Bound(t_n) => {
                    if *t_n >= lev {
                        let bound = Term::new_bound(*t_n + inc);
                        push_term_cache(bound)
                    } else {
                        Rc::clone(t)
                    }
                }
            }
        }

        rec(self, 0, inc, push_term_cache)
    }

    /// Given an Abs(x,T,body), substitute x for t in the body. t should
    /// have type T.
    pub fn subst_bound(
        &self,
        t: &Rc<Term>,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> HolrsResult<Rc<Term>> {
        fn rec<'a>(
            s: &'a Rc<Term>,
            t: &Rc<Term>,
            n: usize,
            is_open: bool,
            local_cache: &mut HashMap<(&'a Rc<Term>, usize), Rc<Term>>,
            push_term_cache: &dyn Fn(Term) -> Rc<Term>,
        ) -> Rc<Term> {
            match s.as_ref() {
                Term::SVar(_, _) | Term::Var(_, _) | Term::Const(_, _) => Rc::clone(s),
                Term::Bound(s_n) => match s_n.cmp(&n) {
                    Ordering::Equal => {
                        if is_open {
                            t.incr_boundvars(n, push_term_cache)
                        } else {
                            Rc::clone(t)
                        }
                    }
                    Ordering::Less => Rc::clone(s),
                    Ordering::Greater => {
                        let bound = Term::new_bound(*s_n - 1);
                        push_term_cache(bound)
                    }
                },
                _ => {
                    let key = (s, n);
                    let value = local_cache.get(&key);
                    match value {
                        Some(value) => return Rc::clone(value),
                        None => {
                            let res = match s.as_ref() {
                                Term::Comb(fun, arg) => {
                                    let fun_s =
                                        rec(fun, t, n, is_open, local_cache, push_term_cache);
                                    let arg_s =
                                        rec(arg, t, n, is_open, local_cache, push_term_cache);
                                    if fun_s.eq(fun) && arg_s.eq(arg) {
                                        Rc::clone(s)
                                    } else {
                                        let comb = Term::new_comb(fun_s, arg_s);
                                        push_term_cache(comb)
                                    }
                                }
                                Term::Abs(var_name, var_ty, body) => {
                                    let body_s =
                                        rec(body, t, n + 1, is_open, local_cache, push_term_cache);
                                    if body_s.eq(body) {
                                        Rc::clone(s)
                                    } else {
                                        let abs = Term::new_abs(
                                            var_name.clone(),
                                            Rc::clone(var_ty),
                                            body_s,
                                        );
                                        push_term_cache(abs)
                                    }
                                }
                                _ => unreachable!(),
                            };
                            local_cache.insert(key, Rc::clone(&res));
                            return res;
                        }
                    }
                }
            }
        }

        let is_open = self.is_open();
        let mut local_cache = HashMap::new();

        match self.as_ref() {
            Term::Abs(_, _, body) => {
                Ok(rec(body, t, 0, is_open, &mut local_cache, push_term_cache))
            }
            _ => Err(HolrsError::TermError(
                "subst_bound: input is not an abstraction.".to_string(),
            )),
        }
    }

    /// Beta-conversion: given a term of the form (%x. t1) t2, return the
    /// term t1[t2/x] which is beta-equivalent.
    pub fn beta_conv(&self, push_term_cache: &dyn Fn(Term) -> Rc<Term>) -> HolrsResult<Rc<Term>> {
        match self.as_ref() {
            Term::Comb(fun, arg) => {
                if fun.is_abs() {
                    return fun.subst_bound(arg, push_term_cache);
                } else {
                    return Err(HolrsError::TermError(
                        "beta_conv: input is not in the form (%x. t1) t2.".to_string(),
                    ));
                }
            }
            _ => Err(HolrsError::TermError(
                "beta_conv: input is not in the form (%x. t1) t2.".to_string(),
            )),
        }
    }

    /// Normalize self using beta-conversion.
    pub fn beta_norm(&self, push_term_cache: &dyn Fn(Term) -> Rc<Term>) -> HolrsResult<Rc<Term>> {
        match self.as_ref() {
            Term::SVar(_, _) | Term::Var(_, _) | Term::Const(_, _) | Term::Bound(_) => {
                Ok(Rc::clone(self))
            }
            Term::Comb(fun, arg) => {
                let f = fun.beta_norm(push_term_cache)?;
                let x = arg.beta_norm(push_term_cache)?;
                if f.is_abs() {
                    let comb = Term::new_comb(f, x);
                    let comb_ref = push_term_cache(comb);
                    comb_ref
                        .beta_conv(push_term_cache)?
                        .beta_norm(push_term_cache)
                } else {
                    let comb = Term::new_comb(f, x);
                    Ok(push_term_cache(comb))
                }
            }
            Term::Abs(var_name, var_ty, body) => {
                let abs = Term::new_abs(
                    var_name.clone(),
                    Rc::clone(var_ty),
                    body.beta_norm(push_term_cache)?,
                );
                Ok(push_term_cache(abs))
            }
        }
    }

    /// Substitute using the given instantiation, then normalize with
    /// respect to beta-conversion.
    pub fn subst_norm(&self, push_term_cache: &dyn Fn(Term) -> Rc<Term>) -> HolrsResult<Rc<Term>> {
        // return self.subst(HashMap::new()).beta_norm(push_term_cache);
        unimplemented!()
    }

    /// Whether the variable t occurs in self.
    pub fn occurs_var(&self, t: &Rc<Term>) -> bool {
        match self.as_ref() {
            Term::SVar(_, _) => false,
            Term::Var(_, _) => self.eq(t),
            Term::Const(_, _) => false,
            Term::Comb(fun, arg) => fun.occurs_var(t) || arg.occurs_var(t),
            Term::Abs(_, _, body) => body.occurs_var(t),
            Term::Bound(_) => false,
        }
    }

    /// Abstract over the variable t. The result is ready to become
    /// the body of an Abs term.
    pub fn abstract_over(
        &self,
        t: &Rc<Term>,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
    ) -> Result<Rc<Term>, HolrsError> {
        fn rec(
            s: &Rc<Term>,
            t: &Rc<Term>,
            n: usize,
            push_term_cache: &dyn Fn(Term) -> Rc<Term>,
        ) -> Result<Rc<Term>, HolrsError> {
            match s.as_ref() {
                Term::SVar(s_name, s_ty) => match t.as_ref() {
                    Term::SVar(t_name, t_ty) => {
                        if s_name == t_name {
                            let bound = Term::new_bound(n);
                            return Ok(push_term_cache(bound));
                        } else {
                            if !s_ty.eq(t_ty) {
                                return Err(HolrsError::TermError(
                                    "abstract_over: wrong type.".to_string(),
                                ));
                            } else {
                                return Ok(Rc::clone(s));
                            }
                        }
                    }
                    _ => Ok(Rc::clone(s)),
                },
                Term::Var(s_name, s_ty) => match t.as_ref() {
                    Term::Var(t_name, t_ty) => {
                        if s_name == t_name {
                            if !s_ty.eq(t_ty) {
                                return Err(HolrsError::TermError(
                                    "abstract_over: wrong type.".to_string(),
                                ));
                            } else {
                                let bound = Term::new_bound(n);
                                return Ok(push_term_cache(bound));
                            }
                        } else {
                            Ok(Rc::clone(s))
                        }
                    }
                    _ => Ok(Rc::clone(s)),
                },
                Term::Const(_, _) => Ok(Rc::clone(s)),
                Term::Comb(fun, arg) => {
                    let fun_s = rec(fun, t, n, push_term_cache)?;
                    let arg_s = rec(arg, t, n, push_term_cache)?;
                    if fun_s.eq(fun) && arg_s.eq(arg) {
                        return Ok(Rc::clone(s));
                    } else {
                        let comb = Term::new_comb(fun_s, arg_s);
                        return Ok(push_term_cache(comb));
                    }
                }
                Term::Abs(var_name, var_ty, body) => {
                    let body_s = rec(body, t, n + 1, push_term_cache)?;
                    if body_s.eq(body) {
                        return Ok(Rc::clone(s));
                    } else {
                        let abs = Term::new_abs(var_name.clone(), Rc::clone(var_ty), body_s);
                        return Ok(push_term_cache(abs));
                    }
                }
                Term::Bound(_) => Ok(Rc::clone(s)),
            }
        }

        if t.is_var() || t.is_svar() {
            return rec(self, t, 0, push_term_cache);
        } else {
            return Err(HolrsError::TermError(
                "abstract_over: t is not a variable.".to_string(),
            ));
        }
    }

    /// Perform type-checking and return the type of self.
    pub fn checked_get_type(
        &self,
        push_type_cache: &dyn Fn(Type) -> Rc<Type>,
    ) -> HolrsResult<Rc<Type>> {
        // help function
        fn rec(
            t: &Rc<Term>,
            bd_vars: &mut Vec<Rc<Type>>,
            push_type_cache: &dyn Fn(Type) -> Rc<Type>,
        ) -> HolrsResult<Rc<Type>> {
            match t.as_ref() {
                Term::SVar(_, ty) | Term::Var(_, ty) | Term::Const(_, ty) => Ok(Rc::clone(ty)),
                Term::Comb(fun, arg) => {
                    let fun_ty = rec(fun, bd_vars, push_type_cache)?;
                    let arg_ty = rec(arg, bd_vars, push_type_cache)?;
                    if !fun_ty.is_fun() {
                        return Err(HolrsError::TypeCheckError(
                            "function type expected in application".to_string(),
                        ));
                    } else if !fun_ty.domain_type()?.eq(&arg_ty) {
                        return Err(HolrsError::TypeCheckError(format!(
                            "type mismatch in application. Expected {}. Got {}",
                            fun_ty.domain_type()?,
                            arg_ty
                        )));
                    } else {
                        return fun_ty.range_type();
                    }
                }
                Term::Abs(_, var_ty, body) => {
                    let mut new_bd_vars = vec![Rc::clone(var_ty)];
                    new_bd_vars.append(&mut bd_vars.clone());
                    let args = vec![
                        Rc::clone(var_ty),
                        rec(body, &mut new_bd_vars, push_type_cache)?,
                    ];
                    let tfun = Type::new_tconst("fun", args);
                    Ok(push_type_to_cache(tfun))
                }
                Term::Bound(n) => {
                    if *n >= bd_vars.len() {
                        Err(HolrsError::TypeCheckError("open term".to_string()))
                    } else {
                        Ok(Rc::clone(&bd_vars[*n]))
                    }
                }
            }
        }

        let mut vec_vars = vec![];
        rec(self, &mut vec_vars, push_type_cache)
    }

    pub fn convert_svar(
        &self,
        push_term_cache: &dyn Fn(Term) -> Rc<Term>,
        push_type_cache: &dyn Fn(Type) -> Rc<Type>,
    ) -> HolrsResult<Rc<Term>> {
        match self.as_ref() {
            Term::SVar(_, _) => Err(HolrsError::TermError(
                "convert_svar: term already contains SVar.".to_string(),
            )),
            Term::Var(name, ty) => {
                let svar = Term::new_svar(name.clone(), ty.convert_stvar(push_type_cache)?);
                Ok(push_term_cache(svar))
            }
            Term::Const(name, ty) => {
                let _const = Term::new_const(name.clone(), ty.convert_stvar(push_type_cache)?);
                Ok(push_term_cache(_const))
            }
            Term::Comb(fun, arg) => {
                let comb = Term::new_comb(
                    fun.convert_svar(push_term_cache, push_type_cache)?,
                    arg.convert_svar(push_term_cache, push_type_cache)?,
                );
                Ok(push_term_cache(comb))
            }
            Term::Abs(var_name, var_ty, body) => {
                let abs = Term::new_abs(
                    var_name.clone(),
                    var_ty.convert_stvar(push_type_cache)?,
                    body.convert_svar(push_term_cache, push_type_cache)?,
                );
                Ok(push_term_cache(abs))
            }
            Term::Bound(_) => Ok(Rc::clone(self)),
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

#[cfg(test)]
mod term_tests {
    use super::*;
    use crate::ast::pool::TermPool;

    #[test]
    fn test_checked_get_type() {
        // let mut pool = TermPool::new();
        // let t_a = pool.add_type(Type::new_tvar("a"));
        // let t_b = pool.add_type(Type::new_tvar("b"));
        // let t_ab = pool.add_type(Type::new_tconst(
        //     "fun",
        //     vec![Rc::clone(&t_a), Rc::clone(&t_b)],
        // ));
        // let t_aa = pool.add_type(Type::new_tconst(
        //     "fun",
        //     vec![Rc::clone(&t_a), Rc::clone(&t_a)],
        // ));
        // let t_aab = pool.add_type(Type::new_tconst(
        //     "fun",
        //     vec![Rc::clone(&t_a), Rc::clone(&t_a), Rc::clone(&t_b)],
        // ));
        // let a = pool.add_term(Term::new_var("a".to_string(), Rc::clone(&t_a)));
        // let c = pool.add_term(Term::new_const("c".to_string(), Rc::clone(&t_a)));
        // let b0 = pool.add_term(Term::new_bound(0));
        // let f = pool.add_term(Term::new_var("f".to_string(), Rc::clone(&t_ab)));
        // let f2 = pool.add_term(Term::new_var("f2".to_string(), Rc::clone(&t_aab)));
        // let g = pool.add_term(Term::new_var("g".to_string(), Rc::clone(&t_aa)));
        // // Ta = TVar("a")
        // // Tb = TVar("b")
        // // STa = STVar("a")
        // // STb = STVar("b")
        // // Taa = TFun(Ta, Ta)        # 'a => 'a
        // // Tab = TFun(Ta, Tb)        # 'a => 'b
        // // Taab = TFun(Ta, Ta, Tb)   # 'a => 'a => 'b
        // // a = Var("a", Ta)
        // // b = Var("b", Tb)
        // // c = Const("c", Ta)
        // // f = Var("f", Tab)     # f: 'a => 'b
        // // f2 = Var("f2", Taab)  # f2: 'a => 'a => 'b
        // // g = Var("g", Taa)     # g: 'a => 'a
        // // B0 = Bound(0)
        // // B1 = Bound(1)

        // let test_data = vec![
        //     (Rc::clone(&a), Rc::clone(&t_a)),
        //     (Rc::clone(&c), Rc::clone(&t_a)),
        //     (
        //         pool.add_term(Term::new_comb(Rc::clone(&f), Rc::clone(&a))),
        //         Rc::clone(&t_b),
        //     ),
        //     // (pool.add_term(Term::new_comb(Rc::clone(&f2), Rc::clone(&c))), Rc::clone(&t_b)),
        //     // (pool.add_term(Term::new_comb(Rc::clone(&f), Rc::clone(&b0))), Rc::clone(&t_b)),
        //     // (f(a), Tb),
        //     // (f2(a,a), Tb),
        //     // (f(g(a)), Tb),
        //     // (Comb(Abs("x", Ta, B0), a), Ta),
        // ];

        // for (term, ty) in test_data {
        //     assert_eq!(term.checked_get_type(), Ok(ty));
        // }
    }

    #[test]
    fn test_strip_disj() {
        let mut pool = TermPool::new();
        let t_a = pool.add_type(Type::new_tvar("a"));
        let t_b = pool.add_type(Type::new_tvar("b"));

        let a = pool.add_term(Term::new_var("a".to_string(), Rc::clone(&t_a)));
        let b = pool.add_term(Term::new_var("b".to_string(), Rc::clone(&t_b)));

        let test_data = vec![
            (Rc::clone(&a), vec![Rc::clone(&a)]),
            (Rc::clone(&b), vec![Rc::clone(&b)]),
        ];

        for (t, res) in test_data {
            assert_eq!(t.strip_disj(), res);
        }
    }
}
