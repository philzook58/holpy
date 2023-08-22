use crate::ast::{rc::Rc, term::Term, Type};
use crate::{ast, push_term_to_cache};
use crate::{
    push_type_to_cache,
    python::pytype::{PyType, TyInst},
};
use pyo3::prelude::*;
use pyo3::pyclass::CompareOp;
use pyo3::types::{PyDict, PyTuple};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[pyclass(unsendable, subclass)]
#[derive(Default)]
pub struct Inst {
    pub tyinst: TyInst,
    pub var_inst: HashMap<String, PyTerm>,
    pub abs_name_inst: HashMap<String, PyTerm>,
}

#[pymethods]
impl Inst {
    #[new]
    #[pyo3(signature = (*dict, **kwargs))]
    fn new(dict: &PyTuple, kwargs: Option<&PyDict>) -> Self {
        let mut data = HashMap::new();

        if !dict.is_empty() {
            let _dict = dict.get_item(0).unwrap().downcast::<PyDict>().unwrap();
            for (key, value) in _dict.iter() {
                if let Ok(key_str) = key.extract::<String>() {
                    if let Ok(value_type) = value.extract::<PyType>() {
                        data.insert(key_str, value_type);
                    }
                }
            }
            Self {
                tyinst: TyInst { data: data },
                ..Default::default()
            }
        } else if let Some(kwargs_dict) = kwargs {
            for (key, value) in kwargs_dict.iter() {
                if let Ok(key_str) = key.extract::<String>() {
                    if let Ok(value_type) = value.extract::<PyType>() {
                        data.insert(key_str, value_type);
                    }
                }
            }
            Self {
                tyinst: TyInst { data: data },
                ..Default::default()
            }
        } else {
            panic!("Invalid argument")
        }
    }

    fn __str__(&self) -> String {
        // def __str__(self):
        // res = ''
        // if self.tyinst:
        //     res = str(self.tyinst) + ', '
        // res += ', '.join('?%s := %s' % (nm, t) for nm, t in self.items())
        // res += ', '.join('%s := %s' % (nm, t) for nm, t in self.var_inst.items())
        // res += ', '.join('%s -> %s' % (nm, nm2) for nm, nm2 in self.abs_name_inst.items())
        // return res
        // let mut result = String::new();
        let mut res = String::new();

        if !self.tyinst.data.is_empty() {
            res = format!("{}, ", self.tyinst.__str__());
            res.push_str(", ");
        }

        // todo? self.items()
        res.push_str(
            self.var_inst
                .iter()
                .map(|(nm, t)| format!("{} := {}", nm, t.ptr))
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );
        res.push_str(
            self.abs_name_inst
                .iter()
                .map(|(nm, t)| format!("{} -> {}, ", nm, t.ptr))
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );

        res
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp) -> bool {
        match op {
            CompareOp::Eq => self.var_inst == other.var_inst,
            CompareOp::Ne => self.var_inst != other.var_inst,
            _ => self.var_inst == other.var_inst,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[pyclass(unsendable, name = "Term", subclass)]
pub struct PyTerm {
    ptr: ast::rc::Rc<Term>,
}

#[pymethods]
impl PyTerm {
    // todo, unneccessary?
    #[new]
    fn new(name: String, ty: PyType) -> Self {
        Self {
            ptr: ast::rc::Rc::new(Term::new_var(name, ty.ptr)),
        }
    }

    /// consider implementing __hash__ when implementing __richcmp__
    fn __hash__(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.ptr.hash(&mut hasher);
        hasher.finish()
    }

    /// There are some magic methods.
    fn __str__(&self) -> String {
        format!("{}", self.ptr)
    }

    fn __repr__(&self) -> String {
        self.ptr.repr()
    }

    #[pyo3(signature = (*args))]
    fn __call__(&self, args: &PyTuple) -> Self {
        let mut term_args = Vec::new();

        for arg in args {
            if let Ok(pyterm) = arg.extract::<PyTerm>() {
                term_args.push(ast::rc::Rc::clone(&pyterm.ptr))
            } else {
                // process error
                unimplemented!()
            }
        }

        // args always in CACHE
        let mut res = ast::rc::Rc::clone(&self.ptr);
        for arg in term_args {
            res = push_term_to_cache(Term::new_comb(res, arg));
        }
        PyTerm { ptr: res }
    }

    /// There are some magic methods. 0.19.3
    fn __richcmp__(&self, other: &Self, op: CompareOp) -> bool {
        match op {
            CompareOp::Eq => {
                if self.ptr == other.ptr {
                    return true;
                }

                self.ptr.eq(&other.ptr)
            }
            CompareOp::Ne => {
                if self.ptr != other.ptr {
                    return true;
                }

                !self.ptr.eq(&other.ptr)
            }
            _ => self.ptr == other.ptr,
        }
    }

    fn get_type(&self) -> PyResult<PyType> {
        // funciton ptr
        match self
            .ptr
            .get_type(&(push_type_to_cache as fn(Type) -> ast::rc::Rc<Type>))
        {
            Ok(res) => Ok(PyType {
                ptr: ast::rc::Rc::clone(&res),
            }),
            Err(error) => Err(error.into()),
        }
    }

    fn is_open(&self) -> bool {
        self.ptr.is_open()
    }

    #[pyo3(signature = (tyinst=None, **kwargs))]
    fn subst_type(&self, tyinst: Option<&TyInst>, kwargs: Option<&PyDict>) -> PyTerm {
        let subst_dict: HashMap<String, Rc<Type>> = match tyinst {
            Some(dict) => dict
                .data
                .iter()
                .map(|(k, v)| (k.clone(), v.ptr.clone()))
                .collect(),
            None => {
                let mut subst_dict = HashMap::new();
                if let Some(kwargs_dict) = kwargs {
                    for (key, value) in kwargs_dict.iter() {
                        if let Ok(key_str) = key.extract::<String>() {
                            if let Ok(value_type) = value.extract::<PyType>() {
                                subst_dict.insert(key_str, ast::rc::Rc::clone(&value_type.ptr));
                            }
                        }
                    }
                }
                subst_dict
            }
        };

        PyTerm {
            ptr: self.ptr.subst_type(
                &subst_dict,
                &(push_term_to_cache as fn(Term) -> ast::rc::Rc<Term>),
                &(push_type_to_cache as fn(Type) -> ast::rc::Rc<Type>),
            ),
        }
    }

    fn is_conj(&self) -> bool {
        self.ptr.is_conj()
    }

    fn is_disj(&self) -> bool {
        self.ptr.is_disj()
    }

    fn is_forall(&self) -> bool {
        self.ptr.is_forall()
    }

    fn is_exists(&self) -> bool {
        self.ptr.is_exists()
    }

    fn is_let(&self) -> bool {
        self.ptr.is_let()
    }

    fn is_equal(&self) -> bool {
        self.ptr.is_equal()
    }

    fn is_VAR(&self) -> bool {
        self.ptr.is_var()
    }

    fn is_less_eq(&self) -> bool {
        self.ptr.is_less_eq()
    }

    fn is_less(&self) -> bool {
        self.ptr.is_less()
    }

    fn is_greater_eq(&self) -> bool {
        self.ptr.is_greater_eq()
    }

    fn is_greater(&self) -> bool {
        self.ptr.is_greater()
    }

    fn has_var(&self) -> bool {
        self.ptr.has_var()
    }

    fn get_stvars(&self) -> Vec<PyType> {
        self.ptr
            .get_stvars()
            .into_iter()
            .map(|t| PyType { ptr: t })
            .collect()
    }

    fn get_consts(&self) -> Vec<PyTerm> {
        self.ptr
            .get_consts()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
    }

    fn get_vars(&self) -> Vec<PyTerm> {
        self.ptr
            .get_vars()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
    }

    fn get_svars(&self) -> Vec<PyTerm> {
        self.ptr
            .get_svars()
            .into_iter()
            .map(|t| PyTerm { ptr: t })
            .collect()
    }

    fn occurs_var(&self, var: &PyTerm) -> bool {
        self.ptr.occurs_var(&var.ptr)
    }
}

#[pyfunction]
pub fn SVar(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_svar(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction]
pub fn Var(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_var(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction]
pub fn Const(name: String, ty: PyType) -> PyTerm {
    let term = Term::new_const(name, ty.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

// todo, Rc::clone()?
#[pyfunction]
pub fn Comb(fun: PyTerm, arg: PyTerm) -> PyTerm {
    let term = Term::new_comb(fun.ptr, arg.ptr);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

#[pyfunction(signature = (*args))]
pub fn Abs(args: &PyTuple) -> PyTerm {
    if args.len() < 3 || args.len() % 2 == 0 {
        panic!("Invalid argument length");
    }
    let args_vec: Vec<&PyAny> = args.iter().collect();
    let mut args_iter = args_vec.iter().rev();
    let body = args_iter
        .next()
        .and_then(|arg| arg.extract::<PyTerm>().ok())
        .expect("Invalid argument: body");
    let mut current_term = ast::rc::Rc::clone(&body.ptr);

    loop {
        let var_ty = match args_iter
            .next()
            .and_then(|arg| arg.extract::<PyType>().ok())
        {
            Some(var_ty) => var_ty,
            None => break,
        };
        let var_name = match args_iter
            .next()
            .and_then(|arg| arg.extract::<String>().ok())
        {
            Some(var_name) => var_name,
            None => break,
        };

        let new_term = Term::Abs(var_name, var_ty.ptr, current_term);
        current_term = push_term_to_cache(new_term);
    }

    // println!("{:?}", current_term);
    PyTerm { ptr: current_term }
}

#[pyfunction]
pub fn Bound(n: usize) -> PyTerm {
    let term = Term::new_bound(n);
    let term_ref = push_term_to_cache(term);

    PyTerm { ptr: term_ref }
}

pub fn register_term_module(py: Python<'_>) -> PyResult<&PyModule> {
    let res = PyModule::new(py, "term")?;
    res.add_class::<PyTerm>()?;
    res.add_function(wrap_pyfunction!(SVar, res)?)?;
    res.add_function(wrap_pyfunction!(Var, res)?)?;
    res.add_function(wrap_pyfunction!(Const, res)?)?;
    res.add_function(wrap_pyfunction!(Comb, res)?)?;
    res.add_function(wrap_pyfunction!(Abs, res)?)?;
    res.add_function(wrap_pyfunction!(Bound, res)?)?;
    Ok(res)
}
