use super::rc::Rc;
use super::Type;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Term {
    /// Schematic variable with given name and type.
    SVar(String, Rc<Type>),
    ///  SVar { name: String, T: Rc<Type> },

    /// Variable with given name and type.
    Var(String, Rc<Type>),

    /// Constant with given name and type.
    Const(String, Rc<Type>),

    /// The function f applied to a, written as f a (of f(a))
    Comb(Rc<Term>, Rc<Term>),

    /// Abstraction. x is the suggested name of the bound variable,
    /// and T is the type of the bound variable. body is the body
    /// of the abstraction.
    /// This is written as %x::T. body, where the type T is usually omitted.
    Abs(Rc<Term>, Rc<Term>),

    /// Bound variable with de Bruijn index n.
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
    pub fn new_comb(f: Rc<Term>, a: Rc<Term>) -> Self {
        Self::Comb(f, a)
    }

    /// Constructs a new `Term::Abs`.
    pub fn new_abs(x: Rc<Term>, body: Rc<Term>) -> Self {
        Self::Abs(x, body)
    }

    /// Constructs a new `Term::Bound`.
    pub fn new_bound(n: usize) -> Self {
        Self::Bound(n)
    }

    /// Returns `true` if the term is a schematic variable.
    pub fn is_stvar(&self) -> bool {
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
        match self {
            Term::Const(_name, _) => match name {
                Some(name) => name.eq(_name),
                None => true,
            },
            _ => false,
        }
    }

    /// Returns `true` if the term is a combinator.
    /// Return whether the term is a combination.
    ///
    /// `name`: optional str. If given, test whether the head of the term has that name.
    ///
    /// `nargs`: optional int. Must be given together with name. If given, test whether the
    /// head is applied to exactly that many arguments.
    pub fn is_comb(&self) -> bool {
        // todo
        matches!(self, Term::Comb(_, _))
    }

    /// Return whether the term is an abstraction.
    pub fn is_abs(&self) -> bool {
        matches!(self, Term::Abs(_, _))
    }

    /// Return whether the term is a bound variable.
    pub fn is_bound(&self) -> bool {
        matches!(self, Term::Bound(_))
    }

    /// Return the size of the term.
    pub fn size(&self) -> usize {
        // todo, process typeerror is necessary?
        match self {
            Term::SVar(_, _) | Term::Var(_, _) | Term::Const(_, _) => 1,
            Term::Comb(f, a) => f.size() + a.size(),
            Term::Abs(_, body) => 1 + body.size(),
            Term::Bound(_) => 1,
        }
    }
}

// A marker trait that allows the term to be used as keys in hash maps.
impl Eq for Rc<Term> {}

impl Rc<Term> {}
