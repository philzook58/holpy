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
    // todo 使用 Const{String, Rc<Type>} 这样的匿名结构体会不会更好一点
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

    /// Returns `true` if the term is a variable.
    pub fn is_var(&self) -> bool {
        matches!(self, Term::Var(_, _))
    }

    /// Returns `true` if the term is a constant.
    // todo Not all types have a "name" attribute.?
    pub fn is_const(&self, name: Option<String>) -> bool {
        // match self {
        //     Term::Const(_, _) => true,
        //     _ => match name {
        //         Some(name) => self == name,
        //         None => true,
        //     },
        // }
        true
    }

    /// Returns `true` if the term is a combinator.
    pub fn is_comb(&self) -> bool {
        matches!(self, Term::Comb(_, _))
    }
}

// A marker trait that allows the term to be used as keys in hash maps.
impl Eq for Rc<Term> {}
