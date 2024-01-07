use std::rc::Rc;

pub type Name = Rc<String>;

#[derive(Clone, Debug)]
pub enum Ast {
    Variable(Name),
    Lambda(Name, Box<Ast>),
    Application(Box<Ast>, Box<Ast>),
    Pi(Name, Box<Ast>, Box<Ast>),
    Ann(Box<Ast>, Box<Ast>),
    Let(Name, Box<Ast>, Box<Ast>, Box<Ast>),
    Type,
}
