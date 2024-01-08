pub mod ast;

use core::fmt;
use std::{collections::HashMap, rc::Rc};

use ast::{Ast, Name};

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ast::Variable(x) => write!(f, "{x}"),
            Ast::Lambda(x, t) => write!(f, "(\\{x}. {t})"),
            Ast::Application(t, u) => write!(f, "({t} {u})"),
            Ast::Pi(x, a, b) => write!(f, "({x} : {a}) => {b}"),
            Ast::Ann(x, t) => write!(f, "{{{x} : {t}}}"),
            Ast::Let(bind, t, value, next) => {
                write!(f, "let {bind} : {t} = {value}; {next}")
            }
            Ast::Type => write!(f, "Type"),
        }
    }
}

pub type Type = Rc<Ast>;

#[derive(Clone)]
pub enum Value {
    Variable(Name),
    Application(Box<Value>, Box<Value>),
    Lambda(Name, Rc<dyn Fn(Value) -> Value>),
    Pi(Name, Box<Value>, Rc<dyn Fn(Value) -> Value>),
    Ann(Box<Value>, Box<Value>),
    Type,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variable(arg0) => {
                f.debug_tuple("Variable").field(arg0).finish()
            }
            Self::Application(arg0, arg1) => f
                .debug_tuple("Application")
                .field(arg0)
                .field(arg1)
                .finish(),
            Self::Lambda(arg0, _arg1) => {
                f.debug_tuple("Lambda").field(arg0).finish()
            }
            Self::Pi(arg0, arg1, _arg2) => {
                f.debug_tuple("Pi").field(arg0).field(arg1).finish()
            }
            Self::Ann(arg0, arg1) => {
                f.debug_tuple("Ann").field(arg0).field(arg1).finish()
            }
            Self::Type => write!(f, "Type"),
        }
    }
}

pub struct Env {
    pub variables: HashMap<Name, Rc<Value>>,
}

impl Ast {
    pub fn eval(self, env: HashMap<Name, Value>) -> Value {
        match self {
            Ast::Variable(x) => match env.get(&x).cloned() {
                Some(v) => v,
                None => panic!("Unbound variable '{x}'"),
            },
            Ast::Lambda(x, t) => Value::Lambda(
                x.clone(),
                Rc::new(move |u| {
                    let mut env = env.clone();
                    env.insert(x.clone(), u);
                    t.clone().eval(env)
                }),
            ),
            Ast::Application(t, u) => {
                match (t.eval(env.clone()), u.eval(env)) {
                    (Value::Lambda(_, t), u) => t(u),
                    (t, u) => Value::Application(Box::new(t), Box::new(u)),
                }
            }
            Ast::Pi(x, a, b) => Value::Pi(
                x.clone(),
                Box::new(a.eval(env.clone())),
                Rc::new(move |u| {
                    let mut env = env.clone();
                    env.insert(x.clone(), u);
                    b.clone().eval(env)
                }),
            ),
            Ast::Ann(x, t) => {
                Value::Ann(Box::new(x.eval(env.clone())), Box::new(t.eval(env)))
            }
            Ast::Let(bind, _t, value, next) => {
                let mut env = env.clone();
                env.insert(bind.clone(), value.eval(env.clone()));
                next.eval(env)
            }
            Ast::Type => Value::Type,
        }
    }
}

fn fresh(env: &HashMap<Name, Value>, name: Name) -> Name {
    if *name == "_" {
        return Name::new(String::from("_"));
    }
    match env.get(&name) {
        Some(..) => fresh(env, Rc::new(format!("{name}'"))),
        None => name,
    }
}

impl Value {
    pub fn quote(&self, env: &HashMap<Name, Value>) -> Ast {
        match self {
            Value::Variable(x) => Ast::Variable(x.clone()),
            Value::Application(t, u) => {
                let t = t.quote(env);
                let u = u.quote(env);
                Ast::Application(Box::new(t), Box::new(u))
            }
            Value::Lambda(x, t) => {
                let mut env = env.clone();
                let fresh = fresh(&env, x.clone());
                env.insert(x.clone(), Value::Variable(fresh));
                let t = t(Value::Variable(x.clone())).quote(&env);
                Ast::Lambda(x.clone(), Box::new(t))
            }
            Value::Pi(x, a, b) => {
                let a = a.quote(&env);
                let fresh = fresh(&env, x.clone());
                let mut env = env.clone();
                env.insert(x.clone(), Value::Variable(fresh));
                let b = b(Value::Variable(x.clone())).quote(&env);
                Ast::Pi(x.clone(), Box::new(a), Box::new(b))
            }
            Value::Ann(_x, _t) => {
                todo!() // idk, unreachable?
            }
            Value::Type => Ast::Type,
        }
    }
}

pub type VType = Value;

fn conv(env: &HashMap<Name, Value>, t: Value, u: Value) -> bool {
    match (t, u) {
        (Value::Type, Value::Type) => true,
        (Value::Pi(x, a, b), Value::Pi(_y, c, d)) => {
            let mut env = env.clone();
            env.insert(x.clone(), Value::Variable(x.clone()));
            conv(&env, *a, *c)
                && conv(
                    &env,
                    b(Value::Variable(x.clone())),
                    d(Value::Variable(x.clone())),
                )
        }
        (Value::Lambda(x, t), Value::Lambda(_y, u)) => {
            let mut env = env.clone();
            env.insert(x.clone(), Value::Variable(x.clone()));
            conv(
                &env,
                t(Value::Variable(x.clone())),
                u(Value::Variable(x.clone())),
            )
        }
        (Value::Lambda(x, t), u) => {
            let mut env = env.clone();
            env.insert(x.clone(), Value::Variable(x.clone()));
            conv(
                &env,
                t(Value::Variable(x.clone())),
                Value::Application(
                    Box::new(u),
                    Box::new(Value::Variable(x.clone())),
                ),
            )
        }
        (u, Value::Lambda(x, t)) => {
            let mut env = env.clone();
            env.insert(x.clone(), Value::Variable(x.clone()));
            conv(
                &env,
                Value::Application(
                    Box::new(u),
                    Box::new(Value::Variable(x.clone())),
                ),
                t(Value::Variable(x.clone())),
            )
        }
        (Value::Variable(x), Value::Variable(y)) => x == y,
        (Value::Application(a, b), Value::Application(c, d)) => {
            conv(&env, *a, *c) && conv(env, *b, *d)
        }
        _ => false,
    }
}

impl Ast {
    pub fn check(
        &self,
        a: Value,
        env: &HashMap<Name, Value>,
        ctx: HashMap<Name, VType>,
    ) -> VType {
        match (self, a) {
            (Ast::Lambda(x, t), Value::Pi(x_, a, b)) => {
                let mut env = env.clone();
                let fresh = fresh(&env, x_);
                env.insert(x.clone(), Value::Variable(fresh.clone()));
                let mut ctx = ctx.clone();
                ctx.insert(x.clone(), *a.clone());
                t.check(b(Value::Variable(fresh)), &env, ctx)
            }
            (Ast::Let(bind, typ, value, next), a) => {
                typ.check(Value::Type, env, ctx.clone());
                let typ_ = typ.clone().eval(env.clone());
                value.check(typ_.clone(), env, ctx.clone());
                let mut env = env.clone();
                let mut ctx = ctx.clone();
                let t = value.clone().eval(env.clone());
                env.insert(bind.clone(), t);
                ctx.insert(bind.clone(), typ_);
                next.check(a, &env, ctx)
            }
            (t, u) => {
                let tt = t.clone().infer(env, ctx);
                if !conv(env, tt.clone(), u.clone()) {
                    panic!("type mismatch, expected {} but got {}", u.quote(env), tt.quote(env))
                } else {
                    tt
                }
            }
        }
    }

    pub fn infer(
        self,
        env: &HashMap<Name, Value>,
        ctx: HashMap<Name, VType>,
    ) -> VType {
        match self {
            Ast::Variable(x) => match ctx.get(&x) {
                Some(t) => t.clone(),
                None => panic!("Unbound variable '{x}'."),
            },
            Ast::Lambda(_x, _t) => panic!("Can't infer lambda."),
            Ast::Application(t, u) => {
                let tt = t.infer(env, ctx.clone());
                match tt {
                    VType::Pi(_, a, b) => {
                        u.check(*a, env, ctx);
                        return b(u.eval(env.clone()));
                    }
                    other => {
                        panic!("Expected a function type, but got: '{other:?}'.")
                    }
                }
            }
            Ast::Pi(x, typ, body) => {
                typ.check(Value::Type, env, ctx.clone());
                let mut env = env.clone();
                let mut ctx = ctx.clone();
                env.insert(x.clone(), Value::Variable(x.clone()));
                ctx.insert(x.clone(), typ.eval(env.clone()));
                body.check(Value::Type, &env, ctx);
                VType::Type
            }
            Ast::Ann(value, t) => {
                value.check(t.eval(env.clone()), env, ctx);
                VType::Type
            }
            Ast::Let(bind, t, value, next) => {
                t.check(Value::Type, env, ctx.clone());

                let tt = t.eval(env.clone());
                value.check(tt.clone(), env, ctx.clone());

                let mut env = env.clone();
                let mut ctx = ctx.clone();
                ctx.insert(bind.clone(), tt);
                env.insert(bind.clone(), value.eval(env.clone()));

                next.infer(&env, ctx)
            }
            Ast::Type => VType::Type,
        }
    }
}

fn main() {
    use lalrpop_util::lalrpop_mod;

    lalrpop_mod!(pub parser);

    let s = std::fs::read_to_string("./test.lamen").unwrap();
    match parser::TypeParser::new().parse(&s) {
        Ok(e) => {
            let env = HashMap::new();
            let ctx = HashMap::new();
            let inferred = e.infer(&env, ctx);
            let quoted = inferred.quote(&env);

            println!("ok, quoted: {quoted}")
        }
        Err(e) => println!("{e:?}"),
    }
}
