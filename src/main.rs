use rnix::ast::{self, AstToken};

mod parse_type;

// TODO: review memory usage & remove clones

/* Types */

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    String,
    Boolean,
    Function { param_ty: Box<Type>, ret: Box<Type> },

    AttributeSet { attributes: Vec<(String, Type)> },
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Integer => write!(f, "integer"),
            Type::String => write!(f, "string"),
            Type::Boolean => write!(f, "boolean"),
            Type::Function { param_ty, ret } => match &**param_ty {
                Type::Function { .. } => write!(f, "({}) -> {}", param_ty, ret),
                _ => write!(f, "{} -> {}", param_ty, ret),
            },
            Type::AttributeSet { attributes } => {
                let attributes = attributes
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{ {} }}", attributes)
            }
        }
    }
}

/* Expressions */

#[derive(Debug, Clone)]
enum Expr {
    StringLiteral(String),
    IntegerLiteral(i64),
    Lambda {
        param_id: String,
        param_ty: Type,
        body: Box<Expr>,
    },
    Identifier(String),
    AttributeSet {
        attributes: Vec<(String, Expr)>,
    },
    Select {
        attrset: Box<Expr>,
        attrname: String,
    },
    Annotated {
        expr: Box<Expr>,
        ty: Type,
    },
    Let {
        var_name: String,
        var_expr: Box<Expr>,
        body: Box<Expr>,
    },
    App {
        f: Box<Expr>,
        param: Box<Expr>,
    },
}

fn to_expr(expr: rnix::ast::Expr) -> Expr {
    let comment = comment_after(&expr.syntax());
    let expr = match expr {
        ast::Expr::Literal(x) => to_expr_literal(x),
        ast::Expr::Str(s) => to_expr_str(s),
        ast::Expr::Ident(i) => to_expr_id(i),
        ast::Expr::Lambda(l) => to_expr_lambda(l),
        ast::Expr::AttrSet(a) => to_expr_attrset(a),
        ast::Expr::Select(s) => to_expr_select(s),
        ast::Expr::LetIn(l) => to_expr_let(l),
        ast::Expr::Apply(a) => to_expr_app(a),
        _ => todo!("expr not handled: {:?}", expr),
    };

    match comment {
        Some(comment) => Expr::Annotated {
            expr: Box::new(expr),
            ty: parse_type::parse(comment),
        },
        None => expr,
    }
}

fn to_expr_literal(x: rnix::ast::Literal) -> Expr {
    match x.kind() {
        ast::LiteralKind::Integer(f) => Expr::IntegerLiteral(f.value().unwrap()),
        _ => todo!(),
    }
}

fn to_expr_str(s: rnix::ast::Str) -> Expr {
    let mut parts: Vec<String> = vec![];

    for part in s.parts() {
        match part {
            ast::InterpolPart::Literal(l) => {
                let part: String = l.syntax().text().to_string();
                parts.push(part);
            }
            _ => todo!("Cannot handle non literal string"),
        }
    }

    Expr::StringLiteral(parts.join(""))
}

fn to_expr_id(i: rnix::ast::Ident) -> Expr {
    Expr::Identifier(i.ident_token().unwrap().text().to_string())
}

fn to_expr_lambda(s: rnix::ast::Lambda) -> Expr {
    let param = s.param().unwrap();

    let ty_str = comment_after(&param.syntax()).expect("missing type annotation");
    let ty = parse_type::parse(ty_str);

    let param = match param {
        rnix::ast::Param::Pattern(_) => todo!("patterns are not supported in lambda"),
        rnix::ast::Param::IdentParam(p) => {
            p.ident().unwrap().ident_token().unwrap().text().to_string()
        }
    };

    let body = to_expr(s.body().unwrap());
    Expr::Lambda {
        param_id: param,
        param_ty: ty,
        body: Box::new(body),
    }
}

fn to_expr_attrset(a: rnix::ast::AttrSet) -> Expr {
    use rnix::ast::HasEntry;

    let attributes = a
        .entries()
        .map(|entry| match entry {
            rnix::ast::Entry::Inherit(_) => todo!("inherit patterns are not implemented"),
            rnix::ast::Entry::AttrpathValue(av) => (
                attrpath_str(av.attrpath().unwrap()),
                to_expr(av.value().unwrap()),
            ),
        })
        .collect();

    Expr::AttributeSet { attributes }
}

fn to_expr_let(l: rnix::ast::LetIn) -> Expr {
    use rnix::ast::HasEntry;

    let assignment: Vec<rnix::ast::Entry> = l.entries().collect();
    if assignment.len() != 1 {
        panic!("let in must have exactly one assignment");
    }

    let assignment = &assignment[0];

    let (var_name, var_expr) = match assignment {
        rnix::ast::Entry::Inherit(_) => todo!("inherit patterns are not implemented"),
        rnix::ast::Entry::AttrpathValue(av) => (
            attrpath_str(av.attrpath().unwrap()),
            to_expr(av.value().unwrap()),
        ),
    };

    let body = to_expr(l.body().unwrap());

    Expr::Let {
        var_name,
        var_expr: Box::new(var_expr),
        body: Box::new(body),
    }
}

fn attrpath_str(ap: rnix::ast::Attrpath) -> String {
    let attrs: Vec<rnix::ast::Attr> = ap.attrs().collect();
    if attrs.len() != 1 {
        todo!("can only handle attrpaths of length 1");
    }

    let attrpath = &attrs[0];

    match attrpath {
        rnix::ast::Attr::Ident(i) => i.ident_token().unwrap().text().to_string(),
        _ => todo!("Unhandled attrpath: {:?}", attrpath),
    }
}

fn to_expr_select(s: rnix::ast::Select) -> Expr {
    let expr = to_expr(s.expr().unwrap());
    let attrname = attrpath_str(s.attrpath().unwrap());

    Expr::Select {
        attrset: Box::new(expr),
        attrname: attrname.to_string(),
    }
}

fn to_expr_app(s: rnix::ast::Apply) -> Expr {
    let f = to_expr(s.lambda().unwrap());
    let param = to_expr(s.argument().unwrap());

    Expr::App {
        f: Box::new(f),
        param: Box::new(param),
    }
}

use rowan::ast::AstNode;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    for arg in args {
        let ast = rnix::Root::parse(&arg).ok().unwrap();
        let expr = ast.expr().unwrap();
        let expr = to_expr(expr);

        println!("AST: {:?}", expr);

        let synth = synthesize(&expr);

        println!("Type: {}", synth);
    }
}

/* Synthesizing */

struct Env(std::collections::HashMap<String, Type>);

impl Env {
    fn get(&self, id: &String) -> Option<&Type> {
        let Env(m) = self;
        m.get(id)
    }

    fn set(&self, id: String, ty: Type) -> Env {
        let Env(m) = self;
        let mut m = m.clone();
        let _ = m.insert(id, ty.clone());
        Env(m)
    }

    // The default nix env
    fn default() -> Env {
        let mut m = std::collections::HashMap::new();
        let _ = m.insert("true".to_string(), Type::Boolean);
        let _ = m.insert(
            "add".to_string(),
            Type::Function {
                param_ty: Box::new(Type::Integer),
                ret: Box::new(Type::Function {
                    param_ty: Box::new(Type::Integer),
                    ret: Box::new(Type::Integer),
                }),
            },
        );
        Env(m)
    }
}

fn synthesize(expr: &Expr) -> Type {
    let env = Env::default();
    synth(&env, &expr)
}

fn synth(env: &Env, expr: &Expr) -> Type {
    match expr {
        Expr::StringLiteral(_) => Type::String,
        Expr::IntegerLiteral(_) => Type::Integer,
        Expr::Identifier(i) => synth_identifier(env, i),
        Expr::AttributeSet { attributes } => synth_attrset(env, attributes.to_vec()),
        Expr::Lambda {
            param_id,
            param_ty,
            body,
        } => synth_lambda(env, param_id, param_ty, body),
        Expr::Select { attrset, attrname } => synth_select(env, attrset, attrname),
        Expr::Let {
            var_name,
            var_expr,
            body,
        } => synth_let(env, var_name, var_expr, body),
        Expr::App { f, param } => synth_app(env, f, param),
        _ => todo!(),
    }
}

fn synth_attrset(env: &Env, attributes: Vec<(String, Expr)>) -> Type {
    let synthed = attributes
        .into_iter()
        .map(|(attrname, expr)| (attrname, synth(env, &expr)))
        .collect();

    Type::AttributeSet {
        attributes: synthed,
    }
}

fn synth_identifier(env: &Env, id: &String) -> Type {
    env.get(&id)
        .expect(format!("Identifier not found: {:?}", id).as_str())
        .clone()
}

fn synth_lambda(env: &Env, param_id: &String, param_ty: &Type, body: &Expr) -> Type {
    let env = env.set(param_id.clone(), param_ty.clone());
    let ret = synth(&env, body);
    Type::Function {
        param_ty: Box::new(param_ty.clone()),
        ret: Box::new(ret),
    }
}

fn synth_select(env: &Env, expr: &Expr, param_id: &String) -> Type {
    let synthed = synth(env, expr);

    match synthed {
        Type::AttributeSet { attributes } => attributes
            .iter()
            .find_map(|(attrname, attr_ty)| {
                if attrname == param_id {
                    Some(attr_ty)
                } else {
                    None
                }
            })
            .expect(format!("No such attribute: {:?}", param_id).as_str())
            .clone(),
        _ => panic!("Can only select on attrsets"),
    }
}

fn synth_let(env: &Env, var_name: &String, var_expr: &Expr, body: &Expr) -> Type {
    let var_ty = synth(env, var_expr);
    let env = env.set(var_name.clone(), var_ty);
    synth(&env, body)
}

fn synth_app(env: &Env, f: &Expr, param: &Expr) -> Type {
    /* The type of 'f a' is basically the return type of 'f', after checking that
     * 'f' is indeed a function, and after checking that 'f's argument is of the
     * same type as 'a'. */
    let f_ty = synth(env, f);
    match f_ty {
        Type::Function {
            param_ty: expected_param_ty,
            ret,
        } => {
            check(env, param, expected_param_ty.as_ref());
            ret.as_ref().clone()
        }
        _ => panic!("Can only apply with function"),
    }
}

/* Checking */

fn check(env: &Env, expr: &Expr, ty: &Type) {
    let synthed = synth(env, expr);

    if synthed != *ty {
        panic!("Could not match types {} and {}", synthed, ty);
    }
}

/* rnix helpers */

use rnix::match_ast;

fn comment_after(node: &rnix::SyntaxNode) -> Option<String> {
    let foo = node
        .siblings_with_tokens(rowan::Direction::Next)
        // rowan always returns the first node for some reason
        .skip(1)
        .map_while(|element| match element {
            rnix::NodeOrToken::Token(token) => match_ast! {
                match token {
                    ast::Comment(it) => {
                        Some(Some(it))
                    },
                    ast::Whitespace(_) => Some(None),
                    _ => None,
                }
            },
            _ => None,
        })
        .find_map(|element| element)?;

    Some(foo.text().trim().to_string())
}
