use crate::ast;
use crate::types;

/* Synthesizing */

struct Env(std::collections::HashMap<String, types::Type>);

impl Env {
    fn get(&self, id: &String) -> Option<&types::Type> {
        self.0.get(id)
    }

    fn set(&self, id: String, ty: types::Type) -> Env {
        let mut m = self.0.clone();
        let _ = m.insert(id, ty.clone());
        Env(m)
    }

    // The default nix env
    fn default() -> Env {
        let mut m = std::collections::HashMap::new();

        let mut insert = |name: &str, ty: &str| {
            let _ = m.insert(name.to_string(), ty.parse().unwrap());
        };
        insert("abort", "string -> never");
        insert("add", "integer -> integer -> integer");
        insert("baseNameOf", "string -> string");
        insert("bitAnd", "integer -> integer");
        insert("bitOr", "integer -> integer");
        insert("bitXor", "integer -> integer");
        insert("true", "bool");
        Env(m)
    }
}

#[test]
fn test_default_env_parses() {
    // We define types as strings in the default env (for simplicity and legibility) and this
    // just tests that types parse properly.
    let _ = Env::default();
}

pub fn synthesize(expr: &ast::Expr) -> types::Type {
    let env = Env::default();
    let (ty, cs) = synth(&env, &expr);

    if !cs.is_empty() {
        panic!("Leftover constraints: {:?}", cs);
    }

    ty
}

fn synth(env: &Env, expr: &ast::Expr) -> (types::Type, types::Constraints) {
    let no_constraints =
        |x: types::Type| (x, std::collections::HashMap::<String, types::Type>::new());
    match expr {
        ast::Expr::StringLiteral(_) => no_constraints(types::Type::String),
        ast::Expr::IntegerLiteral(_) => no_constraints(types::Type::Integer),
        ast::Expr::Identifier(i) => no_constraints(synth_identifier(env, i)),
        ast::Expr::AttributeSet { attributes } => synth_attrset(env, attributes.to_vec()),
        ast::Expr::Lambda {
            param_id,
            param_ty,
            body,
            quantifier,
        } => synth_lambda(env, quantifier, param_id, param_ty, body),
        ast::Expr::Select { attrset, attrname } => synth_select(env, attrset, attrname),
        ast::Expr::Let {
            var_name,
            var_expr,
            body,
        } => synth_let(env, var_name, var_expr, body),
        ast::Expr::App { f, param } => synth_app(env, f, param),
    }
}

fn synth_attrset(
    env: &Env,
    attributes: Vec<(String, ast::Expr)>,
) -> (types::Type, types::Constraints) {
    let (tys, css): (Vec<(String, types::Type)>, Vec<types::Constraints>) = attributes
        .into_iter()
        .map(|(attrname, expr)| {
            let (ty, cs) = synth(env, &expr);

            ((attrname, ty), cs)
        })
        .unzip();

    let mut cs = std::collections::HashMap::new();

    for cs_extra in css {
        cs.extend(cs_extra);
    }

    (types::Type::AttributeSet { attributes: tys }, cs)
}

fn synth_identifier(env: &Env, id: &String) -> types::Type {
    env.get(&id)
        .expect(format!("Identifier not found: {:?}", id).as_str())
        .clone()
}

fn synth_lambda(
    env: &Env,
    quantifier: &Option<String>,
    param_id: &String,
    param_ty: &types::Type,
    body: &ast::Expr,
) -> (types::Type, types::Constraints) {
    let env = env.set(param_id.clone(), param_ty.clone());
    let (ty, cs) = synth(&env, body);
    let ty = types::Type::Function {
        param_ty: Box::new(param_ty.clone()),
        ret: Box::new(ty),
        quantifier: quantifier.clone(),
    };

    (ty, cs)
}

fn synth_select(
    env: &Env,
    expr: &ast::Expr,
    param_id: &String,
) -> (types::Type, types::Constraints) {
    let (synthed, cs) = synth(env, expr);

    let res = match synthed {
        types::Type::AttributeSet { attributes } => attributes
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
    };

    (res, cs)
}

fn synth_let(
    env: &Env,
    var_name: &String,
    var_expr: &ast::Expr,
    body: &ast::Expr,
) -> (types::Type, types::Constraints) {
    let (var_ty, mut cs) = synth(env, var_expr);
    let env = env.set(var_name.clone(), var_ty);
    let (ty, cs_extra) = synth(&env, body);
    cs.extend(cs_extra);
    (ty, cs)
}

fn synth_app(env: &Env, f: &ast::Expr, param: &ast::Expr) -> (types::Type, types::Constraints) {
    /* The type of 'f a' is basically the return type of 'f', after checking that
     * 'f' is indeed a function, and after checking that 'f's argument is of the
     * same type as 'a'. */
    let (f_ty, mut cs) = synth(env, f);
    match f_ty {
        types::Type::Function {
            param_ty: f_param_ty,
            ret,
            quantifier,
        } => {
            let check_cs = check(env, param, f_param_ty.as_ref());
            cs.extend(check_cs);

            let mut ty = ret.as_ref().clone();

            if let Some(quantifier) = quantifier {
                if let Some(new_ty) = cs.remove(&quantifier) {
                    ty = ty.subst(&quantifier, &new_ty);
                }
            }

            (ty, cs)
        }
        _ => panic!("Can only apply with function"),
    }
}

/* Checking */

fn check(env: &Env, expr: &ast::Expr, ty: &types::Type) -> types::Constraints {
    let (ty_synthed, mut cs) = synth(env, expr);

    match ty {
        types::Type::Var(ref tyvar) => {
            if let Some(t) = cs.insert(tyvar.to_string(), ty_synthed.clone()) {
                panic!("Constraint overwritten for {}: {}", tyvar, t);
            };
        }
        ty_synthed => {
            if ty_synthed != ty {
                panic!("Could not match types {} and {}", ty_synthed, ty);
            }
        }
    }

    cs
}

#[cfg(test)]
mod tests {

    use crate::ast;
    use crate::typecheck;
    use crate::types;

    fn synthesizes_to(expr: &str, ty: &str) {
        let expr = ast::parse(expr);

        let expected = types::parse::parse(ty.to_string());

        let actual = typecheck::synthesize(&expr);

        assert_eq!(expected, actual);
    }

    #[test]
    fn synth_literals() {
        synthesizes_to("2", "integer");
        synthesizes_to("\"hi\"", "string");
    }

    #[test]
    fn synth_attrsets() {
        synthesizes_to("{foo = \"bar\";}", "{foo: string}");
    }

    #[test]
    fn synth_functions() {
        synthesizes_to("x /* integer */: x", "integer -> integer");
        synthesizes_to("add 2", "integer -> integer");
        synthesizes_to("let foo = add 2; in foo 3", "integer");
        synthesizes_to(
            "x /* { foo: string } */: x.foo",
            "{ foo: string } -> string",
        );
        synthesizes_to("x /* T.T */: x", "T.T -> T");
    }

    #[test]
    fn synth_with_unions() {
        synthesizes_to(
            "x /* integer | string */: x",
            "integer | string -> integer | string",
        );
    }

    #[test]
    fn synth_tyvar() {
        synthesizes_to("let f = x /* T.T */: x; in f 2", "integer");
        synthesizes_to("let f = x /* T.T */: add 2 x; in f 2", "integer");
    }

    #[test]
    fn more() {
        synthesizes_to(
            "let x = 2; in let attrs = { foo = x; }; in attrs.foo",
            "integer",
        );
    }
}
