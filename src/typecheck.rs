use crate::ast;
use crate::types;

/* Synthesizing */

struct Env(std::collections::HashMap<String, types::Type>);

impl Env {
    fn get(&self, id: &String) -> Option<&types::Type> {
        let Env(m) = self;
        m.get(id)
    }

    fn set(&self, id: String, ty: types::Type) -> Env {
        let Env(m) = self;
        let mut m = m.clone();
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
    synth(&env, &expr)
}

fn synth(env: &Env, expr: &ast::Expr) -> types::Type {
    match expr {
        ast::Expr::StringLiteral(_) => types::Type::String,
        ast::Expr::IntegerLiteral(_) => types::Type::Integer,
        ast::Expr::Identifier(i) => synth_identifier(env, i),
        ast::Expr::AttributeSet { attributes } => synth_attrset(env, attributes.to_vec()),
        ast::Expr::Lambda {
            param_id,
            param_ty,
            body,
        } => synth_lambda(env, param_id, param_ty, body),
        ast::Expr::Select { attrset, attrname } => synth_select(env, attrset, attrname),
        ast::Expr::Let {
            var_name,
            var_expr,
            body,
        } => synth_let(env, var_name, var_expr, body),
        ast::Expr::App { f, param } => synth_app(env, f, param),
        _ => todo!(),
    }
}

fn synth_attrset(env: &Env, attributes: Vec<(String, ast::Expr)>) -> types::Type {
    let synthed = attributes
        .into_iter()
        .map(|(attrname, expr)| (attrname, synth(env, &expr)))
        .collect();

    types::Type::AttributeSet {
        attributes: synthed,
    }
}

fn synth_identifier(env: &Env, id: &String) -> types::Type {
    env.get(&id)
        .expect(format!("Identifier not found: {:?}", id).as_str())
        .clone()
}

fn synth_lambda(
    env: &Env,
    param_id: &String,
    param_ty: &types::Type,
    body: &ast::Expr,
) -> types::Type {
    let env = env.set(param_id.clone(), param_ty.clone());
    let ret = synth(&env, body);
    types::Type::Function {
        param_ty: Box::new(param_ty.clone()),
        ret: Box::new(ret),
    }
}

fn synth_select(env: &Env, expr: &ast::Expr, param_id: &String) -> types::Type {
    let synthed = synth(env, expr);

    match synthed {
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
    }
}

fn synth_let(env: &Env, var_name: &String, var_expr: &ast::Expr, body: &ast::Expr) -> types::Type {
    let var_ty = synth(env, var_expr);
    let env = env.set(var_name.clone(), var_ty);
    synth(&env, body)
}

fn synth_app(env: &Env, f: &ast::Expr, param: &ast::Expr) -> types::Type {
    /* The type of 'f a' is basically the return type of 'f', after checking that
     * 'f' is indeed a function, and after checking that 'f's argument is of the
     * same type as 'a'. */
    let f_ty = synth(env, f);
    match f_ty {
        types::Type::Function {
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

fn check(env: &Env, expr: &ast::Expr, ty: &types::Type) {
    let synthed = synth(env, expr);

    if synthed != *ty {
        panic!("Could not match types {} and {}", synthed, ty);
    }
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
    }

    #[test]
    fn synth_with_unions() {
        synthesizes_to(
            "x /* integer | string */: x",
            "integer | string -> integer | string",
        );
    }

    #[test]
    fn more() {
        synthesizes_to(
            "let x = 2; in let attrs = { foo = x; }; in attrs.foo",
            "integer",
        );
    }
}
