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

    fn set_many(&self, vars: Vec<(String, types::Type)>) -> Env {
        let mut m = self.0.clone();

        for (id, ty) in vars {
            let _ = m.insert(id, ty.clone());
        }

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

pub fn synthesize(expr: &ast::Expr) -> Result<types::Type, String> {
    let env = Env::default();
    let (ty, cs) = synth(&env, &expr)?;

    if !cs.is_empty() {
        return Err(format!("Leftover constraints: {:?}", cs));
    }

    Ok(ty)
}

fn synth(env: &Env, expr: &ast::Expr) -> Result<(types::Type, types::Constraints), String> {
    let no_constraints =
        |x: types::Type| (x, std::collections::HashMap::<String, types::Type>::new());
    match expr {
        ast::Expr::StringLiteral(_) => Ok(no_constraints(types::Type::String)),
        ast::Expr::IntegerLiteral(_) => Ok(no_constraints(types::Type::Integer)),
        ast::Expr::Identifier(i) => Ok(no_constraints(synth_identifier(env, i)?)),
        ast::Expr::AttributeSet { attributes } => synth_attrset(env, attributes.to_vec()),
        ast::Expr::Lambda {
            param_id,
            param_ty,
            body,
            quantifier,
        } => synth_lambda(env, quantifier, param_id, param_ty, body),
        ast::Expr::Select { attrset, attrname } => synth_select(env, attrset, attrname),
        ast::Expr::Let { bindings, body } => synth_let(env, bindings, body),
        ast::Expr::App { f, param } => synth_app(env, f, param),
    }
}

fn synth_bindings(
    env: &Env,
    bindings: Vec<(String, ast::Expr)>,
) -> Result<(Vec<(String, types::Type)>, types::Constraints), String> {
    use types::{Constraints, Type};

    let bindings: Vec<(String, Type, Constraints)> = bindings
        .into_iter()
        .map(|(attrname, expr)| {
            let (ty, cs) = synth(&env, &expr)?;
            Ok((attrname, ty, cs))
        })
        .collect::<Result<Vec<(String, Type, Constraints)>, String>>()?;

    let mut tys = vec![];
    let mut css = std::collections::HashMap::new();

    for (n, ty, cs) in bindings {
        tys.push((n, ty));
        css.extend(cs);
    }

    Ok((tys, css))
}

fn synth_bindings_rec(
    env: &Env,
    bindings: Vec<(String, ast::Expr)>,
) -> Result<(Vec<(String, types::Type)>, types::Constraints), String> {
    use types::Type;

    let bindings_tyvars: Vec<(String, types::Type)> = bindings
        .iter()
        .map(|(varname, _expr)| {
            // TODO: ugly hack, Type var should be "A", "B", ...
            (varname.clone(), types::Type::Var(varname.clone()))
        })
        .collect();

    let env = env.set_many(bindings_tyvars.clone());
    let (bindings_tys, cs) = synth_bindings(&env, bindings.clone())?;

    let constraints: Vec<(types::Type, types::Type)> = bindings_tys
        .iter()
        .zip(bindings_tyvars.clone().into_iter())
        .map(|((varl, tyl), (varr, tyr))| {
            assert_eq!(varl, &varr);
            (tyr, tyl.clone())
        })
        .collect();

    type Res = Result<Vec<(String, Type)>, String>;
    let substs: Substitutions = unify(constraints)?;
    println!("Unification gave {:?}", substs);
    let bindings = bindings_tyvars
        .iter()
        .map(|(k, tyvar)| {
            let ty = tyvar.apply_substs(&substs);
            Ok((k.clone(), ty.clone()))
        })
        .collect::<Res>()?;
    // TODO: are these the right constraints?
    Ok((bindings, cs))
}

fn synth_attrset(
    env: &Env,
    attributes: Vec<(String, ast::Expr)>,
) -> Result<(types::Type, types::Constraints), String> {
    let (tys, css) = synth_bindings(env, attributes)?;
    Ok((types::Type::AttributeSet { attributes: tys }, css))
}

fn synth_identifier(env: &Env, id: &String) -> Result<types::Type, String> {
    let ty = env
        .get(&id)
        .ok_or(format!("Identifier not found: {:?}", &id).as_str())?;
    Ok(ty.clone())
}

fn synth_lambda(
    env: &Env,
    quantifier: &Option<String>,
    param_id: &String,
    param_ty: &types::Type,
    body: &ast::Expr,
) -> Result<(types::Type, types::Constraints), String> {
    let env = env.set(param_id.clone(), param_ty.clone());
    let (ty, cs) = synth(&env, body)?;

    if let Some(quantifier) = quantifier {
        if let Some(c) = cs.get(quantifier) {
            return Err(format!(
                "Quantifier {} cannot be constrained to {}",
                quantifier, c
            ));
        }
    }

    let ty = types::Type::Function {
        param_ty: Box::new(param_ty.clone()),
        ret: Box::new(ty),
        quantifier: quantifier.clone(),
    };

    Ok((ty, cs))
}

fn synth_select(
    env: &Env,
    expr: &ast::Expr,
    param_id: &String,
) -> Result<(types::Type, types::Constraints), String> {
    let (synthed, cs) = synth(env, expr)?;

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
            .ok_or(format!("No such attribute: {:?}", param_id).as_str())?
            .clone(),
        _ => return Err("Can only select on attrsets".to_string()),
    };

    Ok((res, cs))
}

fn synth_let(
    env: &Env,
    bindings: &Vec<(String, ast::Expr)>,
    body: &ast::Expr,
) -> Result<(types::Type, types::Constraints), String> {
    let (bindings, mut cs) = synth_bindings_rec(env, bindings.clone())?;
    let env = env.set_many(bindings);
    let (ty, cs_extra) = synth(&env, body)?;

    cs.extend(cs_extra);
    Ok((ty, cs))
}

fn synth_app(
    env: &Env,
    f: &ast::Expr,
    param: &ast::Expr,
) -> Result<(types::Type, types::Constraints), String> {
    /* The type of 'f a' is basically the return type of 'f', after checking that
     * 'f' is indeed a function, and after checking that 'f's argument is of the
     * same type as 'a'. */
    let (f_ty, mut cs) = synth(env, f)?;
    match f_ty {
        types::Type::Function {
            param_ty: f_param_ty,
            ret,
            quantifier,
        } => {
            let check_cs = check(env, param, f_param_ty.as_ref())?;
            cs.extend(check_cs);

            let mut ty = ret.as_ref().clone();

            if let Some(quantifier) = quantifier {
                if let Some(new_ty) = cs.remove(&quantifier) {
                    ty = ty.subst(&quantifier, &new_ty);
                }
            }

            Ok((ty, cs))
        }
        _ => Err("Can only apply with function".to_string()),
    }
}

/* Checking */

fn check(env: &Env, expr: &ast::Expr, ty: &types::Type) -> Result<types::Constraints, String> {
    let (ty_synthed, mut cs) = synth(env, expr)?;

    let substs = unify(vec![(ty_synthed, ty.clone())])?;

    cs.extend(substs);

    Ok(cs)
}

/* Unification */

type Constraint = (types::Type, types::Type);
pub type Substitutions = Vec<(String, types::Type)>;

pub fn unify(cs: Vec<Constraint>) -> Result<Substitutions, String> {
    let mut cs = std::collections::VecDeque::from(cs);

    let mut substs: Substitutions = vec![];

    while let Some(c) = cs.pop_front() {
        match c {
            // if tyl == tyr then the constraint isn't giving us any info
            // so we just discard it
            (tyl, tyr) if tyl == tyr => {}
            (types::Type::Var(tyvar), ty) | (ty, types::Type::Var(tyvar)) => {
                if ty.mentions(&tyvar) {
                    return Err(format!("Cannot construct infinite type"));
                }

                substs.push((tyvar.clone(), ty.clone()));

                cs.iter_mut().for_each(|(t1, t2)| {
                    *t1 = t1.subst(&tyvar, &ty);
                    *t2 = t2.subst(&tyvar, &ty);
                });
            }
            (
                types::Type::Function {
                    quantifier: _,
                    param_ty: param_tyl,
                    ret: retl,
                },
                types::Type::Function {
                    quantifier: _,
                    param_ty: param_tyr,
                    ret: retr,
                },
            ) => {
                cs.push_back((*param_tyl, *param_tyr));
                cs.push_back((*retl, *retr));
            }
            (tyl, tyr) => {
                // in the first match we check tyl == tyr, so here it means we have
                // tyl != tyr, so it's inconsistent
                return Err(format!("Cannot unify {} and {}", tyl, tyr));
            }
        }
    }

    Ok(substs)
}

#[cfg(test)]
mod tests {

    use crate::ast;
    use crate::typecheck;
    use crate::types;

    fn synthesizes_to(expr_str: &str, ty: &str) {
        let expr = ast::parse(expr_str).unwrap();

        let expected = types::parse::parse_type(ty.to_string()).unwrap();

        let actual =
            typecheck::synthesize(&expr).expect(&format!("could not synth: {:?}", expr_str));

        assert_eq!(expected, actual);
    }

    fn assert_bindings_rec(exprs: Vec<(&str, &str)>, expected: Vec<(&str, &str)>) {
        let exprs: Vec<(String, ast::Expr)> = exprs
            .into_iter()
            .map(|(k, v)| {
                let expr = ast::parse(v).unwrap();
                (k.to_string(), expr)
            })
            .collect();

        let expected: Vec<(String, types::Type)> = expected
            .into_iter()
            .map(|(k, v)| {
                let ty: types::Type = types::parse::parse_type(v.to_string()).unwrap();
                (k.to_string(), ty)
            })
            .collect();

        let env = typecheck::Env::default();
        let (actual, _) = typecheck::synth_bindings_rec(&env, exprs).unwrap();

        for (var, ty_expected) in expected {
            let ty_actual = actual
                .iter()
                .find_map(|(k, v)| if k == &var { Some(v) } else { None })
                .unwrap();
            assert_eq!(&ty_expected, ty_actual);
        }
    }

    fn ill_typed(expr: &str) {
        let parsed = ast::parse(expr).unwrap();
        match typecheck::synthesize(&parsed) {
            Err(_) => (),
            Ok(ty) => panic!(
                "Expected expression no to synthesize, but got '{}: {}'\n{:?}",
                expr, ty, &parsed
            ),
        }
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

        ill_typed("let f = x /* string */:x; in x 2");
        ill_typed("add {} {}");

        ill_typed("x /*A.A*/: add x x");
    }

    #[test]
    fn synth_tyvar() {
        synthesizes_to("let f = x /* T.T */: x; in f 2", "integer");

        // Ill typed because T is actually integer
        ill_typed("let f = x /* T.T */: add 2 x; in f 2");
    }

    #[test]
    fn more() {
        synthesizes_to("(2)", "integer");

        synthesizes_to(r#"(x /* string */: 32) "hello""#, "integer");

        synthesizes_to(
            "let x = 2; in let attrs = { foo = x; }; in attrs.foo",
            "integer",
        );

        synthesizes_to(
            r#"let f = a /* A.A */: b /* B.B */: { a = a; b = b; } ; in f 2 """#,
            "{ a: integer, b: string }",
        );

        ill_typed(r#"(x /* A.A */: y /* A */: {}) 2 "string""#);
    }

    #[test]
    fn synth_bindings_rec() {
        assert_bindings_rec(vec![("x", "2")], vec![("x", "integer")]);
        assert_bindings_rec(vec![("x", "y"), ("y", "2")], vec![("x", "integer")]);
    }

    fn unifies_to(tyenv: &str, expected: Vec<(&str, &str)>) {
        let tyenv: types::parse::TypeDecls =
            types::parse::parse_type_env(tyenv.to_string()).expect("Could not parse type env");
        let to_unify = tyenv
            .clone()
            .into_iter()
            .map(|(k, v)| (types::Type::Var(k), v))
            .collect();

        let substs = typecheck::unify(to_unify).unwrap();

        let mut tyenv: Vec<(String, String)> = tyenv
            .into_iter()
            .map(|(k, mut v)| {
                for (tyvar, ty) in &substs {
                    v = v.subst(&tyvar, &ty);
                }

                (k, format!("{}", v))
            })
            .collect();
        tyenv.sort();
        let actual = tyenv;

        let mut expected: Vec<(String, String)> = expected
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        expected.sort();

        for e in expected {
            if !actual.contains(&e) {
                panic!("{:?}, missing: {:?}", actual, e);
            }
        }
    }

    #[test]
    fn unification() {
        unifies_to("A = integer", vec![("A", "integer")]);
        unifies_to("A = A, A = integer", vec![("A", "integer")]);
        unifies_to(
            "A = integer, B = A",
            vec![("A", "integer"), ("B", "integer")],
        );
        unifies_to(
            "A = B, B = integer",
            vec![("A", "integer"), ("B", "integer")],
        );
        unifies_to(
            "A = integer -> B, B = integer",
            vec![("A", "integer -> integer"), ("B", "integer")],
        );
        unifies_to(
            "A = integer -> B, B = integer",
            vec![("A", "integer -> integer"), ("B", "integer")],
        );
        unifies_to(
            "A = B -> integer, C = integer -> B, A = C",
            vec![("A", "integer -> integer"), ("C", "integer -> integer")],
        );
    }
}
