use crate::ast;
use crate::types;

/* Synthesizing */

#[derive(Clone)]
struct Env {
    values: std::collections::HashMap<String, types::Type>,
}

impl Env {
    fn get(&self, id: &String) -> Option<&types::Type> {
        self.values.get(id)
    }

    fn set(&self, id: String, ty: types::Type) -> Env {
        let mut env = self.clone();
        let _ = env.values.insert(id, ty.clone());
        env
    }

    fn set_many(&self, vars: &Vec<(String, types::Type)>) -> Env {
        let mut env = self.clone();

        for (id, ty) in vars {
            let _ = env.values.insert(id.clone(), ty.clone());
        }

        env
    }

    // The default nix env
    fn default() -> Env {
        let mut values = std::collections::HashMap::new();

        let mut insert = |name: &str, ty: &str| {
            let _ = values.insert(name.to_string(), ty.parse().unwrap());
        };
        insert("abort", "string -> never");
        insert("add", "integer -> integer -> integer");
        insert("baseNameOf", "string -> string");
        insert("bitAnd", "integer -> integer");
        insert("bitOr", "integer -> integer");
        insert("bitXor", "integer -> integer");
        insert("true", "bool");
        Env { values }
    }
}

type Synthed = (types::Type, Vec<TyVarEq>);

// Result of a synth.
// * Ok: The type of the expression, and any potential leftover Eq constraints.
// * Err: The error string.
type TypingResult = Result<Synthed, String>;

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

fn synth(env: &Env, expr: &ast::Expr) -> TypingResult {
    let no_constraints = |x: types::Type| (x, vec![]);
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
        ast::Expr::IfElse {
            cond,
            branch_true,
            branch_false,
        } => synth_ifelse(env, cond, branch_true, branch_false),
    }
}

// Synth bindings
// Non-recursive, meaning all value references inside expressions
// are assumed to come from the environment.
fn synth_bindings(
    env: &Env,
    bindings: Vec<(String, ast::Expr)>,
) -> Result<(Vec<(String, types::Type)>, Vec<TyVarEq>), String> {
    // Synth each expression, returning (for each binding key) the synthesized
    // type (if successful) and any potential constraints on the env
    let bindings: Vec<(String, Synthed)> = bindings
        .into_iter()
        .map(|(key, expr)| Ok((key, synth(&env, &expr)?)))
        .collect::<Result<Vec<_>, String>>()?;

    // Prepare the resulting types (indexed by keys) and the constraints (aggregated)
    let mut tys = vec![];
    let mut css = vec![];
    for (n, (ty, cs)) in bindings {
        tys.push((n, ty));
        css.append(&mut cs.clone());
    }

    Ok((tys, css))
}

fn synth_bindings_rec(
    env: &Env,
    bindings: Vec<(String, ast::Expr)>,
) -> Result<(Vec<(String, types::Type)>, Vec<TyVarEq>), String> {
    use types::Type;

    // To synth recursive bindings, we first (1) add all keys to the env (as type variables), then
    // (2) synth the bindings (non-recursive) and finally (3) unify, making sure all bindings are fully
    // resolved.

    // 1. Create a fresh type variable for each binding
    let bindings_tyvars: Vec<(String, types::Type)> = bindings
        .iter()
        .map(|(key, _expr)| {
            // TODO: ugly hack, Type var should be "A", "B", ...
            let fresh_tyvar = types::Type::Var(key.clone());
            (key.clone(), fresh_tyvar)
        })
        .collect();

    // 2. Synth the bindings with the updated environment
    let env = env.set_many(&bindings_tyvars);
    let (bindings_tys, cs) = synth_bindings(&env, bindings.clone())?;

    // 3. Unify

    // we partition requirements between _our_ new requirements (i.e. related to tyvars
    // we introduced above) and unrelated requirements.
    //  * Our requirements: prepare for unification
    //  * Unrelated requirements: bubble up
    //
    // NOTE: quadratic because I'm lazy
    let (ours, unrelated): (Vec<TyVarEq>, Vec<TyVarEq>) = cs.into_iter().partition(|(tyvar, _)| {
        bindings_tyvars
            .iter()
            .any(|(our_tyvar, _)| tyvar == our_tyvar)
    });

    let mut ours: Vec<Constraint> = ours
        .into_iter()
        .map(|(tyvar, ty)| (Type::Var(tyvar), ty))
        .collect();

    // Create new constraints from the bindings, i.e. each key ~ tyvar generated for key
    let mut constraints: Vec<Constraint> = bindings_tys
        .iter()
        .zip(bindings_tyvars.clone().into_iter())
        .map(|((keyl, tyl), (keyr, tyr))| {
            assert_eq!(keyl, &keyr);
            (tyr, tyl.clone())
        })
        .collect();
    constraints.append(&mut ours);

    type Res = Result<Vec<(String, Type)>, String>;
    let substs: Substitutions = unify(constraints)?;
    let bindings = bindings_tyvars
        .iter()
        .map(|(k, tyvar)| {
            let ty = tyvar.apply_substs(&substs);
            Ok((k.clone(), ty.clone()))
        })
        .collect::<Res>()?;

    Ok((bindings, unrelated))
}

fn synth_attrset(env: &Env, attributes: Vec<(String, ast::Expr)>) -> TypingResult {
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
) -> TypingResult {
    let env = env.set(param_id.clone(), param_ty.clone());
    let (ty, cs) = synth(&env, body)?;

    if let Some(quantifier) = quantifier {
        if let Some((_, ty)) = cs.iter().find(|(tyvar, _)| tyvar == quantifier) {
            return Err(format!(
                "Quantifier {} cannot be constrained to {}",
                quantifier, ty
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

fn synth_select(env: &Env, expr: &ast::Expr, param_id: &String) -> TypingResult {
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

fn synth_let(env: &Env, bindings: &Vec<(String, ast::Expr)>, body: &ast::Expr) -> TypingResult {
    let (bindings, mut cs) = synth_bindings_rec(env, bindings.clone())?;
    let env = env.set_many(&bindings);
    let (ty, cs_extra) = synth(&env, body)?;

    cs.extend(cs_extra);
    Ok((ty, cs))
}

fn synth_app(env: &Env, f: &ast::Expr, param: &ast::Expr) -> TypingResult {
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
            /* When synthing/checking an application, we check if the function has a quantifier.
             * If so, we check for a constraint on that tyvar, and if so replace tyvar with
             * the constraint in the function return type. */
            let check_cs = check(env, param, f_param_ty.as_ref())?;
            cs.extend(check_cs);

            let mut ty = ret.as_ref().clone();

            if let Some(quantifier) = quantifier {
                let relevant: Vec<&(String, types::Type)> = cs
                    .iter()
                    .filter(|(tyvar, _)| tyvar == &quantifier)
                    .collect();
                if relevant.len() > 1 {
                    return Err(format!(
                        "Too many requirements on quantifier {}",
                        quantifier
                    ));
                }

                if let Some((_, new_ty)) = cs.pop() {
                    ty = ty.subst(&quantifier, &new_ty);
                }
            }

            Ok((ty, cs))
        }
        _ => Err("Can only apply with function".to_string()),
    }
}

fn synth_ifelse(
    env: &Env,
    cond: &ast::Expr,
    branch_true: &ast::Expr,
    branch_false: &ast::Expr,
) -> TypingResult {
    let (ty_cond, mut cs) = synth(env, cond)?;

    let (ty_true, mut cs_new) = synth(env, branch_true)?;
    cs.append(&mut cs_new);

    let (ty_false, mut cs_new) = synth(env, branch_false)?;
    cs.append(&mut cs_new);

    if ty_cond != types::Type::Bool {
        return Err("If/else condition must be boolean".to_string());
    }

    if ty_true != ty_false {
        return Err("If/else branches have differing types".to_string());
    }

    Ok((ty_true, cs))
}

/* Checking */

fn check(env: &Env, expr: &ast::Expr, ty: &types::Type) -> Result<Vec<TyVarEq>, String> {
    let (ty_synthed, mut cs) = synth(env, expr)?;

    let substs = unify(vec![(ty_synthed, ty.clone())])?;

    cs.extend(substs);

    Ok(cs)
}

/* Unification */

// A requirement of equality of a type variable
type TyVarEq = (String, types::Type);

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
        synthesizes_to(r#""hi""#, "string");
    }

    #[test]
    fn synth_attrsets() {
        synthesizes_to(r#"{foo = "bar";}"#, "{foo: string}");
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
    fn synth_ifelse() {
        synthesizes_to("if true then 2 else 5", "integer");

        ill_typed("if true then 2 else {}");
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

        synthesizes_to(
            r#"
                let
                  x = 2;
                  y = add 34 x;
                in { foo = y; }
            "#,
            "{ foo: integer }",
        );

        ill_typed(r#"(x /* A.A */: y /* A */: {}) 2 "string""#);

        ill_typed(
            r#"
            let
                foo = 2;
                f = x /* integer[] */: x;
            in f 2
        "#,
        );
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
