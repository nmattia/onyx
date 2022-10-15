/* Parsing of type annotations */
/* This module defines and uses some simple parser combinator */

use crate::types::parse_utils;
use crate::types::parse_utils::{run_parser, ParseResult};
use crate::types::Type;

/* Type Environments (A = ..., B = ...) */

pub type TypeDecls = Vec<(String, Type)>;

#[test]
fn test_parse_ty_env() {
    let tys = parse_type_env(r#"A = integer, B = A "#.to_string()).unwrap();

    let tys: Vec<(String, String)> = tys
        .into_iter()
        .map(|(k, v)| (k, format!("{}", v)))
        .collect();

    assert_eq!(
        tys,
        vec![
            ("A".to_string(), "integer".to_string()),
            ("B".to_string(), "A".to_string())
        ]
    );
}

pub fn parse_type_env(s: String) -> Result<TypeDecls, String> {
    run_parser(
        &|s| parse_utils::parse_joined(s, &parse_ty_assignment, ","),
        &s,
    )
}

#[test]
fn test_parse_ty_assignment() {
    let (name, ty) = run_parser(&parse_ty_assignment, "T = integer").unwrap();
    assert_eq!(name, "T");
    assert_eq!(format!("{}", ty), "integer");

    let (name, ty) = run_parser(&parse_ty_assignment, " T = integer  ").unwrap();
    assert_eq!(name, "T");
    assert_eq!(format!("{}", ty), "integer");
}

fn parse_ty_assignment(s: &str) -> ParseResult<(String, Type)> {
    let mut tally = 0;

    let (tyvarname, l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s| parse_ty_varname(s))?;
    tally += l;

    let ((), l) =
        parse_utils::parse_trim_whitespace(&s[tally..], &|s| parse_utils::parse_ty_char(s, '='))?;
    tally += l;

    let (ty, l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s| parse_ty(s))?;
    tally += l;

    Some(((tyvarname, ty), tally))
}

/* Single types */

// Use top level parser and discard implementation details
pub fn parse_type(s: String) -> Result<Type, String> {
    run_parser(&parse_ty, &s)
}

#[test]
fn test_parse_ty() {
    assert_eq!(
        format!(
            "{}",
            parse_type("{ foo: string } -> string".to_string()).unwrap()
        ),
        "{ foo: string } -> string"
    );
}

// Top-level parse, for any type
fn parse_ty(s: &str) -> ParseResult<Type> {
    parse_ty_fn(s)
        .or_else(|| parse_ty_parens(s))
        .or_else(|| parse_ty_attrset(s))
        .or_else(|| parse_ty_list(s))
        .or_else(|| parse_ty_simple(s))
        .or_else(|| parse_ty_var(s))
}

#[test]
fn test_parse_ty_simple() {
    assert_eq!(
        format!("{}", parse_ty_simple("integer").unwrap().0),
        "integer"
    );

    assert_eq!(
        format!("{}", parse_ty_simple(" integer ").unwrap().0),
        "integer"
    );
}

fn parse_ty_varname(s: &str) -> ParseResult<String> {
    let mut tally = 0;

    let (tyvar, l) = parse_utils::parse_ty_char_(&s[tally..])?;
    tally += l;

    if !tyvar.is_uppercase() {
        return None;
    }

    Some((tyvar.to_string(), tally))
}

fn parse_ty_var(s: &str) -> ParseResult<Type> {
    parse_ty_varname(s).map(|(v, l)| (Type::Var(v), l))
}

// Parse a simple type like 'integer', 'string', etc
fn parse_ty_simple(s: &str) -> ParseResult<Type> {
    parse_utils::parse_trim_whitespace(s, &|s: &str| {
        const INTEGER: &str = "integer";
        const STRING: &str = "string";
        const BOOL: &str = "bool";
        const NEVER: &str = "never";
        if s.to_string().starts_with(INTEGER) {
            Some((Type::Integer, INTEGER.len()))
        } else if s.to_string().starts_with(STRING) {
            Some((Type::String, STRING.len()))
        } else if s.to_string().starts_with(BOOL) {
            Some((Type::Bool, BOOL.len()))
        } else if s.to_string().starts_with(NEVER) {
            Some((Type::Never, NEVER.len()))
        } else {
            None
        }
    })
}

#[test]
fn test_parse_quantifier() {
    assert_parse_ty_roundtrip("T.T -> T");
    assert_eq!(run_parser(&parse_quantifier_prefix, "T.").unwrap(), "T");
}

pub fn parse_quantifier_prefix(s: &str) -> ParseResult<String> {
    let mut tally = 0;

    let (quantifier, l) = parse_ty_varname(s)?;
    tally += l;

    let ((), l) = parse_utils::parse_ty_char(&s[tally..], '.')?;
    tally += l;

    Some((quantifier, tally))
}

#[test]
fn test_parse_ty_fn() {
    assert_eq!(
        format!("{}", parse_ty_fn("integer->integer").unwrap().0),
        "integer -> integer"
    );
    assert_eq!(
        format!("{}", parse_ty_fn("(integer->integer)->integer").unwrap().0),
        "(integer -> integer) -> integer"
    );
    assert_eq!(
        format!("{}", parse_ty_fn("integer->(integer->integer)").unwrap().0),
        "integer -> integer -> integer"
    );

    assert_eq!(
        format!("{}", parse_ty_fn("integer->(integer->integer)").unwrap().0),
        "integer -> integer -> integer"
    );

    assert_eq!(
        format!("{}", parse_ty_fn("integer->integer->integer").unwrap().0),
        "integer -> integer -> integer"
    );

    assert_eq!(
        format!("{}", parse_ty_fn("{} -> integer").unwrap().0),
        "{} -> integer"
    );

    assert_eq!(
        format!(
            "{}",
            parse_ty_fn("integer  -> integer->  integer").unwrap().0
        ),
        "integer -> integer -> integer"
    );

    assert_eq!(
        format!("{}", parse_ty_fn("{ foo: string } -> string").unwrap().0),
        "{ foo: string } -> string"
    );

    /* with quantifier */
    assert_parse_ty_roundtrip("T.T -> T");
}

// Parse a function
fn parse_ty_fn(s: &str) -> ParseResult<Type> {
    let mut tally = 0;

    let (quantifier, l) = parse_utils::parse_try(&s[tally..], &parse_quantifier_prefix)?;
    tally += l;

    let (lres, l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s| {
        parse_ty_parens(s)
            .or_else(|| parse_ty_attrset(s))
            .or_else(|| parse_ty_list(s))
            .or_else(|| parse_ty_simple(s))
            .or_else(|| parse_ty_var(s))
    })?;
    tally += l;

    let ((), l) = parse_utils::parse_ty_char(&s[tally..], '-')?;
    tally += l;

    let ((), l) = parse_utils::parse_ty_char(&s[tally..], '>')?;
    tally += l;

    let (rres, l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s| parse_ty(s))?;
    tally += l;

    let ty = Type::Function {
        param_ty: Box::new(lres),
        ret: Box::new(rres),
        quantifier,
    };

    Some((ty, tally))
}

#[test]
fn test_parse_ty_attrset() {
    assert_eq!(
        parse_ty_attrset("{foo:integer}"),
        Some((
            (Type::AttributeSet {
                attributes: vec![("foo".to_string(), Type::Integer)]
            }),
            13
        ))
    );

    assert_eq!(
        format!("{}", parse_ty_attrset("{foo:integer}").unwrap().0),
        "{ foo: integer }"
    );

    assert_eq!(
        format!("{}", parse_ty_attrset(" { foo : integer } ").unwrap().0),
        "{ foo: integer }"
    );

    assert_eq!(
        format!(
            "{}",
            parse_ty_attrset("{foo:integer, bar:string}").unwrap().0
        ),
        "{ foo: integer, bar: string }"
    );

    assert_eq!(format!("{}", parse_ty_attrset("{}").unwrap().0), "{}");
}

// Parse an attrset type {foo: string}
fn parse_ty_attrset(s: &str) -> ParseResult<Type> {
    let mut tally: usize = 0;

    let ((), l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s: &str| {
        parse_utils::parse_ty_char(s, '{')
    })?;
    tally += l;

    let (ids_and_tys, l) =
        parse_utils::parse_joined(&s[tally..], &|s| parse_ty_attrset_kv(s), ",")?;
    tally += l;

    let ((), l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s: &str| {
        parse_utils::parse_ty_char(s, '}')
    })?;
    tally += l;

    Some((
        Type::AttributeSet {
            attributes: ids_and_tys,
        },
        tally,
    ))
}

// Parse a 'foo: string' part of an attrset type
fn parse_ty_attrset_kv(s: &str) -> ParseResult<(String, Type)> {
    let mut tally = 0;

    let (identifier, l) =
        parse_utils::parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty_attrset_identifier(s))?;
    tally += l;

    let ((), l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s: &str| {
        parse_utils::parse_ty_char(s, ':')
    })?;
    tally += l;

    let (ty, l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty(s))?;
    tally += l;

    Some(((identifier, ty), tally))
}

#[test]
fn test_parse_ty_attrset_identifier() {
    assert_eq!(
        parse_ty_attrset_identifier("foo"),
        Some(("foo".to_string(), 3))
    );

    assert_eq!(
        parse_ty_attrset_identifier("foo:"), // note the colon
        Some(("foo".to_string(), 3))
    );

    assert_eq!(parse_ty_attrset_identifier(":"), None);

    assert_eq!(
        parse_ty_attrset_identifier("bar:"), // note the colon
        Some(("bar".to_string(), 3))
    );
}

// Parse an identifier in an attrset (i.e. attrname)
fn parse_ty_attrset_identifier(s: &str) -> ParseResult<String> {
    fn is_char_identifier(c: &char) -> bool {
        matches!(c, 'a'..='z')
    }

    let s: String = s.chars().take_while(is_char_identifier).collect();
    let l = s.len();

    if l < 1 {
        return None;
    }

    Some((s, l))
}

#[test]
fn test_parse_ty_parens() {
    assert_eq!(
        format!("{}", parse_ty_parens("(integer)").unwrap().0),
        "integer"
    );
    assert_eq!(
        format!("{}", parse_ty_parens("((integer))").unwrap().0),
        "integer"
    );

    assert_eq!(
        format!("{}", parse_ty_parens("(  (integer ) )").unwrap().0),
        "integer"
    );
}

// Parse a type, potentially wraps in ()
fn parse_ty_parens(s: &str) -> ParseResult<Type> {
    let mut tally = 0;

    let ((), l) = parse_utils::parse_ty_char(&s[tally..], '(')?;
    tally += l;

    let (res, l) = parse_utils::parse_trim_whitespace(&s[tally..], &|s| parse_ty(s))?;
    tally += l;

    let ((), l) = parse_utils::parse_ty_char(&s[tally..], ')')?;
    tally += l;

    Some((res, tally))
}

#[test]
fn test_parse_ty_list() {
    assert_eq!(
        format!("{}", parse_ty_list("integer[]").unwrap().0),
        "integer[]"
    );

    assert_eq!(format!("{}", parse_ty("integer[]").unwrap().0), "integer[]");

    assert_eq!(
        format!("{}", parse_ty("integer[] -> T[]").unwrap().0),
        "integer[] -> T[]"
    );
}

// Parse a (homogeneous) list: integer[], T[], etc
fn parse_ty_list(s: &str) -> ParseResult<Type> {
    let mut tally = 0;

    let (res, l) = parse_ty_parens(&s[tally..])
        .or_else(|| parse_ty_attrset(&s[tally..]))
        .or_else(|| parse_ty_simple(&s[tally..]))
        .or_else(|| parse_ty_var(&s[tally..]))?;
    tally += l;

    let ((), l) = parse_utils::parse_ty_char(&s[tally..], '[')?;
    tally += l;

    let ((), l) = parse_utils::parse_ty_char(&s[tally..], ']')?;
    tally += l;

    Some((Type::List(Box::new(res)), tally))
}

/* Test helper */

#[cfg(test)]
pub fn assert_parse_ty_roundtrip(ty: &str) {
    assert_eq!(format!("{}", parse_type(ty.to_string()).unwrap()), ty);
}
