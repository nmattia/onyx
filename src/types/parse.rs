/* Parsing of type annotations */
/* This module defines and uses some simple parser combinator */

use crate::types::Type;

type ParseResult<T> = Option<(T, usize)>;

// Use top level parser and discard implementation details
pub fn parse(s: String) -> Type {
    let (res, l) = parse_ty(&s).expect(format!("Failed to parse type: {}", s).as_str());

    if s.len() != l {
        panic!("not all input was consumed");
    }

    res
}

#[test]
fn test_parse_ty() {
    assert_eq!(
        parse("integer | string -> integer | string".to_string()),
        Type::Function {
            param_ty: Box::new(Type::Union {
                left: Box::new(Type::Integer),
                right: Box::new(Type::String)
            }),
            ret: Box::new(Type::Union {
                left: Box::new(Type::Integer),
                right: Box::new(Type::String)
            }),
        }
    );

    assert_eq!(
        format!(
            "{}",
            parse_ty_fn("integer | string -> integer | string")
                .unwrap()
                .0
        ),
        "integer | string -> integer | string"
    );
}

// Top-level parse, for any type
fn parse_ty(s: &str) -> ParseResult<Type> {
    parse_ty_parens(s)
        .or_else(|| parse_ty_attrset(s))
        .or_else(|| parse_ty_fn(s))
        .or_else(|| parse_ty_union(s))
        .or_else(|| parse_ty_simple(s))
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

// Parse a simple type like 'integer', 'string', etc
fn parse_ty_simple(s: &str) -> ParseResult<Type> {
    parse_trim_whitespace(s, &|s: &str| {
        const INTEGER: &str = "integer";
        const STRING: &str = "string";
        const BOOL: &str = "bool";
        if s.to_string().starts_with(INTEGER) {
            Some((Type::Integer, INTEGER.len()))
        } else if s.to_string().starts_with(STRING) {
            Some((Type::String, STRING.len()))
        } else if s.to_string().starts_with(BOOL) {
            Some((Type::Bool, BOOL.len()))
        } else {
            None
        }
    })
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
        format!(
            "{}",
            parse_ty_fn("integer  -> integer->  integer").unwrap().0
        ),
        "integer -> integer -> integer"
    );
}

// Parse a function
fn parse_ty_fn(s: &str) -> ParseResult<Type> {
    let mut tally = 0;

    let (lres, l) = parse_trim_whitespace(&s[tally..], &|s| {
        parse_ty_parens(s)
            .or_else(|| parse_ty_attrset(s))
            .or_else(|| parse_ty_union(s))
            .or_else(|| parse_ty_simple(s))
    })?;
    tally += l;

    let ((), l) = parse_ty_char(&s[tally..], '-')?;
    tally += l;

    let ((), l) = parse_ty_char(&s[tally..], '>')?;
    tally += l;

    let (rres, l) = parse_trim_whitespace(&s[tally..], &|s| parse_ty(s))?;
    tally += l;

    Some((
        Type::Function {
            param_ty: Box::new(lres),
            ret: Box::new(rres),
        },
        tally,
    ))
}

#[test]
fn test_parse_ty_union() {
    assert_eq!(
        format!("{}", parse_ty_union("integer|integer").unwrap().0),
        "integer | integer"
    );
    assert_eq!(
        format!("{}", parse_ty_union("integer|integer|integer").unwrap().0),
        "integer | integer | integer"
    );
    assert_eq!(
        format!("{}", parse_ty_union("(integer|integer)|integer").unwrap().0),
        "integer | integer | integer"
    );
    assert_eq!(
        format!("{}", parse_ty_union("integer|(integer|integer)").unwrap().0),
        "integer | integer | integer"
    );

    assert_eq!(
        format!(
            "{}",
            parse_ty_union("integer  | integer|  integer").unwrap().0
        ),
        "integer | integer | integer"
    );
}

// Parse a type union
fn parse_ty_union(s: &str) -> ParseResult<Type> {
    let mut tally = 0;

    let (lres, l) = parse_trim_whitespace(&s[tally..], &|s| {
        parse_ty_parens(s)
            .or_else(|| parse_ty_attrset(s))
            .or_else(|| parse_ty_simple(s))
    })?;
    tally += l;

    let ((), l) = parse_ty_char(&s[tally..], '|')?;
    tally += l;

    let (rres, l) = parse_trim_whitespace(&s[tally..], &|s| {
        parse_ty_parens(s)
            .or_else(|| parse_ty_attrset(s))
            .or_else(|| parse_ty_union(s))
            .or_else(|| parse_ty_simple(s))
    })?;
    tally += l;

    Some((
        Type::Union {
            left: Box::new(lres),
            right: Box::new(rres),
        },
        tally,
    ))
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
}

// Parse an attrset type {foo: string}
fn parse_ty_attrset(s: &str) -> ParseResult<Type> {
    let mut tally: usize = 0;

    let ((), l) = parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty_char(s, '{'))?;
    tally += l;

    let (ids_and_tys, l) = parse_joined(&s[tally..], &|s| parse_ty_attrset_kv(s), ",")?;
    tally += l;

    let ((), l) = parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty_char(s, '}'))?;
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
        parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty_attrset_identifier(s))?;
    tally += l;

    let ((), l) = parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty_char(s, ':'))?;
    tally += l;

    let (ty, l) = parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty(s))?;
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

    let ((), l) = parse_ty_char(&s[tally..], '(')?;
    tally += l;

    let (res, l) = parse_trim_whitespace(&s[tally..], &|s| parse_ty(s))?;
    tally += l;

    let ((), l) = parse_ty_char(&s[tally..], ')')?;
    tally += l;

    Some((res, tally))
}

/* Helpers */

fn parse_trim_whitespace<T>(s: &str, f: &dyn Fn(&str) -> ParseResult<T>) -> ParseResult<T> {
    let mut tally = 0;

    fn is_space(c: &char) -> bool {
        c == &' '
    }

    let l: usize = s.chars().take_while(is_space).collect::<Vec<_>>().len();

    tally += l;

    let (res, l) = f(&s[tally..])?;

    tally += l;

    let l: usize = s[tally..]
        .chars()
        .take_while(is_space)
        .collect::<Vec<_>>()
        .len();

    tally += l;

    Some((res, tally))
}

fn parse_ty_char(s: &str, c: char) -> ParseResult<()> {
    if s.chars().nth(0) != Some(c) {
        None
    } else {
        Some(((), 1))
    }
}

fn parse_ty_string(s: &str, t: &str) -> ParseResult<()> {
    if s.starts_with(t) {
        Some(((), t.len()))
    } else {
        None
    }
}

#[cfg(test)]
fn parse_ty_char_(s: &str) -> ParseResult<char> {
    match s.chars().nth(0) {
        Some(c) => Some((c, 1)),
        None => None,
    }
}

#[test]
fn test_parse_many() {
    let s = "aaa";
    assert_eq!(
        parse_many(&s, &|s| parse_ty_char_(s)).unwrap().0,
        vec!['a', 'a', 'a'],
    );
}

// Call the parser as many times as possible. Note: always matches (may return empty vec).
fn parse_many<T>(s: &str, f: &dyn Fn(&str) -> ParseResult<T>) -> ParseResult<Vec<T>> {
    let mut tally = 0;

    let mut vec: Vec<T> = vec![];

    while let Some((res, l)) = f(&s[tally..]) {
        tally += l;

        vec.push(res);
    }

    Some((vec, tally))
}

#[test]
fn test_parse_joined() {
    let s = "a,b,c";
    assert_eq!(
        parse_joined(&s, &|s| parse_ty_char_(s), ",").unwrap().0,
        vec!['a', 'b', 'c'],
    );
}

fn parse_joined<T: std::fmt::Debug>(
    s: &str,
    f: &dyn Fn(&str) -> ParseResult<T>,
    joiner: &str,
) -> ParseResult<Vec<T>> {
    let mut tally = 0;

    let (first, l) = f(&s)?;
    let mut vec = vec![first];
    tally += l;

    let pump_one = |s: &str| {
        let mut tally = 0;
        let (res, l) = parse_ty_string(s, joiner).and_then(|((), l)| {
            tally += l;
            f(&s[tally..])
        })?;

        tally += l;

        Some((res, tally))
    };

    let (mut rest, l) = parse_many(&s[tally..], &pump_one)?;
    tally += l;

    vec.append(&mut rest);

    Some((vec, tally))
}
