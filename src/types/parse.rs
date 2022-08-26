/* Parsing of type annotations */
/* This module defines and uses some simple parser combinator */

use crate::types::Type;

type ParseResult<T> = Option<(T, usize)>;

// Use top level parser and discard implementation details
pub fn parse(s: String) -> Type {
    parse_ty(&s).unwrap().0
}

// Top-level parse, for any type
fn parse_ty(s: &str) -> ParseResult<Type> {
    parse_ty_fn(s).or_else(|| parse_ty_not_fn(s))
}

// Parse anything _but_ a function (used to avoid infinite recursion in function parsing)
fn parse_ty_not_fn(s: &str) -> ParseResult<Type> {
    parse_ty_simple(s)
        .or_else(|| parse_ty_parens(s))
        .or_else(|| parse_ty_attrset(s))
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
        if s.to_string().starts_with(INTEGER) {
            Some((Type::Integer, INTEGER.len()))
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

    let (lres, l) = parse_trim_whitespace(&s[tally..], &|s| parse_ty_not_fn(s))?;
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
}

// Parse an attribute set
fn parse_ty_attrset(s: &str) -> ParseResult<Type> {
    let mut tally: usize = 0;

    let ((), l) = parse_trim_whitespace(s, &|s: &str| parse_ty_char(&s[tally..], '{'))?;
    tally += l;

    let (identifier, l) =
        parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty_attrset_identifier(s))?;
    tally += l;

    let ((), l) = parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty_char(s, ':'))?;
    tally += l;

    let (ty, l) = parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty(s))?;
    tally += l;

    let ((), l) = parse_trim_whitespace(&s[tally..], &|s: &str| parse_ty_char(s, '}'))?;
    tally += l;

    Some((
        Type::AttributeSet {
            attributes: vec![(identifier, ty)],
        },
        tally,
    ))
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
}

// Parse an identifier in an attrset (i.e. attrname)
fn parse_ty_attrset_identifier(s: &str) -> ParseResult<String> {
    fn is_char_identifier(c: &char) -> bool {
        c > &'a' && c < &'z'
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
