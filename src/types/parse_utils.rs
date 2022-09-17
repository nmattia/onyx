pub type ParseResult<T> = Option<(T, usize)>;

// Run the parser but don't fail if there's leftover string.
pub fn run_parser_leftover<'a, T>(
    f: &'a dyn Fn(&str) -> ParseResult<T>,
    s: &'a str,
) -> Result<(T, &'a str), String> {
    match f(s) {
        Some((res, l)) => Ok((res, &s[l..])),
        None => Err("Failed to parse".to_string()),
    }
}

// Run the parser and fail if there's leftover string.
pub fn run_parser<T>(f: &dyn Fn(&str) -> ParseResult<T>, s: &str) -> Result<T, String> {
    let (res, leftover) = run_parser_leftover(f, s)?;

    if leftover.len() != 0 {
        return Err(format!(
            "not all input was consumed: full: '{}', leftover: '{}'",
            s, leftover
        ));
    }

    Ok(res)
}

/* Helpers */

pub fn parse_trim_whitespace<T>(s: &str, f: &dyn Fn(&str) -> ParseResult<T>) -> ParseResult<T> {
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

pub fn parse_ty_char(s: &str, c: char) -> ParseResult<()> {
    if s.chars().nth(0) != Some(c) {
        None
    } else {
        Some(((), 1))
    }
}

pub fn parse_ty_string(s: &str, t: &str) -> ParseResult<()> {
    if s.starts_with(t) {
        Some(((), t.len()))
    } else {
        None
    }
}

pub fn parse_ty_char_(s: &str) -> ParseResult<char> {
    match s.chars().nth(0) {
        Some(c) => Some((c, 1)),
        None => None,
    }
}

#[test]
pub fn test_parse_many() {
    let s = "aaa";
    assert_eq!(
        parse_many(&s, &|s| parse_ty_char_(s)).unwrap().0,
        vec!['a', 'a', 'a'],
    );
}

// Call the parser as many times as possible. Note: always matches (may return empty vec).
pub fn parse_many<T>(s: &str, f: &dyn Fn(&str) -> ParseResult<T>) -> ParseResult<Vec<T>> {
    let mut tally = 0;

    let mut vec: Vec<T> = vec![];

    while let Some((res, l)) = f(&s[tally..]) {
        tally += l;

        vec.push(res);
    }

    Some((vec, tally))
}

pub fn parse_try<T>(s: &str, f: &dyn Fn(&str) -> ParseResult<T>) -> ParseResult<Option<T>> {
    match f(&s) {
        None => Some((None, 0)),
        Some((res, l)) => Some((Some(res), l)),
    }
}

#[test]
fn test_parse_joined() {
    let s = "a,b,c";
    assert_eq!(
        parse_joined(&s, &|s| parse_ty_char_(s), ",").unwrap().0,
        vec!['a', 'b', 'c'],
    );
}

pub fn parse_joined<T: std::fmt::Debug>(
    s: &str,
    f: &dyn Fn(&str) -> ParseResult<T>,
    joiner: &str,
) -> ParseResult<Vec<T>> {
    let mut tally = 0;

    match f(&s) {
        None => Some((vec![], 0)),
        Some((first, l)) => {
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
    }
}
