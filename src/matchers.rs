use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::multispace0,
    combinator::eof,
    sequence::delimited,
    IResult,
};

#[derive(Debug, Clone)]
pub enum Matcher {
    Pattern(String),
    NonPattern(String),
    And(Box<Matcher>, Box<Matcher>),
    Or(Box<Matcher>, Box<Matcher>),
}

impl Matcher {
    pub fn matches(&self, line: &str) -> bool {
        match self {
            Matcher::Pattern(p) => line.contains(p),
            Matcher::NonPattern(p) => !line.contains(p),
            Matcher::And(a, b) => a.matches(line) && b.matches(line),
            Matcher::Or(a, b) => a.matches(line) || b.matches(line),
        }
    }

    /// Evaluate this matcher over the ENTIRE set of lines.
    /// - Pattern(s): true if ANY line contains s
    /// - NonPattern(s): true if ALL lines do NOT contain s
    pub fn eval(&self, lines: &[String]) -> bool {
        match self {
            Matcher::Pattern(p) => lines.iter().any(|l| l.contains(p)),
            Matcher::NonPattern(p) => lines.iter().all(|l| !l.contains(p)),
            Matcher::And(a, b) => a.eval(lines) && b.eval(lines),
            Matcher::Or(a, b) => a.eval(lines) || b.eval(lines),
        }
    }
}

pub fn run_matchers(matchers: &[Matcher], lines: &[String]) -> bool {
    matchers.iter().any(|m| m.eval(lines))
}


// ---------- Parser combinators for the tiny expression language ----------

// Consume leading whitespace before running `parser`
fn sp<'a, F, O>(mut parser: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    move |input| {
        let (input, _) = multispace0(input)?;
        parser(input)
    }
}

// Consume trailing whitespace AFTER the parser succeeds.
fn lexeme<'a, F, O>(mut parser: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    move |input| {
        let (input, result) = parser(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, result))
    }
}

fn double_quotes(input: &str) -> IResult<&str, String> {
    delimited(
        lexeme(tag("\"")),
        take_while1(|c| c != '\n' && c != '"'),
        lexeme(tag("\"")),
    )(input)
    .map(|(rest, content)| (rest, content.to_string()))
}

fn single_quotes(input: &str) -> IResult<&str, String> {
    delimited(
        lexeme(tag("'")),
        take_while1(|c| c != '\n' && c != '\''),
        lexeme(tag("'")),
    )(input)
    .map(|(rest, content)| (rest, content.to_string()))
}

// Bare term: stop on space, operator, or newline/tab.
// (Quoted strings handle spaces.)
fn term_string(input: &str) -> IResult<&str, String> {
    take_while1(|c| {
        c != '\n' && c != '\t' && c != ' ' && c != '!' && c != '|' && c != '&'
    })(input)
    .map(|(rest, content)| (rest, content.to_string()))
}

fn pattern(input: &str) -> IResult<&str, Matcher> {
    // Allow leading spaces before a pattern; consume trailing after the token.
    let parser = alt((double_quotes, single_quotes, term_string));
    sp(lexeme(parser))(input).map(|(rest, content)| (rest, Matcher::Pattern(content)))
}

fn negated_pattern(input: &str) -> IResult<&str, Matcher> {
    // Allow spaces before '!' and between '!' and the token
    let (input, _) = sp(lexeme(tag("!")))(input)?;
    let (input, content) = alt((double_quotes, single_quotes, term_string))(input)?;
    Ok((input, Matcher::NonPattern(content)))
}

fn parse_negated_pattern(input: &str) -> IResult<&str, Matcher> {
    alt((negated_pattern, pattern))(input)
}

fn parse_and_expr(input: &str) -> IResult<&str, Matcher> {
    let (mut input, mut left) = parse_negated_pattern(input)?;

    loop {
        // Allow spaces before '&' and consume trailing spaces after it.
        if let Ok((new_input, _)) = sp(lexeme(tag("&")))(input) {
            let (new_input, right) = parse_negated_pattern(new_input)?;
            left = Matcher::And(Box::new(left), Box::new(right));
            input = new_input;
        } else {
            break;
        }
    }

    Ok((input, left))
}

fn parse_or_expr(input: &str) -> IResult<&str, Matcher> {
    let (mut input, mut left) = parse_and_expr(input)?;

    loop {
        // Allow spaces before '|' and consume trailing spaces after it.
        if let Ok((new_input, _)) = sp(lexeme(tag("|")))(input) {
            let (new_input, right) = parse_and_expr(new_input)?;
            left = Matcher::Or(Box::new(left), Box::new(right));
            input = new_input;
        } else {
            break;
        }
    }

    Ok((input, left))
}

fn matcher_expr(input: &str) -> IResult<&str, Matcher> {
    // Leading/trailing whitespace around the whole expression is fine
    let (input, _) = multispace0(input)?;
    let (input, result) = parse_or_expr(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = eof(input)?;
    Ok((input, result))
}

pub fn parse_matcher(input: &str) -> Result<Matcher, String> {
    matcher_expr(input)
        .map(|(_, result)| result)
        .map_err(|e| format!("Parse error: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_pattern() {
        let m = parse_matcher("hello").unwrap();
        match m {
            Matcher::Pattern(s) => assert_eq!(s, "hello"),
            _ => panic!("expected Pattern"),
        }
    }

    #[test]
    fn parse_negated_pattern() {
        let m = parse_matcher("!error").unwrap();
        match m {
            Matcher::NonPattern(s) => assert_eq!(s, "error"),
            _ => panic!("expected NonPattern"),
        }
    }

    #[test]
    fn parse_quotes_and_spaces() {
        let m1 = parse_matcher("\"foo bar\"").unwrap();
        let m2 = parse_matcher("'baz qux'").unwrap();
        match m1 {
            Matcher::Pattern(s) => assert_eq!(s, "foo bar"),
            _ => panic!("expected Pattern"),
        }
        match m2 {
            Matcher::Pattern(s) => assert_eq!(s, "baz qux"),
            _ => panic!("expected Pattern"),
        }
    }

    #[test]
    fn and_has_higher_precedence_than_or() {
        // a & b | c  ==> (a & b) | c
        let m = parse_matcher("a & b | c").unwrap();
        match m {
            Matcher::Or(left, right) => {
                match *right {
                    Matcher::Pattern(ref s) if s == "c" => {}
                    _ => panic!("expected right to be Pattern(\"c\")"),
                }
                match *left {
                    Matcher::And(ref a, ref b) => {
                        match **a {
                            Matcher::Pattern(ref s) if s == "a" => {}
                            _ => panic!("expected a"),
                        }
                        match **b {
                            Matcher::Pattern(ref s) if s == "b" => {}
                            _ => panic!("expected b"),
                        }
                    }
                    _ => panic!("expected left to be And"),
                }
            }
            _ => panic!("expected Or"),
        }
    }

    #[test]
    fn matcher_evaluation_and_or_negation() {
        let m = parse_matcher("ok & !error | ready").unwrap();
        let lines1 = vec!["status: ok".to_string()];
        let lines2 = vec!["status: ok", "contains error"]
            .into_iter()
            .map(String::from)
            .collect::<Vec<_>>();
        let lines3 = vec!["system ready".to_string()];

        assert!(run_matchers(&[m.clone()], &lines1));  // ok & !error
        assert!(!run_matchers(&[m.clone()], &lines2)); // fails due to 'error' present
        assert!(run_matchers(&[m], &lines3));          // 'ready' matches via OR
    }

    #[test]
    fn run_matchers_returns_true_if_any_matcher_matches_any_line() {
        let m1 = parse_matcher("alpha").unwrap();
        let m2 = parse_matcher("beta").unwrap();
        let lines = vec!["zzz beta qqq".to_string()];
        assert!(run_matchers(&[m1, m2], &lines));
    }
}
