//!
//! `regex-intersect` is a library for finding out if two regexes have a non-empty intersection.
//!
//! For example `.*` and `abc` obviously intersect (because `.*` matches everything, including the string "abc").
//! However, `d.*` and `abc` or `a[h-z]c` and `abc` do not, and could never intersect, because `abc` could never start with a `d` in the first case,
//! and `b` is not in the set `[h-z]` in the second case.
//!
//!
//! ## Examples
//!
//! Check if non empty:
//!
//! ```
//! use regex_intersect::non_empty;
//! assert!(non_empty("a.*", "ab.*cd").expect("regex expressions should parse"))
//!```
//!
//! ## Tracing
//!
//! This libary incorporates tracing, which helps follow the way it operates. Tracing is not enabled by
//! default but you can enable it:
//!
//! ```toml
//! regex-intersect = { version="0.1", features=["tracing"] }
//! ```
//!
#![warn(missing_docs)]
#![allow(clippy::similar_names)]

use regex_syntax::hir::{Class, ClassUnicode, Hir, HirKind, Literal, RepetitionKind};
use regex_syntax::ParserBuilder;
use std::borrow::BorrowMut;
use thiserror::Error;

/// An error in intersecting expressions
#[derive(Debug, Error)]
pub enum IntersectError {
    /// An error parsing an expression
    #[error("error while parsing expression: {0}")]
    Parse(#[from] regex_syntax::Error),
}

fn maybe_trim<'a>(
    x: &'a Hir,
    y: &'a Hir,
    xs: &'a [Hir],
    ys: &'a [Hir],
    reversed: bool,
) -> Option<(&'a [Hir], &'a [Hir])> {
    if x.eq(y) {
        trim_slice(xs, ys, reversed)
    } else {
        None
    }
}
fn trim_slice<'a>(
    left: &'a [Hir],
    right: &'a [Hir],
    reversed: bool,
) -> Option<(&'a [Hir], &'a [Hir])> {
    match (left, right) {
        ([xs @ .., x], [ys @ .., y]) | ([x, xs @ ..], [y, ys @ ..])
            if reversed && x.is_literal() && y.is_literal() =>
        {
            maybe_trim(x, y, xs, ys, reversed)
        }
        ([x, xs @ ..], [y, ys @ ..]) if !reversed && x.is_literal() && y.is_literal() => {
            maybe_trim(x, y, xs, ys, reversed)
        }
        _ => Some((left, right)),
    }
}

fn trim<'a>(left: &'a Hir, right: &'a Hir, reversed: bool) -> Option<(Hir, Hir)> {
    match (left.kind(), right.kind()) {
        (HirKind::Concat(left), HirKind::Concat(right)) => {
            trim_slice(left.as_slice(), right.as_slice(), reversed)
                .map(|(left, right)| (Hir::concat(left.to_vec()), Hir::concat(right.to_vec())))
        }
        _ => Some((left.clone(), right.clone())),
    }
}

#[cfg_attr(feature="tracing", tracing::instrument(skip_all, fields(
  left = format!("{}", left.iter().map(|f| format!("{}", f)).collect::<Vec<_>>().join(", ")),
  right = format!("{:?}", right.iter().map(|f| format!("{}", f)).collect::<Vec<_>>().join(", ")))))]
fn concats(left: &[Hir], right: &[Hir]) -> bool {
    match (left, right) {
        // nothing matches with nothing
        (&[], &[]) => true,
        // nothing only matches *something* if that something is 'zero or X' (meaning, can match nothing)
        (&[], [x, ..]) | ([x, ..], &[]) if matches!(x.kind(), HirKind::Repetition(r) if r.kind == RepetitionKind::ZeroOrMore || r.kind == RepetitionKind::ZeroOrOne) => {
            true
        }
        // last item, and we're done only if repetition is one or more (+), which makes it symmetric to the case with zero or more above.
        ([x], [rep, ..]) | ([rep, ..], [x]) if matches!(rep.kind(), HirKind::Repetition(r) if r.kind == RepetitionKind::OneOrMore) => {
            exp(rep, x)
        }
        // otherwise, keep moving recursively
        (xall @ [x, xs @ ..], yall @ [y, ys @ ..]) => {
            match (x.kind(), y.kind()) {
                // need to incorporate the class of repetition (zeroorone, one, many, etc.)
                (HirKind::Repetition(_), _) => exp(x, y) && concats(xall, ys),
                (_, HirKind::Repetition(_)) => exp(x, y) && concats(xs, yall),
                (_, _) => exp(x, y) && concats(xs, ys),
            }
        }
        _ => false,
    }
}

#[cfg_attr(feature = "tracing", tracing::instrument)]
fn unicode_range(left: &ClassUnicode, right: &ClassUnicode) -> bool {
    let mut rl = left.clone();
    rl.borrow_mut().intersect(right);
    rl.iter().count() > 0
}

#[cfg_attr(feature = "tracing", tracing::instrument)]
fn literal(left: &Literal, right: &Literal) -> bool {
    left.eq(right)
}

#[cfg_attr(feature="tracing", tracing::instrument(skip_all, fields(left = format!("{}", left), right = format!("{}", right))))]
fn exp(left: &Hir, right: &Hir) -> bool {
    match (left.kind(), right.kind()) {
        (HirKind::Concat(xs), HirKind::Concat(ys)) => concats(xs, ys),
        (HirKind::Concat(xs), _) => concats(xs, &[right.clone()]),
        (_, HirKind::Concat(ys)) => concats(&[left.clone()], ys),
        (HirKind::Class(Class::Unicode(left)), HirKind::Class(Class::Unicode(right))) => {
            unicode_range(left, right)
        }
        (HirKind::Class(Class::Unicode(cls)), HirKind::Literal(Literal::Unicode(lit)))
        | (HirKind::Literal(Literal::Unicode(lit)), HirKind::Class(Class::Unicode(cls))) => {
            cls.iter().any(|c| *lit >= c.start() && *lit <= c.end())
        }
        (HirKind::Literal(left), HirKind::Literal(right)) => literal(left, right),
        (HirKind::Empty, HirKind::Repetition(rep)) | (HirKind::Repetition(rep), HirKind::Empty)
            if rep.kind == RepetitionKind::ZeroOrMore || rep.kind == RepetitionKind::ZeroOrOne =>
        {
            true
        }
        (HirKind::Repetition(left), HirKind::Repetition(right)) => exp(&left.hir, &right.hir),
        (HirKind::Repetition(left), _) => exp(&left.hir, right),
        (_, HirKind::Repetition(right)) => exp(left, &right.hir),
        (HirKind::Empty, HirKind::Empty) => true,
        _tup => {
            #[cfg(feature = "tracing")]
            tracing::warn!("not found: {:?}", _tup);

            false
        }
    }
}

#[cfg_attr(feature = "tracing", tracing::instrument)]
fn hir(exp: &str) -> Result<Hir, IntersectError> {
    let mut parser = ParserBuilder::new().allow_invalid_utf8(true).build();
    Ok(parser.parse(exp)?)
}

///
/// Check if `left` and `right` contains a non empty intersection.
///
/// ## Examples
///
/// ```
/// use regex_intersect::non_empty;
/// assert!(non_empty("a.*", "ab.*cd").expect("regex expressions should parse"))
///```
///
/// ## Errors
/// Returns an error if cannot parse one of the expressions
///
#[cfg_attr(feature = "tracing", tracing::instrument)]
pub fn non_empty(left: &str, right: &str) -> Result<bool, IntersectError> {
    let trimmed =
        trim(&hir(left)?, &hir(right)?, false).and_then(|(left, right)| trim(&left, &right, true));
    Ok(trimmed.map_or(false, |(hl, hr)| exp(&hl, &hr)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    const NON_EMPTY: &[(&str, &[&str])] = &[
        ("abcd", &["abcd", "....", "[a-d]*"]),
        ("pqrs", &[".qrs", "p.rs", "pq.s", "pqr."]),
        (".*", &["asdklfj", "jasdfh", "asdhfajfh", "asdflkasdfjl"]),
        ("d*", &["[abcd][abcd]", "d[a-z]+", ".....", "[d]*"]),
        ("[a-p]+", &["[p-z]+", "apapapaapapapap", ".*", "abcdefgh*"]),
        (
            "abcd[a-c]z+",
            &["abcd[b-d][yz]*", "abcdazzzz", "abcdbzzz", "abcdcz"],
        ),
        (".*\\\\", &[".*", "asdfasdf\\\\"]),
        (".a.a", &["b.b.", "c.c.", "d.d.", "e.e."]),
        (
            ".*.*.*.*.*.*.*.*.*.*.*.*.*.*.*",
            &[".*.*.*.*.*.*.*.*.*.*.*"],
        ),
        ("foo.*bar", &["foobar", "fooalkdsjfbar"]),
        ("[a-b]+c", &["[b-c]+"]),
        (".xyz.", &[".*pqr.*"]), // this was moved from the 'EMPTY' testset because it didn't make sense there
    ];
    const EMPTY: &[(&str, &[&str])] = &[
        ("abcd", &["lsdfhda", "abcdla", "asdlfk", "ksdfj"]),
        ("[a-d]+", &["xyz", "p+", "[e-f]+"]),
        ("[0-9]*", &["[a-z]", ".\\*"]),
        ("ab+", &["a", "b", "abc"]),
        ("mamama.*", &["dadada.*", "nanana.*"]),
        (".*mamama", &[".*dadada", ".*nanana"]),
        (".xyz.", &["paaap"]),
        (".*.*.*.*f", &[".*.*.*.*g"]),
    ];

    const EXTRAS_NON_EMPTY: &[(&str, &[&str])] = &[
        ("fabcd", &["f.*"]),
        ("f.*abcd", &["fd.*"]),
        ("f[a-n]*abcd", &["fd.*"]),
    ];

    const EXTRAS_EMPTY: &[(&str, &[&str])] = &[
        ("fabcd", &["fd.*"]),
        ("f.*abcd", &["fd.*z"]),
        ("f[a-n]*abcd", &["fd.*z"]),
    ];

    #[test]
    fn test_error() {
        let err = format!("{}", non_empty("*", ".*g").unwrap_err());
        assert_eq!(
            r#"error while parsing expression: regex parse error:
    *
    ^
error: repetition operator missing expression"#,
            err
        );
    }

    #[test]
    fn test_one() {
        assert!(!non_empty(".*f", ".*g").unwrap());
    }

    #[test]
    fn test_non_empty() {
        for (left, rights) in NON_EMPTY {
            for right in rights.iter() {
                assert!(non_empty(left, right).unwrap());
                assert!(non_empty(right, left).unwrap());
            }
        }
    }

    #[test]
    fn test_empty() {
        for (left, rights) in EMPTY {
            for right in rights.iter() {
                assert!(!non_empty(left, right).unwrap());
                assert!(!non_empty(right, left).unwrap());
            }
        }
    }

    #[test]
    fn test_extras_non_empty() {
        for (left, rights) in EXTRAS_NON_EMPTY {
            for right in rights.iter() {
                assert!(non_empty(left, right).unwrap());
                assert!(non_empty(right, left).unwrap());
            }
        }
    }
    #[test]
    fn test_extras_empty() {
        for (left, rights) in EXTRAS_EMPTY {
            for right in rights.iter() {
                assert!(!non_empty(left, right).unwrap());
                assert!(!non_empty(right, left).unwrap());
            }
        }
    }
}
