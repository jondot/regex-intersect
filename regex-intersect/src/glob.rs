use crate::IntersectError;
use std::str::FromStr;

use globset;

/// Find out if two glob expressions intersect
///
/// # Errors
///
/// This function will return an error if any of the expressions are illegal
pub fn non_empty(glob_left: &str, glob_right: &str) -> Result<bool, IntersectError> {
    let binding =
        globset::Glob::from_str(glob_left).map_err(|e| IntersectError::Error(e.to_string()))?;
    let left = binding
        .regex()
        .trim_start_matches("(?-u)^")
        .trim_end_matches('$');
    let binding =
        globset::Glob::from_str(glob_right).map_err(|e| IntersectError::Error(e.to_string()))?;
    let right = binding
        .regex()
        .trim_start_matches("(?-u)^")
        .trim_end_matches('$');
    crate::non_empty(left, right)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_glob_non_empty() {
        assert!(non_empty("foo/bar/**", "foo/bar/*/baz").unwrap());
    }
    #[test]
    fn test_glob_empty() {
        assert!(!non_empty("foo/*/baz", "foo/*/qux").unwrap());
    }
}
