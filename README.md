Regex Intersect
===============

[<img alt="github" src="https://img.shields.io/badge/github-jondot/regex_intersect-8dagcb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/jondot/regex-intersect)
[<img alt="crates.io" src="https://img.shields.io/crates/v/regex-intersect.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/regex-intersect)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-regex_intersect-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/regex-intersect)
[<img alt="build status" src="https://img.shields.io/github/workflow/status/jondot/regex-intersect/Build/master?style=for-the-badge" height="20">](https://github.com/jondot/regex-intersect/actions?query=branch%3Amaster)

This is a Rust library that finds out if two regexes have a non-empty intersection.

## Dependency

```toml
[dependencies]
regex-intersect = "1.1.0"
```

For most recent version see [crates.io](https://crates.io/crates/regex-intersect)


## Usage

Import and use `non_empty`:

```rust
use regex_intersect::non_empty;
assert!(non_empty("a.*", "ab.*cd").expect("regex expressions should parse"))
```


# Copyright

Copyright (c) 2022 [@jondot](http://twitter.com/jondot). See [LICENSE](LICENSE.txt) for further details.
