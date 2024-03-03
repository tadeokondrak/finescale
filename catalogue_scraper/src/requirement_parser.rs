use std::fmt::{self, Write};
use std::iter;

mod data;

// TODO: preprocessing
// Would contain rules like "Math 30, 30-1, or 30- 2" => "Math 30, 30-1, or
// 30-2"

// TODO: list parsing functions
// They need to parse only lists that consistently use the same format
// Good: "A, B, C, D, or E"
// Good: "A, B, C, D or E"
// Good: "A, or B"
// Bad (as one list): "A or B or C or D or E"
// Bad (as one list): "A or B, C, D or E"
// It would obviously be inefficient to do this for a programming language,
// but I think this is relatively maintainable and should hopefully not be
// unusably slow.

// I also wonder if we need to define precedence for punctuation.
// A comma binds tighter than a semicolon.

#[derive(Clone, PartialEq, Eq)]
pub struct Course {
    topic: String,
    number: String,
}

impl fmt::Display for Course {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.topic, self.number)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Expr {
    Equivalent,
    And(Box<[Expr; 2]>),
    Or(Box<[Expr; 2]>),
    Any(Vec<Expr>),
    All(Vec<Expr>),
    Course(Course),
    ConsentOf(Entity),
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Equivalent => f.write_str("(equivalent)"),
            Expr::Any(args) => {
                write!(f, "(any")?;
                for arg in args {
                    write!(f, " {arg:?}")?;
                }
                f.write_char(')')?;
                Ok(())
            }
            Expr::All(args) => {
                write!(f, "(all")?;
                for arg in args {
                    write!(f, " {arg:?}")?;
                }
                f.write_char(')')?;
                Ok(())
            }
            Expr::Course(arg0) => write!(f, "({arg0})"),
            Expr::ConsentOf(arg0) => write!(f, "(consent of {arg0})"),
            Expr::And(exprs) => {
                write!(f, "(and {:?} {:?})", exprs[0], exprs[1])?;
                Ok(())
            }
            Expr::Or(exprs) => {
                write!(f, "(or {:?} {:?})", exprs[0], exprs[1])?;
                Ok(())
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Entity {
    Other,
    Department,
    Instructor,
    College,
}

impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Entity::Department => write!(f, "department"),
            Entity::Instructor => write!(f, "instructor"),
            Entity::College => write!(f, "instructor"),
            Entity::Other => write!(f, "[other]"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    Unrecognized,
    DidNotReachEndOfInput { parsed_so_far: Expr },
}

pub fn parse_requirement(s: &str) -> Result<Expr, ParseError> {
    let tokens = tokenize(s).collect::<Vec<_>>();
    let mut parser = Parser {
        tokens: &tokens,
        pos: 0,
    };
    let req = parser
        .parse_expr_one(PrecedenceLevel::BareCommaList)
        .ok_or(ParseError::Unrecognized)?;
    if !parser.at_eof() {
        Err(ParseError::DidNotReachEndOfInput { parsed_so_far: req })
    } else {
        Ok(req)
    }
}

fn tokenize(mut s: &str) -> impl Iterator<Item = &str> {
    enum TokenKind {
        Whitespace,
        NotWhitespace,
    }

    // TODO remove duplication
    fn token_length(s: &str) -> (TokenKind, usize) {
        let mut it = s.chars();
        let Some(c) = it.next() else {
            return (TokenKind::NotWhitespace, 0);
        };

        if c.is_alphabetic() {
            let mut len = c.len_utf8();
            while let Some(c) = it.next() {
                if c.is_alphabetic() {
                    len += c.len_utf8();
                } else {
                    break;
                }
            }
            return (TokenKind::NotWhitespace, len);
        }

        if c.is_numeric() {
            let mut len = c.len_utf8();
            while let Some(c) = it.next() {
                if c.is_numeric() {
                    len += c.len_utf8();
                } else {
                    break;
                }
            }
            return (TokenKind::NotWhitespace, len);
        }

        if c.is_whitespace() {
            let mut len = c.len_utf8();
            while let Some(c) = it.next() {
                if c.is_whitespace() {
                    len += c.len_utf8();
                } else {
                    break;
                }
            }
            return (TokenKind::Whitespace, len);
        }

        (TokenKind::NotWhitespace, c.len_utf8())
    }

    iter::from_fn(move || loop {
        let (kind, len) = token_length(s);
        if len == 0 {
            break None;
        } else {
            let (text, rest) = s.split_at(len);
            s = rest;
            match kind {
                TokenKind::Whitespace => continue,
                TokenKind::NotWhitespace => break Some(text),
            }
        }
    })
}

struct Parser<'a> {
    tokens: &'a [&'a str],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn at_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn nth_token(&self, n: usize) -> &'a str {
        self.tokens.get(self.pos + n).copied().unwrap_or_default()
    }

    fn bump(&mut self) -> &'a str {
        let token = self.nth_token(0);
        self.advance(1);
        token
    }

    fn advance(&mut self, n: usize) {
        self.pos += n;
    }

    fn at(&self, token: &str) -> bool {
        self.current_token() == token
    }

    fn eat(&mut self, token: &str) -> bool {
        if self.at(token) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_multiple(&mut self, tokens: &[&str]) -> bool {
        for (i, token) in tokens.iter().copied().enumerate() {
            if self.nth_token(i) != token {
                return false;
            }
        }
        self.advance(tokens.len());
        true
    }

    fn current_token(&self) -> &'a str {
        self.nth_token(0)
    }

    fn fork(&self) -> Parser<'a> {
        Parser {
            tokens: self.tokens,
            pos: self.pos,
        }
    }

    fn eat_any(&mut self, tokens: &[&str]) -> bool {
        if self.at_any(tokens) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn at_any(&self, tokens: &[&str]) -> bool {
        self.nth_at_any(0, tokens)
    }

    fn nth_at_any(&self, n: usize, tokens: &[&str]) -> bool {
        tokens.contains(&self.nth_token(n))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum PrecedenceLevel {
    BareCommaList,
    AndList,
    OrList,
    AndExpr,
    OrExpr,
    Base,
}

impl PrecedenceLevel {
    #[allow(unused)]
    const fn above(self) -> PrecedenceLevel {
        match self {
            PrecedenceLevel::BareCommaList => panic!(),
            PrecedenceLevel::AndList => PrecedenceLevel::BareCommaList,
            PrecedenceLevel::OrList => PrecedenceLevel::AndList,
            PrecedenceLevel::AndExpr => PrecedenceLevel::OrList,
            PrecedenceLevel::OrExpr => PrecedenceLevel::AndExpr,
            PrecedenceLevel::Base => PrecedenceLevel::OrExpr,
        }
    }

    const fn below(self) -> PrecedenceLevel {
        match self {
            PrecedenceLevel::BareCommaList => PrecedenceLevel::AndList,
            PrecedenceLevel::AndList => PrecedenceLevel::OrList,
            PrecedenceLevel::OrList => PrecedenceLevel::AndExpr,
            PrecedenceLevel::AndExpr => PrecedenceLevel::OrExpr,
            PrecedenceLevel::OrExpr => PrecedenceLevel::Base,
            PrecedenceLevel::Base => panic!(),
        }
    }
}

// Need X and Y to bind tighter than X, and Y

impl<'a> Parser<'a> {
    fn parse_expr_one(&mut self, precedence: PrecedenceLevel) -> Option<Expr> {
        self.parse_expr(precedence).into_iter().nth(0)
    }

    #[rustfmt::skip]
    fn parse_expr(
        &mut self,
        precedence: PrecedenceLevel,
    ) -> Vec<Expr> {
        fn c<'a>(
            parser: fn(&mut Parser<'a>) -> Option<Expr>,
        ) -> impl Fn(&mut Parser<'a>) -> Vec<Expr> {
            move |p| Vec::from_iter(parser(p))
        }

        match precedence {
            PrecedenceLevel::BareCommaList => self.run_parsers(&[
                &c(Parser::parse_comma_separated_requirement_list),
                &c(|p| p.parse_expr_one(PrecedenceLevel::below(PrecedenceLevel::BareCommaList))),
            ]),
            PrecedenceLevel::AndList => self.run_parsers(&[
                &c(|p| p.parse_expr_list_with_shared_topic(ListMode::All, Expr::All)),
                &c(|p| p.parse_expr_list(ListMode::All, Expr::All, PrecedenceLevel::OrList, PrecedenceLevel::OrList)),
                &c(|p| p.parse_expr_one(PrecedenceLevel::below(PrecedenceLevel::OrList))),
            ]),
            PrecedenceLevel::OrList => self.run_parsers(&[
                &c(|p| p.parse_expr_list_with_shared_topic(ListMode::Any, Expr::Any)),
                &c(|p| p.parse_expr_list(ListMode::Any, Expr::Any, PrecedenceLevel::Base, PrecedenceLevel::Base)),
                &c(|p| p.parse_expr_one(PrecedenceLevel::below(PrecedenceLevel::OrList))),
            ]),
            PrecedenceLevel::AndExpr => self.run_parsers(&[
                &c(|p| p.parse_binary_expr_with_shared_topic(ListMode::All, Expr::And)),
                &c(|p| p.parse_binary_expr(PrecedenceLevel::AndExpr, ListMode::All, Expr::And)),
                &c(|p| p.parse_expr_one(PrecedenceLevel::below(PrecedenceLevel::AndExpr))),
            ]),
            PrecedenceLevel::OrExpr => self.run_parsers(&[
                &c(|p| p.parse_binary_expr_with_shared_topic(ListMode::Any, Expr::Or)),
                &c(|p| p.parse_binary_expr(PrecedenceLevel::OrExpr, ListMode::Any, Expr::Or)),
                &c(|p| p.parse_expr_one(PrecedenceLevel::below(PrecedenceLevel::OrExpr))),
            ]),
            PrecedenceLevel::Base => self.run_parsers(&[
                &c(Parser::parse_prefixed_binary_or_expr_with_shared_topic),
                &c(|p| p.parse_prefixed_or_expr_list(PrecedenceLevel::Base, PrecedenceLevel::Base)),
                &c(|p| p.parse_prefixed_binary_or_expr(PrecedenceLevel::OrExpr)),
                &c(Parser::parse_lone_course_expr),
                &c(Parser::parse_consent_of_entity_expr),
                &c(Parser::parse_equivalent_expr),
            ]),
        }
    }

    fn run_parsers(&mut self, parsers: &[&dyn Fn(&mut Parser<'a>) -> Vec<Expr>]) -> Vec<Expr> {
        parsers
            .into_iter()
            .flat_map(|parser| parser(&mut self.fork()))
            .collect()
    }

    fn parse_prefixed_binary_or_expr(&mut self, precedence: PrecedenceLevel) -> Option<Expr> {
        if !self.eat_any_of_or_one_of() {
            return None;
        }
        self.parse_binary_expr(precedence, ListMode::Any, Expr::Or)
    }

    fn parse_binary_expr(
        &mut self,
        precedence: PrecedenceLevel,
        list_mode: ListMode,
        wrapper: fn(Box<[Expr; 2]>) -> Expr,
    ) -> Option<Expr> {
        let Some(lhs) = self.parse_expr_one(PrecedenceLevel::below(precedence)) else {
            return None;
        };
        if !self.eat_any(list_mode.separators()) {
            return None;
        }
        let Some(rhs) = self.parse_expr_one(precedence) else {
            return None;
        };
        Some(wrapper(Box::new([lhs, rhs])))
    }

    fn parse_binary_expr_with_shared_topic(
        &mut self,
        list_mode: ListMode,
        wrapper: fn(Box<[Expr; 2]>) -> Expr,
    ) -> Option<Expr> {
        let Some(lhs) = self.parse_course() else {
            return None;
        };
        if !self.eat_any(list_mode.separators()) {
            return None;
        }
        let number = self.current_token();
        if !number.starts_with(char::is_numeric) {
            return None;
        }
        self.bump();

        let rhs = Course {
            topic: lhs.topic.clone(),
            number: number.to_owned(),
        };

        Some(wrapper(Box::new([Expr::Course(lhs), Expr::Course(rhs)])))
    }

    fn parse_prefixed_binary_or_expr_with_shared_topic(&mut self) -> Option<Expr> {
        if !self.eat_any_of_or_one_of() {
            return None;
        }
        self.parse_binary_expr_with_shared_topic(ListMode::Any, Expr::Or)
    }

    fn parse_prefixed_or_expr_list(
        &mut self,
        lhs_precedence: PrecedenceLevel,
        rhs_precedence: PrecedenceLevel,
    ) -> Option<Expr> {
        if !self.eat_any_of_or_one_of() {
            return None;
        }
        self.parse_expr_list(ListMode::Any, Expr::Any, lhs_precedence, rhs_precedence)
    }

    fn parse_comma_separated_requirement_list(&mut self) -> Option<Expr> {
        let Some(first_req) = self.parse_expr_one(PrecedenceLevel::OrExpr) else {
            return None;
        };
        let mut reqs = vec![first_req];
        while self.eat(",") {
            let Some(next_req) = self.parse_expr_one(PrecedenceLevel::OrExpr) else {
                break;
            };
            reqs.push(next_req);
        }
        Some(reqs)
            .filter(|reqs| reqs.len() >= 2 && self.at_eof())
            .map(Expr::All)
    }

    fn parse_expr_list(
        &mut self,
        list_mode: ListMode,
        wrapper: fn(Vec<Expr>) -> Expr,
        lhs_precedence: PrecedenceLevel,
        rhs_precedence: PrecedenceLevel,
    ) -> Option<Expr> {
        let Some(first_req) = self.parse_expr_one(lhs_precedence) else {
            return None;
        };
        let mut reqs = vec![first_req];
        loop {
            let at_comma = self.at(",");
            let at_sep = self.at_any(list_mode.separators());
            let at_oxford_sep =
                at_comma && self.nth_at_any(1, list_mode.separators_allowing_oxford_comma());
            if !at_comma && !at_sep && !at_oxford_sep {
                return None;
            }

            self.bump();
            if at_oxford_sep {
                self.bump();
            }

            let Some(next_req) = self.parse_expr_one(rhs_precedence) else {
                return None;
            };
            reqs.push(next_req);
            if at_sep || at_oxford_sep {
                break Some(reqs).filter(|reqs| reqs.len() >= 2).map(wrapper);
            }
        }
    }

    fn eat_any_of_or_one_of(&mut self) -> bool {
        if self.eat_multiple(&["Any", "of"])
            || self.eat_multiple(&["any", "of"])
            || self.eat_multiple(&["One", "of"])
            || self.eat_multiple(&["one", "of"])
        {
            self.eat(":");
            true
        } else {
            false
        }
    }

    fn parse_expr_list_with_shared_topic(
        &mut self,
        list_mode: ListMode,
        wrapper: fn(Vec<Expr>) -> Expr,
    ) -> Option<Expr> {
        self.parse_list_of_courses_with_shared_topic(list_mode)
            .map(|courses| courses.into_iter().map(Expr::Course).collect())
            .map(wrapper)
    }

    fn parse_list_of_courses_with_shared_topic(
        &mut self,
        list_mode: ListMode,
    ) -> Option<Vec<Course>> {
        let Some(Course {
            topic,
            number: first_number,
        }) = self.parse_course()
        else {
            return None;
        };

        let mut courses = vec![Course {
            topic: topic.clone(),
            number: first_number,
        }];

        loop {
            let matched1 = self.at_any(list_mode.separators());
            let matched2 =
                self.at(",") && self.nth_at_any(1, list_mode.separators_allowing_oxford_comma());
            if !matched1 && !matched2 && !self.at(",") {
                return None;
            }

            self.bump();
            if matched2 {
                self.bump();
            }

            let next_number = self.current_token();
            if !next_number.starts_with(char::is_numeric) {
                return None;
            }
            self.bump();

            courses.push(Course {
                topic: topic.clone(),
                number: next_number.to_owned(),
            });

            if matched1 || matched2 {
                break Some(courses).filter(|reqs| reqs.len() >= 2);
            }
        }
    }

    fn parse_lone_course_expr(&mut self) -> Option<Expr> {
        self.parse_course().map(Expr::Course)
    }

    fn parse_course(&mut self) -> Option<Course> {
        let mut course_topic = self.current_token().to_owned();
        if data::UALBERTA_TOPICS_WITH_SPACES_PREFIXES.contains(&course_topic.as_str()) {
            course_topic = format!("{} {}", self.nth_token(0), self.nth_token(1));
            self.bump();
        } else if !data::UALBERTA_TOPICS.contains(&course_topic.as_str()) {
            return None;
        }
        self.bump();

        let course_number = self.current_token();
        if !course_number.starts_with(char::is_numeric) {
            return None;
        }
        self.bump();

        Some(Course {
            topic: course_topic,
            number: course_number.to_owned(),
        })
    }

    fn parse_consent_of_entity_expr(&mut self) -> Option<Expr> {
        if !self.eat_multiple(&["consent", "of"])
            && !self.eat_multiple(&["Consent", "of"])
            && !self.eat_multiple(&["permission", "of"])
            && !self.eat_multiple(&["Permission", "of"])
        {
            return None;
        }
        self.eat("the");
        let entity = match self.bump() {
            "department" | "Department" => Entity::Department,
            "instructor" | "Instructor" => Entity::Instructor,
            "college" | "College" => Entity::College,
            _ => Entity::Other,
        };
        Some(Expr::ConsentOf(entity))
    }

    fn parse_equivalent_expr(&mut self) -> Option<Expr> {
        if !self.eat("equivalent") {
            return None;
        }
        Some(Expr::Equivalent)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ListMode {
    All,
    Any,
}

impl ListMode {
    fn separators(self) -> &'static [&'static str] {
        match self {
            ListMode::All => &["and", "et"],
            ListMode::Any => &["or", "ou"],
        }
    }

    fn separators_allowing_oxford_comma(self) -> &'static [&'static str] {
        match self {
            ListMode::All => &["and"],
            ListMode::Any => &["or"],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(input: &str, output: Expect) {
        let res = parse_requirement(input);
        if let Ok(req) = res {
            output.assert_eq(&format!("{req:?}"));
        } else {
            output.assert_eq(&format!("{res:?}"));
        }
    }

    #[test]
    fn parse_consent_of_the_department_requirement() {
        check(
            "consent of the department",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (consent of department) })"],
        );
    }

    #[test]
    fn parse_simple_course_requirement() {
        check(
            "CMPUT 174",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (CMPUT 174) })"],
        );
    }

    #[test]
    fn parse_any_of_course_requirements() {
        check(
            "CMPUT 174 or CMPUT 274",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (CMPUT 174) })"],
        );
        check(
            "CMPUT 174, CMPUT 274, CMPUT 175 or CMPUT 275",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (CMPUT 174) })"],
        );
    }

    #[test]
    fn parse_any_of_course_requirements_with_prefix() {
        check(
            "one of CMPUT 175 or 275",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (or (CMPUT 175) (CMPUT 275)) })"],
        );
        check(
            "one of CMPUT 175 or CMPUT 275",
            expect!["Err(Unrecognized)"],
        );
        check(
            "any of CMPUT 175 or CMPUT 275",
            expect!["Err(Unrecognized)"],
        );
    }

    #[test]
    fn parse_any_of_course_requirements_without_repeated_topic() {
        check(
            "CMPUT 174 or 274",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (or (CMPUT 174) (CMPUT 274)) })"],
        );
    }

    #[test]
    fn parse_all_of_course_requirements() {
        check(
            "ECON 101 and ECON 102",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 101) })"],
        );
    }

    #[test]
    fn parse_all_of_course_requirements_without_repeated_topic() {
        check(
            "ECON 101 and 102",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (all (ECON 101) (ECON 102)) })"],
        );
    }

    #[test]
    fn parse_top_level_comma_separated_list() {
        check(
            "ACCTG 614, FIN 501",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ACCTG 614) })"],
        );
        check(
            "ACCTG 614, ACCTG 610, FIN 501",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ACCTG 614) })"],
        );
        check(
            "ACCTG 614 or 610, FIN 501 or 503",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (or (ACCTG 614) (ACCTG 610)) })"],
        );
    }

    #[test]
    fn misc_broken() {
        check(
            "ECON 109 and MATH 156",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 109) })"],
        );
        check(
            "ECON 109, and MATH 156",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 109) })"],
        );
        check(
            "ECON 109, ECON 110 and MATH 156",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 109) })"],
        );
        check(
            "ECON 109, ECON 110, and MATH 156",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 109) })"],
        );
        check(
            "MATH 156 or equivalent",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (MATH 156) })"],
        );
        check(
            "ECON 109, and MATH 156 or equivalent",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 109) })"],
        );
        check(
            "ECON 109, ECON 281, 282 and 299 or equivalent",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 109) })"],
        );
        check(
            "ECON 109, ECON 281, 282 and 299 or equivalent, and MATH 156 or equivalent",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 109) })"],
        );
        check(
            "ECON 281, 282 and 299 or equivalent, and MATH 156 or equivalent",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (all (ECON 281) (ECON 282) (ECON 299)) })"],
        );
    }

    #[test]
    #[rustfmt::skip]
    fn misc_working() {
        check(
            "ACCTG 211 or 311 and ACCTG 222 or 322",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (or (ACCTG 211) (ACCTG 311)) })"],
        );
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, or EASIA 325",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (RELIG 240) })"],
        );
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, and EASIA 325",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (RELIG 240) })"],
        );
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325 or consent of Instructor",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (RELIG 240) })"],
        );
        check(
            "one of CMPUT 175 or CMPUT 275",
            expect!["Err(Unrecognized)"],
        );
        check(
            "IMIN 200 and consent of instructor",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (IMIN 200) })"],
        );
        check(
            "One of: RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325 or consent of Instructor",
            expect!["Err(Unrecognized)"],
        );
        check(
            "ECON 281, 282 and 299",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (all (ECON 281) (ECON 282) (ECON 299)) })"],
        );
        check("ECON 281, and MATH 156", expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 281) })"]);
        check(
            "ECON 281 and ECON 282, and MATH 156",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 281) })"],
        );
        check(
            "ECON 281, 282 and 299, and MATH 156",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (all (ECON 281) (ECON 282) (ECON 299)) })"],
        );
        check(
            "ECON 109, ECON 281, 282 and 299, and MATH 156",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (ECON 109) })"],
        );
        check(
            "ACCTG 614 or 610, FIN 501 or 503",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (or (ACCTG 614) (ACCTG 610)) })"],
        );
        check("PL SC 345", expect!["Err(DidNotReachEndOfInput { parsed_so_far: (PL SC 345) })"]);
        check(
            "BIOCH 200, PL SC 345, or consent of instructor",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (BIOCH 200) })"],
        );
        check(
            "one of PHYS 124, PHYS 144, or EN PH 131",
            expect!["Err(Unrecognized)"],
        );
        check(
            "one of PHYS 126, PHYS 146, or PHYS 130 and PHYS 208 or 271",
            expect!["Err(Unrecognized)"],
        );
        check(
            "PHYS 130 and PHYS 208 or 271",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (PHYS 130) })"],
        );
        check(
            "one of PHYS 124, PHYS 144, or EN PH 131, and one of PHYS 126, PHYS 146, or PHYS 130 and PHYS 208 or 271",
            expect!["Err(Unrecognized)"]
        );
        check(
            "MATH 115, 118, 136, 146 or 156, and one of PHYS 124, PHYS 144, or EN PH 131, and one of PHYS 126, PHYS 146, or PHYS 130 and PHYS 208 or 271",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: (MATH 115) })"],
        );
    }
}
