use std::fmt::{self, Write};
use std::{any, iter};

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
pub enum Requirement {
    Custom(&'static str),
    Any(Vec<Requirement>),
    All(Vec<Requirement>),
    Course(Course),
    ConsentOf(Entity),
}

impl fmt::Debug for Requirement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Requirement::Any(args) => {
                write!(f, "any(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg:?}")?;
                }
                f.write_char(')')?;
                Ok(())
            }
            Requirement::All(args) => {
                write!(f, "all(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg:?}")?;
                }
                f.write_char(')')?;
                Ok(())
            }
            Requirement::Course(arg0) => write!(f, "{arg0}"),
            Requirement::ConsentOf(arg0) => write!(f, "consent of {arg0}"),
            Requirement::Custom(arg0) => write!(f, "{arg0}"),
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
    DidNotReachEndOfInput { parsed_so_far: Requirement },
}

pub fn parse_requirement(s: &str) -> Result<Requirement, ParseError> {
    let tokens = tokenize(s).collect::<Vec<_>>();
    let mut parser = Parser {
        tokens: &tokens,
        pos: 0,
    };
    let req = parser
        .parse_requirement(PrecedenceLevel::Highest)?
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

    fn advance_to(&mut self, fork: &Parser<'a>) {
        self.pos = fork.pos
    }

    fn eat_any(&mut self, tokens: &[&str]) -> bool {
        for token in tokens {
            if self.eat(token) {
                return true;
            }
        }
        false
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum PrecedenceLevel {
    Highest,
    NoBareCommaList,
    NoLists,
    NoAndExpr,
    Lowest,
}

// Need X and Y to bind tighter than X, and Y

impl<'a> Parser<'a> {
    fn parse_requirement(
        &mut self,
        precedence: PrecedenceLevel,
    ) -> Result<Option<Requirement>, ParseError> {
        match precedence {
            PrecedenceLevel::Highest => self.run_parsers(&[
                Parser::parse_comma_separated_requirement_list,
                Parser::parse_and_requirement_list,
                Parser::parse_or_requirement_list,
                |this| Parser::parse_requirement(this, PrecedenceLevel::Lowest),
            ]),
            PrecedenceLevel::NoBareCommaList => self.run_parsers(&[
                Parser::parse_and_requirement_list,
                Parser::parse_or_requirement_list,
                |this| Parser::parse_requirement(this, PrecedenceLevel::Lowest),
            ]),
            PrecedenceLevel::NoLists => self.run_parsers(&[
                Parser::parse_binary_and_expression,
                Parser::parse_binary_or_expression,
                |this| Parser::parse_requirement(this, PrecedenceLevel::Lowest),
            ]),
            PrecedenceLevel::NoAndExpr => self
                .run_parsers(&[Parser::parse_binary_or_expression, |this| {
                    Parser::parse_requirement(this, PrecedenceLevel::Lowest)
                }]),

            PrecedenceLevel::Lowest => self.run_parsers(&[
                Parser::parse_prefixed_or_requirement_list,
                Parser::parse_and_course_requirement_list_with_shared_topic,
                Parser::parse_or_course_requirement_list_with_shared_topic,
                Parser::parse_lone_course_requirement,
                Parser::parse_consent_of_entity_requirement,
                Parser::parse_equivalent_requirement,
            ]),
        }
    }

    fn run_parsers(
        &mut self,
        parsers: &[fn(&mut Parser<'a>) -> Result<Option<Requirement>, ParseError>],
    ) -> Result<Option<Requirement>, ParseError> {
        for parser in parsers {
            let mut fork = self.fork();
            if let Some(req) = parser(&mut fork)? {
                self.advance_to(&fork);
                return Ok(Some(req));
            }
        }
        Ok(None)
    }

    fn parse_binary_and_expression(&mut self) -> Result<Option<Requirement>, ParseError> {
        let Some(lhs) = self.parse_requirement(PrecedenceLevel::NoAndExpr)? else {
            return Ok(None);
        };
        if !self.eat_any(&["and", "et"]) {
            return Ok(None);
        }
        let Some(rhs) = self.parse_requirement(PrecedenceLevel::NoLists)? else {
            return Ok(None);
        };
        Ok(Some(Requirement::All(vec![lhs, rhs])))
    }

    fn parse_binary_or_expression(&mut self) -> Result<Option<Requirement>, ParseError> {
        let Some(lhs) = self.parse_requirement(PrecedenceLevel::Lowest)? else {
            return Ok(None);
        };
        if !self.eat_any(&["or", "ou"]) {
            return Ok(None);
        }
        let Some(rhs) = self.parse_requirement(PrecedenceLevel::NoAndExpr)? else {
            return Ok(None);
        };
        Ok(Some(Requirement::Any(vec![lhs, rhs])))
    }

    fn parse_prefixed_or_requirement_list(&mut self) -> Result<Option<Requirement>, ParseError> {
        if !self.eat_any_of_or_one_of() {
            return Ok(None);
        }
        let Some(reqs) = self.parse_list_of_requirements(ListMode::Any)? else {
            return Ok(None);
        };
        if reqs.len() < 2 {
            return Ok(None);
        }
        Ok(Some(Requirement::Any(reqs)))
    }

    fn parse_comma_separated_requirement_list(
        &mut self,
    ) -> Result<Option<Requirement>, ParseError> {
        let Some(first_req) = self.parse_requirement(PrecedenceLevel::NoBareCommaList)? else {
            return Ok(None);
        };
        let mut reqs = vec![first_req];
        while self.eat(",") {
            let Some(next_req) = self.parse_requirement(PrecedenceLevel::NoBareCommaList)? else {
                break;
            };
            reqs.push(next_req);
        }
        Ok(Some(reqs)
            .filter(|reqs| reqs.len() >= 2)
            .map(Requirement::All))
    }

    fn parse_and_requirement_list(&mut self) -> Result<Option<Requirement>, ParseError> {
        let Some(reqs) = self.parse_list_of_requirements(ListMode::All)? else {
            return Ok(None);
        };
        if reqs.len() < 2 {
            return Ok(None);
        }
        Ok(Some(Requirement::All(reqs)))
    }

    fn parse_or_requirement_list(&mut self) -> Result<Option<Requirement>, ParseError> {
        let Some(reqs) = self.parse_list_of_requirements(ListMode::Any)? else {
            return Ok(None);
        };
        if reqs.len() < 2 {
            return Ok(None);
        }
        Ok(Some(Requirement::Any(reqs)))
    }

    fn parse_list_of_requirements(
        &mut self,
        list_mode: ListMode,
    ) -> Result<Option<Vec<Requirement>>, ParseError> {
        let sep_predicate = match list_mode {
            ListMode::Comma => |_| false,
            ListMode::Any => |s| matches!(s, "or" | "ou"),
            ListMode::All => |s| matches!(s, "and" | "et"),
        };

        let lhs_precedence = match list_mode {
            ListMode::Comma => PrecedenceLevel::NoBareCommaList,
            ListMode::Any => PrecedenceLevel::Lowest,
            ListMode::All => PrecedenceLevel::Lowest,
        };

        let rhs_precedence = match list_mode {
            ListMode::Comma => PrecedenceLevel::NoBareCommaList,
            ListMode::Any => PrecedenceLevel::Lowest,
            ListMode::All => PrecedenceLevel::Lowest,
        };

        let Some(first_req) = self.parse_requirement(lhs_precedence)? else {
            return Ok(None);
        };
        let mut requirements = vec![first_req];
        let mut any_sep_matched = false;
        while !any_sep_matched {
            let sep_matches = sep_predicate(self.current_token());
            if !sep_matches && !self.at(",") {
                break;
            }
            any_sep_matched |= sep_matches;

            if self.at(",") && sep_predicate(self.nth_token(1)) {
                any_sep_matched = true;
                self.advance(2);
            } else {
                self.bump();
            }

            let Some(next_req) = self.parse_requirement(rhs_precedence)? else {
                break;
            };

            requirements.push(next_req);
        }
        Ok(Some(requirements).filter(|_| any_sep_matched || list_mode == ListMode::Comma))
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

    fn parse_and_course_requirement_list_with_shared_topic(
        &mut self,
    ) -> Result<Option<Requirement>, ParseError> {
        self.eat_any_of_or_one_of();
        Ok(self
            .parse_list_of_courses_shared_topic(ListMode::Any)?
            .filter(|courses| courses.len() >= 2)
            .map(|courses| courses.into_iter().map(Requirement::Course).collect())
            .map(Requirement::Any))
    }

    fn parse_or_course_requirement_list_with_shared_topic(
        &mut self,
    ) -> Result<Option<Requirement>, ParseError> {
        Ok(self
            .parse_list_of_courses_shared_topic(ListMode::All)?
            .filter(|courses| courses.len() >= 2)
            .map(|courses| courses.into_iter().map(Requirement::Course).collect())
            .map(Requirement::All))
    }

    fn parse_list_of_courses_shared_topic(
        &mut self,
        list_mode: ListMode,
    ) -> Result<Option<Vec<Course>>, ParseError> {
        let sep_predicate = match list_mode {
            ListMode::Any => |s| matches!(s, "or" | "ou"),
            ListMode::All => |s| matches!(s, "and" | "et"),
            ListMode::Comma => unimplemented!(),
        };

        let Some(Course {
            topic,
            number: first_number,
        }) = self.parse_course()?
        else {
            return Ok(None);
        };

        let mut requirements = vec![Course {
            topic: topic.clone(),
            number: first_number,
        }];

        let mut any_sep_matched = false;

        while !any_sep_matched {
            let sep_matches = sep_predicate(self.current_token());
            if sep_matches {
                self.bump();
                any_sep_matched = true;
            } else if self.at(",") && sep_predicate(self.nth_token(1)) {
                self.advance(2);
                any_sep_matched = true;
            } else if self.at(",") {
                self.bump();
            } else {
                break;
            }

            let next_number = self.current_token();
            if !next_number.starts_with(char::is_numeric) {
                return Ok(None);
            }
            self.bump();

            requirements.push(Course {
                topic: topic.clone(),
                number: next_number.to_owned(),
            });
        }

        Ok(Some(requirements).filter(|_| any_sep_matched))
    }

    fn parse_lone_course_requirement(&mut self) -> Result<Option<Requirement>, ParseError> {
        Ok(self.parse_course()?.map(Requirement::Course))
    }

    fn parse_course(&mut self) -> Result<Option<Course>, ParseError> {
        let mut course_topic = self.current_token().to_owned();
        if data::UALBERTA_TOPICS_WITH_SPACES_PREFIXES.contains(&course_topic.as_str()) {
            course_topic = format!("{} {}", self.nth_token(0), self.nth_token(1));
            self.bump();
        } else if !data::UALBERTA_TOPICS.contains(&course_topic.as_str()) {
            return Ok(None);
        }
        self.bump();

        let course_number = self.current_token();
        if !course_number.starts_with(char::is_numeric) {
            return Ok(None);
        }
        self.bump();

        Ok(Some(Course {
            topic: course_topic,
            number: course_number.to_owned(),
        }))
    }

    fn parse_consent_of_entity_requirement(&mut self) -> Result<Option<Requirement>, ParseError> {
        if !self.eat_multiple(&["consent", "of"])
            && !self.eat_multiple(&["Consent", "of"])
            && !self.eat_multiple(&["permission", "of"])
            && !self.eat_multiple(&["Permission", "of"])
        {
            return Ok(None);
        }
        self.eat("the");
        let entity = match self.bump() {
            "department" | "Department" => Entity::Department,
            "instructor" | "Instructor" => Entity::Instructor,
            "college" | "College" => Entity::College,
            _ => Entity::Other,
        };
        Ok(Some(Requirement::ConsentOf(entity)))
    }

    fn parse_equivalent_requirement(&mut self) -> Result<Option<Requirement>, ParseError> {
        if !self.eat("equivalent") {
            return Ok(None);
        }
        Ok(Some(Requirement::All(vec![])))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ListMode {
    Comma,
    Any,
    All,
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
            expect!["consent of department"],
        );
        assert_eq!(
            parse_requirement("consent of the department"),
            Ok(Requirement::ConsentOf(Entity::Department)),
        );
    }

    #[test]
    fn parse_simple_course_requirement() {
        check("CMPUT 174", expect!["CMPUT 174"]);
    }

    #[test]
    fn parse_any_of_course_requirements() {
        check(
            "CMPUT 174 or CMPUT 274",
            expect!["any(CMPUT 174, CMPUT 274)"],
        );
    }

    #[test]
    fn parse_any_of_course_requirements_without_repeated_topic() {
        check("CMPUT 174 or 274", expect!["any(CMPUT 174, CMPUT 274)"]);
    }

    #[test]
    fn parse_all_of_course_requirements() {
        check("ECON 101 and ECON 102", expect!["all(ECON 101, ECON 102)"]);
    }

    #[test]
    fn parse_all_of_course_requirements_without_repeated_topic() {
        check("ECON 101 and 102", expect!["all(ECON 101, ECON 102)"]);
    }

    #[test]
    fn parse_top_level_comma_separated_list() {
        check("ACCTG 614, FIN 501", expect!["all(ACCTG 614, FIN 501)"]);
        check(
            "ACCTG 614, ACCTG 610, FIN 501",
            expect!["all(ACCTG 614, ACCTG 610, FIN 501)"],
        );
        check(
            "ACCTG 614 or 610, FIN 501 or 503",
            expect!["all(any(ACCTG 614, ACCTG 610), any(FIN 501, FIN 503))"],
        );
    }

    #[test]
    fn misc_broken() {
        check("ECON 109, and MATH 156", expect!["all(ECON 109, MATH 156)"]);
        check(
            "ECON 109, and MATH 156 or equivalent",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: all(ECON 109, MATH 156) })"],
        );
        check(
            "ECON 109, ECON 281, 282 and 299 or equivalent",
            expect!["any(ECON 109, all(ECON 281, ECON 282, ECON 299), all())"],
        );
        check(
            "ECON 109, ECON 281, 282 and 299 or equivalent, and MATH 156 or equivalent",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: any(ECON 109, all(ECON 281, ECON 282, ECON 299), all()) })"],
        );
        //check(
        //    "One course in Christian theology and permission of the College",
        //    expect![],
        //);
    }

    #[test]
    #[rustfmt::skip]
    fn misc_working() {
        check(
            "ACCTG 211 or 311 and ACCTG 222 or 322",
            expect!["all(any(ACCTG 211, ACCTG 311), any(ACCTG 222, ACCTG 322))"],
        );
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, or EASIA 325",
            expect!["any(RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325)"],
        );
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, and EASIA 325",
            expect!["all(RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325)"],
        );
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325 or consent of Instructor",
            expect!["any(RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325, consent of instructor)"],
        );
        check(
            "one of CMPUT 175 or CMPUT 275",
            expect!["any(CMPUT 175, CMPUT 275)"],
        );
        check(
            "One of CMPUT 175 or CMPUT 275",
            expect!["any(CMPUT 175, CMPUT 275)"],
        );
        check(
            "One of: CMPUT 175 or CMPUT 275",
            expect!["any(CMPUT 175, CMPUT 275)"],
        );
        check(
            "IMIN 200 and consent of instructor",
            expect!["all(IMIN 200, consent of instructor)"],
        );
        check(
            "One of: RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325 or consent of Instructor",
            expect!["any(RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325, consent of instructor)"],
        );
        check(
            "ECON 281, 282 and 299",
            expect!["all(ECON 281, ECON 282, ECON 299)"],
        );
        check("ECON 281, and MATH 156", expect!["all(ECON 281, MATH 156)"]);
        check(
            "ECON 281 and ECON 282, and MATH 156",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: all(ECON 281, ECON 282) })"],
        );
        check(
            "ECON 281, 282 and 299, and MATH 156",
            expect!["all(all(ECON 281, ECON 282, ECON 299), MATH 156)"],
        );
        check(
            "ECON 109, ECON 281, 282 and 299, and MATH 156",
            expect!["all(ECON 109, all(ECON 281, ECON 282, ECON 299), MATH 156)"],
        );
        check(
            "ACCTG 614 or 610, FIN 501 or 503",
            expect!["all(any(ACCTG 614, ACCTG 610), any(FIN 501, FIN 503))"],
        );
        check("PL SC 345", expect!["PL SC 345"]);
        check(
            "BIOCH 200, PL SC 345, or consent of instructor",
            expect!["any(BIOCH 200, PL SC 345, consent of instructor)"],
        );
        check(
            "one of PHYS 124, PHYS 144, or EN PH 131",
            expect!["any(PHYS 124, PHYS 144, EN PH 131)"],
        );
        check(
            "one of PHYS 126, PHYS 146, or PHYS 130 and PHYS 208 or 271",
            expect!["all(any(PHYS 126, PHYS 146, PHYS 130), any(PHYS 208, PHYS 271))"],
        );
        check(
            "PHYS 130 and PHYS 208 or 271",
            expect!["all(PHYS 130, any(PHYS 208, PHYS 271))"],
        );
        check(
            "one of PHYS 124, PHYS 144, or EN PH 131, and one of PHYS 126, PHYS 146, or PHYS 130 and PHYS 208 or 271",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: all(any(PHYS 124, PHYS 144, EN PH 131), any(PHYS 126, PHYS 146, PHYS 130)) })"]
        );
        check(
            "MATH 115, 118, 136, 146 or 156, and one of PHYS 124, PHYS 144, or EN PH 131, and one of PHYS 126, PHYS 146, or PHYS 130 and PHYS 208 or 271",
            expect!["Err(DidNotReachEndOfInput { parsed_so_far: all(any(MATH 115, MATH 118, MATH 136, MATH 146, MATH 156), any(PHYS 124, PHYS 144, EN PH 131)) })"],
        );
    }
}
