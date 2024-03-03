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
// Good: "A or B or C or D or E"
// Bad: "A, or B"
// Bad (as one list): "A or B, C, D or E"
// It would obviously be inefficient to do this for a programming language,
// but I think this is relatively maintainable and should hopefully not be
// unusably slow.

// I also wonder if we need to define precedence for punctuation.
// A comma binds tighter than a semicolon.

#[derive(Clone, PartialEq, Eq)]
pub enum Requirement {
    Any(Vec<Requirement>),
    All(Vec<Requirement>),
    Course(String),
    ConsentOf(Entity),
}

impl fmt::Debug for Requirement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any(args) => {
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
            Self::All(args) => {
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
            Self::Course(arg0) => write!(f, "{arg0}"),
            Self::ConsentOf(arg0) => write!(f, "consent of {arg0}"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Entity {
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
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParseError {
    Unrecognized,
}

pub fn parse_requirement(s: &str) -> Result<Requirement, ParseError> {
    let tokens = tokenize(s).collect::<Vec<_>>();
    let mut parser = Parser {
        tokens: &tokens,
        pos: 0,
    };
    parser.parse_requirement()
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
}

impl<'a> Parser<'a> {
    fn parse_requirement(&mut self) -> Result<Requirement, ParseError> {
        // The otherwise left-recursive rules go here.
        self.run_parsers(&[
            Parser::parse_any_of_requirements,
            Parser::parse_all_of_requirements,
            Parser::parse_base_requirement,
        ])?
        .ok_or(ParseError::Unrecognized)
    }

    fn parse_base_requirement(&mut self) -> Result<Option<Requirement>, ParseError> {
        self.run_parsers(&[
            Parser::parse_any_of_courses_shared_topic,
            Parser::parse_all_of_courses_shared_topic,
            Parser::parse_lone_course_requirement,
            Parser::parse_consent_of_entity_requirement,
        ])
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

    fn parse_any_of_requirements(&mut self) -> Result<Option<Requirement>, ParseError> {
        self.eat_any_of_or_one_of();
        let Some(reqs) = self.parse_list_of_requirements(|s| matches!(s, "or" | "ou" | "/"))?
        else {
            return Ok(None);
        };
        if reqs.len() < 2 {
            return Ok(None);
        }
        Ok(Some(Requirement::Any(reqs)))
    }

    fn parse_all_of_requirements(&mut self) -> Result<Option<Requirement>, ParseError> {
        let Some(reqs) = self.parse_list_of_requirements(|s| matches!(s, "and" | "et" | ","))?
        else {
            return Ok(None);
        };
        if reqs.len() < 2 {
            return Ok(None);
        }
        Ok(Some(Requirement::All(reqs)))
    }

    fn parse_list_of_requirements(
        &mut self,
        sep_predicate: impl Fn(&str) -> bool,
    ) -> Result<Option<Vec<Requirement>>, ParseError> {
        let Some(first_req) = self.parse_base_requirement()? else {
            return Ok(None);
        };
        let mut requirements = vec![first_req];
        while sep_predicate(self.current_token()) {
            self.bump();
            if let Some(next_req) = self.parse_base_requirement()? {
                requirements.push(next_req);
                continue;
            }
            break;
        }

        Ok(Some(requirements))
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

    fn parse_any_of_courses_shared_topic(&mut self) -> Result<Option<Requirement>, ParseError> {
        self.eat_any_of_or_one_of();
        let Some(courses) =
            self.parse_list_of_courses_shared_topic(|s| matches!(s, "or" | "ou" | "/"))?
        else {
            return Ok(None);
        };
        if courses.len() < 2 {
            return Ok(None);
        }
        Ok(Some(Requirement::Any(
            courses.into_iter().map(Requirement::Course).collect(),
        )))
    }

    fn parse_all_of_courses_shared_topic(&mut self) -> Result<Option<Requirement>, ParseError> {
        let Some(courses) =
            self.parse_list_of_courses_shared_topic(|s| matches!(s, "and" | "et" | ","))?
        else {
            return Ok(None);
        };
        if courses.len() < 2 {
            return Ok(None);
        }
        Ok(Some(Requirement::All(
            courses.into_iter().map(Requirement::Course).collect(),
        )))
    }

    fn parse_list_of_courses_shared_topic(
        &mut self,
        sep_predicate: impl Fn(&str) -> bool,
    ) -> Result<Option<Vec<String>>, ParseError> {
        self.eat_any_of_or_one_of();
        let Some((topic, first_number)) = self.parse_course()? else {
            return Ok(None);
        };
        let mut requirements = vec![format!("{topic} {first_number}")];
        while sep_predicate(self.current_token()) {
            self.bump();
            _ = self.eat(topic);
            let next_number = self.current_token();
            if !next_number.starts_with(char::is_numeric) {
                return Ok(None);
            }
            self.bump();
            requirements.push(format!("{topic} {next_number}"));
        }
        Ok(Some(requirements))
    }

    fn parse_lone_course_requirement(&mut self) -> Result<Option<Requirement>, ParseError> {
        Ok(self
            .parse_course()?
            .map(|(topic, number)| Requirement::Course(format!("{topic} {number}"))))
    }

    fn parse_course(&mut self) -> Result<Option<(&'a str, &'a str)>, ParseError> {
        let course_topic = self.current_token();
        if !data::UALBERTA_TOPICS.contains(&course_topic) {
            return Ok(None);
        }
        self.bump();

        let course_number = self.current_token();
        if !course_number.starts_with(char::is_numeric) {
            return Ok(None);
        }
        self.bump();

        Ok(Some((course_topic, course_number)))
    }

    fn parse_consent_of_entity_requirement(&mut self) -> Result<Option<Requirement>, ParseError> {
        if !self.eat_multiple(&["consent", "of"]) && !self.eat_multiple(&["Consent", "of"]) {
            return Ok(None);
        }
        self.eat("the");
        let entity = match self.bump() {
            "department" | "Department" => Entity::Department,
            "instructor" | "Instructor" => Entity::Instructor,
            "college" | "College" => Entity::College,
            _ => {
                return Err(ParseError::Unrecognized);
            }
        };
        Ok(Some(Requirement::ConsentOf(entity)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(input: &str, output: Expect) {
        let req = parse_requirement(input).unwrap();
        output.assert_eq(&format!("{req:?}"));
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
    fn parse_course_list_requirement() {
        check("CMPUT 174", expect!["CMPUT 174"]);
        check("CMPUT 174 or 274", expect!["any(CMPUT 174, CMPUT 274)"]);
        check(
            "CMPUT 174 or CMPUT 274",
            expect!["any(CMPUT 174, CMPUT 274)"],
        );
        check("ECON 101 and 102", expect!["all(ECON 101, ECON 102)"]);
        check("ECON 101 and ECON 102", expect!["all(ECON 101, ECON 102)"]);
        check(
            "ACCTG 211 or 311 and ACCTG 222 or 322",
            expect!["all(any(ACCTG 211, ACCTG 311), any(ACCTG 222, ACCTG 322))"],
        );
        // FIXME: wrong
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, or EASIA 325",
            expect!["all(RELIG 240, RELIG 343, EASIA 223, EASIA 323)"],
        );
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, and EASIA 325",
            expect!["all(RELIG 240, RELIG 343, EASIA 223, EASIA 323)"],
        );
        check(
            "RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325 or consent of Instructor",
            expect!["all(RELIG 240, RELIG 343, all(EASIA 223, EASIA 323, EASIA 325))"],
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
            "One course in Christian theology and permission of the College",
            expect![],
        );
        check(
            "One of: RELIG 240, RELIG 343, EASIA 223, EASIA 323, EASIA 325 or consent of \
             Instructor",
            expect![],
        );
    }
}
