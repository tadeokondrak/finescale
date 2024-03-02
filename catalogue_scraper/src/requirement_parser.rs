use std::iter;

mod data;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Requirement {
    Any(Vec<Requirement>),
    All(Vec<Requirement>),
    Course(String),
    ConsentOf(Entity),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Entity {
    Department,
    Instructor,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParseError {
    Unspecified,
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
        self.run_parsers(&[
            Parser::parse_any_of_courses,
            Parser::parse1_base_requirement,
        ])?
        .ok_or(ParseError::Unspecified)
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

    fn eat_any_of_or_one_of(&mut self) -> bool {
        self.eat_multiple(&["any", "of"])
            || self.eat_multiple(&["Any", "of"])
            || self.eat_multiple(&["one", "of"])
            || self.eat_multiple(&["One", "of"])
    }

    // Parses things like "One of CMPUT 201 or CMPUT 275"
    fn parse_any_of_courses(&mut self) -> Result<Option<Requirement>, ParseError> {
        self.eat_any_of_or_one_of();
        let Some(first_course) = self.parse_course_requirement()? else {
            return Ok(None);
        };
        let mut requirements = vec![first_course];
        while self.eat("or") {
            if let Some(next_course) = self.parse_course_requirement()? {
                requirements.push(next_course);
                continue;
            }

            return Ok(None);
        }
        if requirements.len() == 1 {
            Ok(Some(requirements.into_iter().nth(0).unwrap()))
        } else {
            Ok(Some(Requirement::Any(requirements)))
        }
    }
    fn parse_any_of_courses_shared_topic(&mut self) -> Result<Option<Requirement>, ParseError> {
        let Some(courses) = self.parse_list_of_courses_shared_topic("or")? else {
            return Ok(None);
        };
        Ok(Some(Requirement::Any(
            courses.into_iter().map(Requirement::Course).collect(),
        )))
    }

    fn parse_all_of_courses_shared_topic(&mut self) -> Result<Option<Requirement>, ParseError> {
        let Some(courses) = self.parse_list_of_courses_shared_topic("and")? else {
            return Ok(None);
        };
        Ok(Some(Requirement::All(
            courses.into_iter().map(Requirement::Course).collect(),
        )))
    }

    fn parse_list_of_courses_shared_topic(
        &mut self,
        sep: &str,
    ) -> Result<Option<Vec<String>>, ParseError> {
        self.eat_any_of_or_one_of();
        let Some((topic, first_number)) = self.parse_course()? else {
            return Ok(None);
        };
        let mut requirements = vec![format!("{topic} {first_number}")];
        while self.eat(sep) {
            let next_number = self.current_token();
            if !next_number.starts_with(char::is_numeric) {
                return Ok(None);
            }
            self.bump();
            requirements.push(format!("{topic} {next_number}"));
        }
        Ok(Some(requirements))
    }

    fn parse1_base_requirement(&mut self) -> Result<Option<Requirement>, ParseError> {
        self.run_parsers(&[
            Parser::parse_any_of_courses_shared_topic,
            Parser::parse_all_of_courses_shared_topic,
            Parser::parse_consent_of_entity_requirement,
        ])
    }

    fn parse_course_requirement(&mut self) -> Result<Option<Requirement>, ParseError> {
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
        if !self.eat_multiple(&["consent", "of"]) {
            return Ok(None);
        }
        self.eat("the");
        let entity = match self.bump() {
            "department" => Entity::Department,
            "instructor" => Entity::Instructor,
            _ => {
                return Err(ParseError::Unspecified);
            }
        };
        Ok(Some(Requirement::ConsentOf(entity)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::Expect;

    fn check(input: &str, output: Expect) {}

    #[test]
    fn parse_consent_of_the_department_requirement() {
        assert_eq!(
            parse_requirement("consent of the department"),
            Ok(Requirement::ConsentOf(Entity::Department)),
        );
    }

    #[test]
    fn parse_course_list_requirement() {
        assert_eq!(
            parse_requirement("CMPUT 174"),
            Ok(Requirement::Course("CMPUT 174".to_owned())),
        );
        assert_eq!(
            parse_requirement("CMPUT 174 or 274"),
            Ok(Requirement::Any(vec![
                Requirement::Course("CMPUT 174".to_owned()),
                Requirement::Course("CMPUT 274".to_owned())
            ])),
        );
    }
}
