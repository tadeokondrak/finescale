use regex::{Regex, RegexSet};
use std::sync::OnceLock;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RequirementKind {
    Prerequisite,
    Corequisite,
}

pub fn extract_prerequisites(s: &str) -> Vec<(RequirementKind, &str)> {
    const PREREQUISITE_REGEX: &str = r"(?x)(?i)(?-u)
        (pre-?requisite|préalable)
        ((\(s\))|s)?
        :\ (?<data>.*?)\.
    ";

    const COREQUISITE_REGEX: &str = r"(?x)(?i)(?-u)
        ((pre-and/or )?co-?requisite|(préalable((\(s\))|s)? ou )?concomitant((\(s\))|s)?)
        ((\(s\))|s)?
        :\ (?<data>.*)?\.
    ";

    fn get_prerequisite_regex() -> &'static Regex {
        static STATIC: OnceLock<Regex> = OnceLock::new();
        STATIC.get_or_init(|| Regex::new(PREREQUISITE_REGEX).unwrap())
    }

    fn get_corequisite_regex() -> &'static Regex {
        static STATIC: OnceLock<Regex> = OnceLock::new();
        STATIC.get_or_init(|| Regex::new(COREQUISITE_REGEX).unwrap())
    }

    let kinds = [RequirementKind::Corequisite, RequirementKind::Prerequisite];
    let exprs = [COREQUISITE_REGEX, PREREQUISITE_REGEX];
    let regexes = [get_corequisite_regex(), get_prerequisite_regex()];
    let set = RegexSet::new(exprs).unwrap();
    let mut i = 0;
    let mut results = Vec::new();
    loop {
        let matches = set.matches_at(s, i);
        let Some((first_match, first_match_kind)) = matches
            .into_iter()
            .filter_map(|m| Some((regexes[m].captures_at(s, i)?, m)))
            .min_by_key(|&(ref captures, m)| (captures.get(0).unwrap().start(), m))
        else {
            break;
        };
        results.push((
            kinds[first_match_kind],
            &s[first_match.name("data").unwrap().range()],
        ));
        i = first_match.get(0).unwrap().end();
    }
    results
}
