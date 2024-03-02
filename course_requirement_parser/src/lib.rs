

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prerequisite_and_corequisite_name_lists_match_regex() {
        const PREREQUISITE_NAMES: [&'static str; 18] = [
            "Pre-requisite",
            "Pre-requisite(s)",
            "Pre-requisites",
            "Prerequisite",
            "Prerequisite(s)",
            "Prerequisites",
            "Préalable",
            "Préalable(s)",
            "Préalables",
            "pre-requisite",
            "pre-requisite(s)",
            "pre-requisites",
            "prerequisite",
            "prerequisite(s)",
            "prerequisites",
            "préalable",
            "préalable(s)",
            "préalables",
        ];

        const COREQUISITE_NAMES: [&'static str; 15] = [
            "Concomitant",
            "Concomitant(s)",
            "Concomitants",
            "Corequisite",
            "Corequisite(s)",
            "Corequisites",
            "Pre-and/or corequisite",
            "Préalable ou concomitant",
            "Préalable(s) ou concomitant(s)",
            "concomitant",
            "concomitant(s)",
            "concomitants",
            "corequisite",
            "corequisite(s)",
            "corequisites",
        ];
    }

    #[test]
    fn parse_prerequisites() {
        let s = "Introduction to the basics of evaluation, including the foundations, approaches, \
                 steps, strategies, and ethical considerations of evaluation. Prerequisites: \
                 CMPUT 174 or 274; one of MATH 100, 114, 117, 134, 144, or 154. Corequisites: \
                 CMPUT 175 or 275; CMPUT 272; MATH 125 or 127; one of STAT 141, 151, 235, or 265, \
                 or SCI 151.";
        let results = extract_prerequisites(s);
        eprintln!("{results:?}");
    }
}
