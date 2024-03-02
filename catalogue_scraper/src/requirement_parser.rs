mod data;

use crate::requirement_extractor::{extract_requirements, RequirementKind};

pub fn parse_requirements(s: &str) -> Expr {
    let requirements = extract_requirements(s);
    todo!()
}

enum Token {
    ConsentOf(Entity),
}

const CONSENT_OF_ENTITY_REGEX: &'static str = r"(?x)(?i)
    consent \s of \s (
        ((the \s)? (?<entity>faculty))
            | ((the \s)? (?<entity>instructor))
            | ((the \s)? (?<entity>department))
            | ((the \s)? (?<entity>assistant \s dean))
    )
";

pub struct Expr {
    context: ExprContext,
    expr: NodeId,
}

pub struct ExprContext {
    nodes: Vec<Node>,
    strings: String,
}

pub struct NodeId(pub u32);

pub struct StringRef {
    pos: u32,
    len: u32,
}

enum CourseStatus {
    TakenInPreviousTerm,
    TakingInCurrentTerm,
}

struct Course {
    topic: String,
    level: String,
    status: CourseStatus,
}

// A Node is a boolean predicate on a set of [Course]s.
pub enum Node {
    Not(NodeId),
    Any(Box<[NodeId]>), // false is Any([])
    All(Box<[NodeId]>), // true is All([])
    Course {
        topic: StringRef,
        level: CourseLevelPredicate,
        req: RequirementKind,
    },
    Credits {
        count: u8,
        satisfying: NodeId,
    },
    ConsentOf(Entity),
}

pub enum ParseNode {
    Equivalent,
}

pub enum Entity {
    Faculty,
    Department,
    Supervisor,
    AssociateDean,
}

enum CourseLevel {
    Number(u16),
    Freeform(String),
}

pub enum CourseLevelPredicate {
    Any,
    Level100,    // 100-199
    Level200,    // 200-299
    Level300,    // 300-399
    Level400,    // 400-499
    SeniorLevel, // 200-499
    ExactNumber(u16),
    ExactFreeform(StringRef),
}
