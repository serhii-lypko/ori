use pest::Parser;
use pest::iterators::Pair;

mod backends;
use backends::VM;

/*
    About spans in parse tree:

    When walking parse tree and building an AST or doing semantic analysis,
    I can attach source location metadata to each node for error reporting.
    If I detect a type error on the "+" operation, I know exactly which
    characters to highlight.

    When parsing fails or validation errors occur, I can point to the exact
    character range that caused the problem.
*/

fn main() {
    // let src = "10 * (12 + -2)";
    // let src = "2 + 3 - 4";

    let src = "2 + (3 - 8)";
    // let src = "11 - 8";

    // let res = TreeWalkInterpreter::from_source(src);
    // dbg!(res);

    let res = VM::from_source(src);

    // println!("{:016b}", bar);

    // TODO -> how to construct in a way like this?
    // let v: u16 = vec![left, right].into();
}

// Rules used within the definition of other rules to eventually build
// a parser that understands complex input
#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct OriParser;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
}

impl From<Operator> for &str {
    fn from(value: Operator) -> Self {
        match value {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Multiply => "*",
        }
    }
}

impl From<&str> for Operator {
    fn from(value: &str) -> Self {
        match value {
            "+" => Operator::Plus,
            "-" => Operator::Minus,
            "*" => Operator::Multiply,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Node {
    Int(i64),
    UnaryExpr {
        op: Operator,
        child: Box<Node>,
    },
    BinaryExpr {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    None,
}

pub(crate) trait Compile {
    type Output;

    fn from_ast(node: Node) -> Self::Output;

    // TODO -> fix unwraps and handle Result
    fn from_source(src: &str) -> Self::Output {
        let res = parse(src).unwrap();
        let res_node = res[0].clone();

        Self::from_ast(res_node)
    }
}

struct TreeWalkInterpreter;

impl Compile for TreeWalkInterpreter {
    type Output = i64;

    fn from_ast(node: Node) -> Self::Output {
        eval_tree_walk(node)
    }
}

fn eval_tree_walk(node: Node) -> i64 {
    match node {
        Node::Int(value) => value,
        Node::UnaryExpr { op, child } => {
            let val_res = eval_tree_walk(*child);
            eval_unary(op, val_res)
        }
        Node::BinaryExpr { op, lhs, rhs } => {
            let lhs_val = eval_tree_walk(*lhs);
            let rhs_val = eval_tree_walk(*rhs);
            eval_arithmetics(op, lhs_val, rhs_val)
        }
        Node::None => 0,
    }
}

fn eval_unary(op: Operator, value: i64) -> i64 {
    match op {
        Operator::Plus => value,
        Operator::Minus => -value,
        Operator::Multiply => unreachable!(),
    }
}

fn eval_arithmetics(op: Operator, lhs: i64, rhs: i64) -> i64 {
    match op {
        Operator::Plus => lhs + rhs,
        Operator::Minus => lhs - rhs,
        Operator::Multiply => lhs * rhs,
    }
}

/* -- -- -- -- -- -- -- -- Parsing -- -- -- -- -- -- -- -- */

// It's critical to entirely separate parsing & AST creation from evaluation

pub fn parse(source: &str) -> std::result::Result<Vec<Node>, pest::error::Error<Rule>> {
    let mut ast = vec![];
    let pairs = OriParser::parse(Rule::Program, source).unwrap();

    for pair in pairs {
        if let Rule::Expr = pair.as_rule() {
            ast.push(build_ast_from_expr(pair));
        }
    }

    Ok(ast)
}

// TODO -> fix unwraps
fn build_ast_from_expr(pair: Pair<Rule>) -> Node {
    // Pair<Rule> represents one matched grammar rule

    // `as_rule()` extracts which grammar rule this pair matched, returning a Rule enum value
    // `inner()` returns another Pairs<Rule> iterator containing the nested matches

    match pair.as_rule() {
        Rule::Expr => build_ast_from_expr(pair.into_inner().next().unwrap()),

        Rule::BinaryExpr => {
            let mut inner = pair.into_inner();

            let mut lhs = build_ast_from_expr(inner.next().unwrap());

            while let Some(operator_pair) = inner.next() {
                let op = operator_pair.as_str().into();
                let rhs = build_ast_from_expr(inner.next().unwrap());

                lhs = Node::BinaryExpr {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
            }

            lhs
        }
        Rule::UnaryExpr => {
            let mut inner = pair.into_inner();

            let operator = inner.next().unwrap();
            let rhs_val = build_ast_from_expr(inner.next().unwrap());

            Node::UnaryExpr {
                op: operator.as_str().into(),
                child: Box::new(rhs_val),
            }
        }
        Rule::Term => {
            let inner = pair.into_inner().next().unwrap();

            match inner.as_rule() {
                Rule::Int => {
                    let int: i64 = inner.as_str().parse().unwrap();
                    return Node::Int(int);
                }
                _ => build_ast_from_expr(inner),
            }
        }

        Rule::WHITESPACE => Node::None,
        Rule::EOF => todo!(),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO -> tests for unary

    #[test]
    fn test_single_atom_parsing() {
        let source = "25";
        let expected = vec![Node::Int(25)];
        let res = parse(source).unwrap();
        assert_eq!(expected, res);
    }

    #[test]
    fn test_simple_binary_expr_parsing() {
        let source = "10 + 5";
        let expected = vec![Node::BinaryExpr {
            op: Operator::Plus,
            lhs: Box::new(Node::Int(10)),
            rhs: Box::new(Node::Int(5)),
        }];
        let res = parse(source).unwrap();
        assert_eq!(expected, res);
    }

    #[test]
    fn test_chained_binary_expr_parsing() {
        let source = "10 + 5 + 11";
        let expected = vec![Node::BinaryExpr {
            op: Operator::Plus,
            lhs: Box::new(Node::BinaryExpr {
                op: Operator::Plus,
                lhs: Box::new(Node::Int(10)),
                rhs: Box::new(Node::Int(5)),
            }),
            rhs: Box::new(Node::Int(11)),
        }];
        let res = parse(source).unwrap();
        assert_eq!(expected, res);
    }
}

/*
    - prop_map: Strategy<A> → (A → B) → Strategy<B>
    Takes strategy producing A, closure transforms value A to value B, returns strategy producing B
    Use case: Transform values (convert u32 to String, build struct from tuple)

    - prop_flat_map: Strategy<A> → (A → Strategy<B>) → Strategy<B>
    Takes strategy producing A, closure transforms value A to strategy producing B, returns strategy producing B
    Use case: Generate dependent values (range depends on previous value, second value's strategy depends on first value)
*/
#[cfg(test)]
mod prop_tests {
    use super::*;
    use proptest::prelude::*;

    // 3 + 5 - 7

    // test_basic_expression_parsed

    // test nested_expression_parsed

    fn vec_and_index() -> impl Strategy<Value = (Vec<String>, usize)> {
        prop::collection::vec(".*", 1..100).prop_flat_map(|vec| {
            let len = vec.len();

            // NOTE: 0..len would not work without producing new Strategy. Hence -> prop_flat_map
            (Just(vec), 0..len)
        })
    }

    fn operator() -> impl Strategy<Value = Operator> {
        prop_oneof![
            Just(Operator::Plus),
            Just(Operator::Minus),
            Just(Operator::Multiply),
        ]
        .boxed()
    }

    fn arithmetic_expr_2() -> impl Strategy<Value = String> {
        let num = any::<i64>();
        let op = operator();

        prop::collection::vec("", 1..10).prop_map(|val| {
            //

            "".to_string()
        })
    }

    fn arithmetic_expr() -> impl Strategy<Value = String> {
        use proptest::prelude::*;

        let num = any::<i64>();

        // TODO -> what's the difference?
        let op = prop_oneof![Just("+"), Just("-"), Just("*")];
        // let op2 = operator();

        (num, prop::collection::vec((op, num), 0..5)).prop_map(|(first, rest)| {
            let mut s = first.to_string();
            for (op, num) in rest {
                s.push_str(&format!(" {} {}", op, num));
            }
            s
        })
    }

    // fn gen_two_map() -> impl Strategy<Value = (i32, i32)> {
    //     (1..65536).prop_flat_map(|a| (Just(a), 0..a))
    // }

    // fn foo_bar_strategy() -> impl Strategy<Value = String> {
    //     proptest::string::string_regex("[a-z]+( [a-z]+)*").unwrap()
    // }

    proptest! {
        #[test]
        fn parses_valid_arithmetics(expr_str in arithmetic_expr()) {
            // dbg!(expr_str);

            let parsed = parse(&expr_str);
            prop_assert!(parsed.is_ok());
        }
    }
}
