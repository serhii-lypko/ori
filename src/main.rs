use pest::Parser;
use pest::iterators::Pair;

/*
    About spans in parse tree:

    When walking parse tree and building an AST or doing semantic analysis,
    I can attach source location metadata to each node for error reporting.
    If I detect a type error on the "+" operation, I know exactly which
    characters to highlight.

    When parsing fails or validation errors occur, I can point to the exact
    character range that caused the problem.
*/

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct OriParser;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Node {
    Int(i32),
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

fn main() {
    // let input = "12";
    // let input = "10 + 12 + 100";
    let input = "10 * (12 + 2)";

    let res = parse(input);

    dbg!(res);
}

// pub fn parse(source: &str) -> std::result::Result<Vec<Node>, pest::error::Error<Rule>> {
pub fn parse(source: &str) -> Vec<Node> {
    let mut ast = vec![];
    let pairs = OriParser::parse(Rule::Program, source).unwrap();

    for pair in pairs {
        if let Rule::Expr = pair.as_rule() {
            ast.push(build_ast_from_expr(pair));
        }
    }

    ast
}

// TODO -> fix unwraps
// Pair<Rule> represents one matched grammar rule
fn build_ast_from_expr(pair: Pair<Rule>) -> Node {
    // `as_rule()` extracts which grammar rule this pair matched, returning a Rule enum value
    // `inner()` returns another Pairs<Rule> iterator containing the nested matches

    // 10 + 12 + 100
    match pair.as_rule() {
        Rule::Expr => build_ast_from_expr(pair.into_inner().next().unwrap()),

        Rule::BinaryExpr => {
            println!("It's a BinaryExpr");

            let mut inner = pair.into_inner();

            // TODO -> handle unary
            let mut lhs = build_ast_from_expr(inner.next().unwrap());

            while let Some(operator_pair) = inner.next() {
                let op = handle_operator(operator_pair);
                let rhs = build_ast_from_expr(inner.next().unwrap());

                lhs = Node::BinaryExpr {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
            }

            lhs
        }
        Rule::UnaryExpr => unimplemented!(),
        Rule::Term => {
            println!("It's a Term");
            let inner = pair.into_inner().next().unwrap();

            match inner.as_rule() {
                Rule::Int => {
                    let int: i32 = inner.as_str().parse().unwrap();
                    return Node::Int(int);
                }
                _ => {
                    // TODO -> handle Expr:
                    // Term = { Int | "(" ~ Expr ~ ")" }

                    unimplemented!()
                }
            }
        }
        Rule::Operator => todo!(),

        Rule::WHITESPACE => todo!(),
        Rule::EOF => todo!(),
        _ => todo!(),
    }
}

// TODO -> return Result
fn handle_operator(pair: Pair<Rule>) -> Operator {
    match pair.as_str() {
        "+" => Operator::Plus,
        "-" => Operator::Minus,
        "*" => Operator::Multiply,
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_atom_parsing() {
        let source = "25";
        let expected = vec![Node::Int(25)];
        let res = parse(source);
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
        let res = parse(source);
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
        let res = parse(source);
        assert_eq!(expected, res);
    }
}
