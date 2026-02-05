use super::op_code::*;
use crate::{Node, Operator};

#[derive(Debug)]
pub(super) struct Bytecode {
    instructions: Vec<u8>,
    constants: Vec<Node>,
}

impl Bytecode {
    fn new() -> Self {
        Bytecode {
            instructions: vec![],
            constants: vec![],
        }
    }

    fn add_constant(&mut self, constant: Node) -> u16 {
        self.constants.push(constant);
        let constant_index = (self.constants.len() - 1) as u16;

        constant_index
    }

    fn add_instruction(&mut self, op_code: OpCode) -> u16 {
        let instr_offset = self.instructions.len() as u16;
        let instr_bytes: Vec<u8> = op_code.into();
        self.instructions.extend(instr_bytes);

        instr_offset
    }

    // Post-order DFS traversal
    fn build_from_ast(&mut self, ast_node: Node) {
        match ast_node {
            Node::Int(val) => {
                let const_offset = self.add_constant(Node::Int(val));
                let op_code = OpCode::OpConstant(const_offset);
                self.add_instruction(op_code);
            }
            Node::UnaryExpr { op, child } => unimplemented!(),
            Node::BinaryExpr { op, lhs, rhs } => {
                self.build_from_ast(*lhs);
                self.build_from_ast(*rhs);

                match op {
                    Operator::Plus => self.add_instruction(OpCode::OpAdd),
                    Operator::Minus => self.add_instruction(OpCode::OpSub),
                    Operator::Multiply => self.add_instruction(OpCode::OpMul),
                };
            }
            Node::None => unimplemented!(),
        };
    }
}

impl From<Node> for Bytecode {
    fn from(ast_node: Node) -> Self {
        let mut bytecode = Bytecode::new();
        bytecode.build_from_ast(ast_node);

        bytecode
    }
}
