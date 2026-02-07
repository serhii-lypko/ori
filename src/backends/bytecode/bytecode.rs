use super::ConstValue;
use super::op_code::*;
use crate::{Node, Operator};

/*
    ByteCode {
        instructions: [1, 0, 0, 1, 0, 1, 3, 2],
        //             ^-----^  ^-----^  ^  ^
        //             OpConstant(0)  OpAdd OpPop
        //                    OpConstant(1)
        constants: [Int(1), Int(2)]
    }
*/
#[derive(Debug)]
pub(super) struct Bytecode {
    // TODO -> consider introducing opcodes like OpConstantSmall(u16) etc. for inlining
    // small ConstValues to bytecode
    pub(super) instructions: Vec<u8>,
    pub(super) constants: Vec<ConstValue>,
}

impl Bytecode {
    fn new() -> Self {
        Bytecode {
            instructions: vec![],
            constants: vec![],
        }
    }

    fn add_constant(&mut self, constant: ConstValue) -> u16 {
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
                let const_offset = self.add_constant(ConstValue::Int(val));
                let op_code = OpCode::OpConstant(const_offset);
                self.add_instruction(op_code);
            }
            Node::UnaryExpr { op, child } => {
                self.build_from_ast(*child);

                match op {
                    Operator::Plus => self.add_instruction(OpCode::OpAdd),
                    Operator::Minus => self.add_instruction(OpCode::OpSub),
                    Operator::Multiply => self.add_instruction(OpCode::OpMul),
                };
            }
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
        // dbg!(&ast_node);

        let mut bytecode = Bytecode::new();
        bytecode.build_from_ast(ast_node);

        bytecode
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Node, Operator};

    #[test]
    fn test_bytecode_creation_from_simple_integer() {
        let ast = Node::Int(42);
        let bytecode = Bytecode::from(ast);

        assert_eq!(bytecode.constants.len(), 1);
        assert_eq!(bytecode.constants[0], ConstValue::Int(42));
        assert_eq!(bytecode.instructions, vec![0x01, 0x00, 0x00]);
    }

    #[test]
    fn test_add_constant_returns_correct_index() {
        let mut bytecode = Bytecode::new();

        let idx1 = bytecode.add_constant(ConstValue::Int(10));
        let idx2 = bytecode.add_constant(ConstValue::Int(20));

        assert_eq!(idx1, 0);
        assert_eq!(idx2, 1);
        assert_eq!(bytecode.constants.len(), 2);
    }

    #[test]
    fn test_add_instruction_returns_correct_offset() {
        let mut bytecode = Bytecode::new();

        let offset1 = bytecode.add_instruction(OpCode::OpAdd);
        let offset2 = bytecode.add_instruction(OpCode::OpMul);

        assert_eq!(offset1, 0);
        assert_eq!(offset2, 1);
        assert_eq!(bytecode.instructions, vec![0x03, 0x05]);
    }

    #[test]
    fn test_binary_expression_compilation() {
        let ast = Node::BinaryExpr {
            op: Operator::Plus,
            lhs: Box::new(Node::Int(5)),
            rhs: Box::new(Node::Int(10)),
        };

        let bytecode = Bytecode::from(ast);

        assert_eq!(bytecode.constants.len(), 2);
        assert_eq!(bytecode.constants[0], ConstValue::Int(5));
        assert_eq!(bytecode.constants[1], ConstValue::Int(10));

        let expected_instructions = vec![0x01, 0x00, 0x00, 0x01, 0x00, 0x01, 0x03];
        assert_eq!(bytecode.instructions, expected_instructions);
    }

    #[test]
    fn test_nested_binary_expression_compilation() {
        let ast = Node::BinaryExpr {
            op: Operator::Multiply,
            lhs: Box::new(Node::BinaryExpr {
                op: Operator::Plus,
                lhs: Box::new(Node::Int(2)),
                rhs: Box::new(Node::Int(3)),
            }),
            rhs: Box::new(Node::Int(4)),
        };

        let bytecode = Bytecode::from(ast);

        assert_eq!(bytecode.constants.len(), 3);
        assert_eq!(bytecode.constants[0], ConstValue::Int(2));
        assert_eq!(bytecode.constants[1], ConstValue::Int(3));
        assert_eq!(bytecode.constants[2], ConstValue::Int(4));

        let expected_instructions = vec![
            0x01, 0x00, 0x00, 0x01, 0x00, 0x01, 0x03, 0x01, 0x00, 0x02, 0x05,
        ];
        assert_eq!(bytecode.instructions, expected_instructions);
    }

    #[test]
    fn test_subtraction_operation() {
        let ast = Node::BinaryExpr {
            op: Operator::Minus,
            lhs: Box::new(Node::Int(15)),
            rhs: Box::new(Node::Int(7)),
        };

        let bytecode = Bytecode::from(ast);

        let expected_instructions = vec![0x01, 0x00, 0x00, 0x01, 0x00, 0x01, 0x04];
        assert_eq!(bytecode.instructions, expected_instructions);
    }
}
