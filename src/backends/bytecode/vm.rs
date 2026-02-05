use super::bytecode::*;
use crate::{Compile, Node};

pub(crate) struct VM {
    bytecode: Bytecode,
}

impl VM {
    pub(crate) fn new(bytecode: Bytecode) -> Self {
        VM { bytecode }
    }
}

impl Compile for VM {
    // TODO -> should return Result<i32>
    type Output = i32;

    fn from_ast(node: Node) -> Self::Output {
        let bytecode = node.into();
        dbg!(&bytecode);

        let vm = VM::new(bytecode);

        todo!()
    }
}
