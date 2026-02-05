#[derive(Debug, Copy, Clone)]
pub(super) enum OpCode {
    // Push constant onto the stack
    OpConstant(u16),

    // Pop and discard the top value
    OnPop,

    // Pop two values, add their sum
    OpAdd,

    // Pop two values, push their difference
    OpSub,

    // Pop two values, push their multyplication
    OpMul,
}

impl From<OpCode> for Vec<u8> {
    fn from(op_code: OpCode) -> Self {
        match op_code {
            OpCode::OpConstant(const_index) => {
                // Address space of the constants is encoded into two bytes
                vec![0x01, (const_index >> 8) as u8, const_index as u8]
            }
            OpCode::OnPop => vec![0x02],
            OpCode::OpAdd => vec![0x03],
            OpCode::OpSub => vec![0x04],
            OpCode::OpMul => vec![0x05],
        }
    }
}
