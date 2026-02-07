use super::ConstValue;
use super::bytecode::*;
use super::op_code::build_const_address;
use crate::{Compile, Node};

use std::mem::MaybeUninit;

// const STACK_SIZE: usize = 512;
const STACK_SIZE: usize = 16;

pub(crate) struct VM {
    bytecode: Bytecode,
    stack: [MaybeUninit<ConstValue>; STACK_SIZE],
    stack_ptr: usize,
}

impl VM {
    fn new(bytecode: Bytecode) -> Self {
        VM {
            bytecode,

            // Evaluate expression once at compile time to create a constant value,
            // and just copy that memory bit-pattern into every slot.
            stack: [const { MaybeUninit::uninit() }; STACK_SIZE],
            stack_ptr: 0,
        }
    }

    fn run(&mut self) {
        let mut ip = 0;

        while ip < self.bytecode.instructions.len() {
            let curr_instr = self.bytecode.instructions[ip];
            ip += 1;

            // TODO -> match over Enum representation of instruction
            match curr_instr {
                // Constant
                0x01 => {
                    // TODO -> so it's now highly inefficient to store 16-bit pointers to the
                    // 8-16 bit small values instead of storing them directly in bytecode
                    let const_address = build_const_address(
                        self.bytecode.instructions[ip],
                        self.bytecode.instructions[ip + 1],
                    );
                    let const_val = self.bytecode.constants[const_address as usize].clone();
                    self.push(const_val);

                    ip += 2;
                }

                // Pop
                0x02 => unimplemented!(),

                // Add
                0x03 => {
                    let sum_val = self.pop() + self.pop();
                    self.push(sum_val);
                }

                // Substract
                0x04 => {
                    let sub_val = self.pop() - self.pop();
                    self.push(sub_val);
                }
                _ => panic!("Unrecognized instruction {}", curr_instr),
            }
        }
    }

    fn push(&mut self, val: ConstValue) {
        self.stack[self.stack_ptr] = MaybeUninit::new(val);
        self.stack_ptr += 1;
    }

    fn pop(&mut self) -> ConstValue {
        self.stack_ptr -= 1;
        unsafe { self.stack[self.stack_ptr].assume_init_read() }
    }

    fn output(&self) -> ConstValue {
        unsafe { self.stack[self.stack_ptr - 1].assume_init_read() }
    }
}

impl Compile for VM {
    // TODO -> should return Result<i32>
    type Output = i32;

    fn from_ast(node: Node) -> Self::Output {
        let bytecode = node.into();
        // dbg!(&bytecode);

        let mut vm = VM::new(bytecode);
        let res = vm.run();

        dbg!(vm.output());

        // TODO
        0
    }
}
