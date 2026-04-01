use crate::compiler::{ConstValue, Instruction, Opcode, PrototypeFunction, StructPrototype, A_SHIFT, B_SHIFT, C_SHIFT, OPCODE_BITS};
use crate::compiler::ConstValue as CV;
use crate::VType;

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    U8(u8),
    I8(i8),

    U16(u16),
    I16(i16),

    U32(u32),
    I32(i32),

    F32(f32),
    F64(f64),

    Str(String),
    USize(usize),
    Bool(bool),

    Ptr(VType, usize),

    Function(usize),
    Struct(usize),
    NativeFunction(usize),

    Empty,
}

#[derive(Debug, Clone)]
pub struct StructInstance {
    struct_idx: usize,
    fields: Vec<RuntimeValue>,
}

#[derive(Debug)]
pub struct NativeFunction {
    pub name: String,
    //pub param_count: usize,
    pub func: fn(&[RuntimeValue]) -> RuntimeValue,
}

#[derive(Debug)]
pub struct CallFrame {
    proto_index: usize,     // which prototype we're running
    pc: usize,              // instruction pointer in that proto
    regs: Vec<RuntimeValue>, // registers for this frame (register 0..N)
    stack_base: usize,
    local_var_bases: Vec<usize>,
}

#[derive(Debug)]
pub struct VM {
    pub native_functions: Vec<NativeFunction>,
    pub protos: Vec<PrototypeFunction>, // prototypes compiled earlier
    pub struct_protos: Vec<StructPrototype>,
    pub consts: Vec<ConstValue>,        // top-level constant pool (if any)
    pub frames: Vec<CallFrame>,         // call stack
    pub frame_regs: usize,
    pub stack: Vec<StructInstance>,
    pub block_stack_bases: Vec<usize>,
}

impl VM {
    pub fn new(native_functions: Vec<NativeFunction>, protos: Vec<PrototypeFunction>, struct_protos: Vec<StructPrototype>, consts: Vec<ConstValue>) -> Self {
        Self {
            native_functions,
            protos,
            struct_protos,
            consts,
            frames: Vec::new(),
            frame_regs: 256,
            stack: Vec::new(),
            block_stack_bases: Vec::new()
        }
    }

    pub fn register_native(&mut self, name: &str, /*param_count: usize, */func: fn(&[RuntimeValue]) -> RuntimeValue) -> usize {
        let idx = self.native_functions.len();
        self.native_functions.push(NativeFunction {
            name: name.to_string(),
            //param_count,
            func,
        });
        idx
    }

    fn allocate_struct(&mut self, struct_idx: usize) -> usize {
        let struct_proto = &self.struct_protos[struct_idx];
        let num_fields = struct_proto.fields.len();
        
        let instance = StructInstance {
            struct_idx,
            fields: vec![RuntimeValue::Empty; num_fields],
        };
        
        let stack_idx = self.stack.len();
        self.stack.push(instance);
        stack_idx
    }

    fn get_struct_mut(&mut self, stack_idx: usize) -> &mut StructInstance {
        self.stack.get_mut(stack_idx)
            .expect(&format!("Invalid struct stack index: {}", stack_idx))
    }

    fn get_struct(&self, stack_idx: usize) -> &StructInstance {
        self.stack.get(stack_idx)
            .expect(&format!("Invalid struct stack index: {}", stack_idx))
    }

    fn cleanup_stack(&mut self, stack_base: usize) {
        if stack_base <= self.stack.len() {
            self.stack.truncate(stack_base);
        }
    }

    fn is_valid_struct_ptr(&self, stack_idx: usize) -> bool {
        stack_idx < self.stack.len()
    }

    pub fn run_from_proto(&mut self, main_proto_index: usize) {
        self.push_frame(main_proto_index);
        while !self.frames.is_empty() {
            let top_idx = self.frames.len() - 1;
            {
                let frame = &mut self.frames[top_idx];
                let proto = &self.protos[frame.proto_index];
                
                if frame.pc >= proto.code.len() {
                    // proto ended: implicit return (empty)
                    self.pop_frame_and_propagate(None);
                    continue;
                }

                let instr = proto.code[frame.pc];
                frame.pc += 1;

                let opcode = instr & ((1 << OPCODE_BITS) - 1);
                let a = ((instr >> A_SHIFT) & 0xFF) as usize;
                let bx = (instr >> B_SHIFT) as usize;
                
                let b = ((instr >> B_SHIFT) & 0x1FF) as usize;
                let c = ((instr >> C_SHIFT) & 0x1FF) as usize;
                
                //prI32ln!("{:?}", Opcode::try_from(opcode).unwrap());
                
                match opcode {
                    x if x == Opcode::LoadConst as u32 => {
                        let const_val = proto.ctx.consts.get(bx)
                            .cloned()
                            .or_else(|| self.consts.get(bx).cloned())
                            .expect(&format!("LoadConst: OOB bx={} proto={}", bx, frame.proto_index));
                        
                        frame.regs[a] = match const_val {
                            CV::I32(n) => RuntimeValue::I32(n),
                            CV::Bool(b) => RuntimeValue::Bool(b),
                            CV::Empty => RuntimeValue::Empty,
                            CV::USize(n) => RuntimeValue::USize(n),
                            CV::Str(s) => RuntimeValue::Str(s),
                            CV::Function(idx) => RuntimeValue::Function(idx),
                            CV::NativeFunction(idx) => RuntimeValue::NativeFunction(idx),
                            CV::Struct(idx) => RuntimeValue::Struct(idx),
                            other => panic!("LoadConst: unsupported const variant at runtime: {:?}", other),
                        };
                    }
                    x if x == Opcode::NewStruct as u32 => {
                        // A = target register, Bx = struct constant index
                        let const_val = proto.ctx.consts.get(bx)
                            .cloned()
                            .or_else(|| self.consts.get(bx).cloned())
                            .expect(&format!("NewStruct: OOB bx={}", bx));
                        
                        if let ConstValue::Struct(struct_idx) = const_val {
                            // Allocate struct on stack directly (inline the allocate_struct logic)
                            let struct_proto = &self.struct_protos[struct_idx];
                            let num_fields = struct_proto.fields.len();
                            
                            let instance = StructInstance {
                                struct_idx,
                                fields: vec![RuntimeValue::Empty; num_fields],
                            };
                            
                            let stack_idx = self.stack.len();
                            self.stack.push(instance);
                            
                            frame.regs[a] = RuntimeValue::Ptr(struct_proto.struct_type.clone(), stack_idx);
                        } else {
                            panic!("NewStruct: constant at index {} is not a Struct", bx);
                        }
                    }

                    x if x == Opcode::StoreStructField as u32 => {
                        // A = struct register, B = field index, C = value register
                        if a >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("StoreStructField: register OOB a={} c={}", a, c);
                        }

                        let value = frame.regs[c].clone();
                        
                        match &frame.regs[a] {
                            RuntimeValue::Ptr(VType::Struct(..), stack_idx) => {
                                // Extract the validation and struct access outside the frame borrow
                            }
                            other => panic!("StoreStructField: target is not a struct pointer: {:?}", other),
                        }
                        
                        // Now handle the struct operation after frame borrow is released
                        if let RuntimeValue::Ptr(VType::Struct(..), stack_idx) = &frame.regs[a] {
                            let stack_idx = *stack_idx;
                            if stack_idx >= self.stack.len() {
                                panic!("StoreStructField: invalid struct pointer {}", stack_idx);
                            }
                            
                            if b >= self.stack[stack_idx].fields.len() {
                                panic!("StoreStructField: field index {} out of bounds", b);
                            }
                            self.stack[stack_idx].fields[b] = value;
                        }
                    }

                    x if x == Opcode::LoadStructField as u32 => {
                        // A = target register, B = struct register, C = field index
                        if a >= frame.regs.len() || b >= frame.regs.len() {
                            panic!("LoadStructField: register OOB a={} b={}", a, b);
                        }

                        match &frame.regs[b] {
                            RuntimeValue::Ptr(VType::Struct(..), stack_idx) => {
                                let stack_idx = *stack_idx;
                                if stack_idx >= self.stack.len() {
                                    panic!("LoadStructField: invalid struct pointer {}", stack_idx);
                                }
                                
                                if c >= self.stack[stack_idx].fields.len() {
                                    panic!("LoadStructField: field index {} out of bounds", c);
                                }
                                frame.regs[a] = self.stack[stack_idx].fields[c].clone();
                            }
                            other => panic!("LoadStructField: source is not a struct pointer: {:?}", other),
                        }
                    }
                    x if x == Opcode::Move as u32 => {
                        if bx >= frame.regs.len() || a >= frame.regs.len() {
                            panic!("Move: reg OOB a={} bx={}", a, bx);
                        }
                        frame.regs[a] = frame.regs[bx].clone();
                    }
                    x if x == Opcode::Ret as u32 => {
                        let return_value = if a == 0 {
                            None
                        } else {
                            let ret_reg = a - 1;
                            if ret_reg >= frame.regs.len() {
                                panic!("Ret: return register OOB ret_reg={}", ret_reg);
                            }
                            Some(frame.regs[ret_reg].clone())
                        };
                        
                        self.pop_frame_and_propagate(return_value);
                    }
                    x if x == Opcode::Call as u32 => {
                        let num_args = bx;
                        if a >= frame.regs.len() {
                            panic!("Call: function register OOB a={}", a);
                        }
                        let func_slot = frame.regs[a].clone();
                        match func_slot {
                            RuntimeValue::Function(proto_idx) => {
                                let stack_base = self.stack.len();
                                let regs = vec![RuntimeValue::Empty; self.frame_regs];
                                let mut callee_frame = CallFrame { 
                                    proto_index: proto_idx,
                                    pc: 0, 
                                    regs, 
                                    stack_base,
                                    local_var_bases: Vec::new()
                                };

                                // Copy arguments to callee's registers
                                for i in 0..num_args {
                                    let src_idx = a + 1 + i;
                                    if src_idx >= frame.regs.len() {
                                        panic!("CALL: caller arg register OOB");
                                    }
                                    callee_frame.regs[i] = frame.regs[src_idx].clone();
                                }

                                self.frames.push(callee_frame);
                            }
                            RuntimeValue::NativeFunction(native_idx) => {
                                let native = self.native_functions.get(native_idx)
                                    .expect("Invalid native function index");

                                let mut args = Vec::with_capacity(num_args);
                                for i in 0..num_args {
                                    let arg_reg = a + 1 + i;
                                    if arg_reg >= frame.regs.len() {
                                        panic!("CALL (native): arg reg OOB");
                                    }
                                    args.push(frame.regs[arg_reg].clone());
                                }

                                let ret = (native.func)(&args);
                                frame.regs[a] = ret;
                            }
                            other => panic!("Call target not a function: {:?}//[{}]{:?}", other, a, frame.regs),
                        }
                    }
                    x if x == Opcode::Add as u32
                        || x == Opcode::Sub as u32
                        || x == Opcode::Mul as u32
                        || x == Opcode::Div as u32 => {
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("Binary op: register OOB a={} b={} c={}", a, b, c);
                        }

                        let left = &frame.regs[b];
                        let right = &frame.regs[c];

                        let result = match opcode {
                            x if x == Opcode::Add as u32 => apply_binop(left, right, |x, y| x + y),
                            x if x == Opcode::Sub as u32 => apply_binop(left, right, |x, y| x - y),
                            x if x == Opcode::Mul as u32 => apply_binop(left, right, |x, y| x * y),
                            x if x == Opcode::Div as u32 => apply_binop(left, right, |x, y| x / y),
                            _ => unreachable!(),
                        };

                        frame.regs[a] = result;
                    }
                    x if x == Opcode::Eq as u32 => {
                        // A = target, B = left, C = right
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("Eq: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = match (&frame.regs[b], &frame.regs[c]) {
                            (RuntimeValue::I32(l), RuntimeValue::I32(r)) => *l == *r,
                            (RuntimeValue::Str(l), RuntimeValue::Str(r)) => l == r,
                            (RuntimeValue::USize(l), RuntimeValue::USize(r)) => l == r,
                            (RuntimeValue::Empty, RuntimeValue::Empty) => true,
                            (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => l == r,
                            // You can add more cross-type comparisons if needed
                            _ => false,
                        };
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::Lt as u32 => {
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("Lt: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = match (&frame.regs[b], &frame.regs[c]) {
                            (RuntimeValue::I32(l), RuntimeValue::I32(r)) => l < r,
                            (RuntimeValue::USize(l), RuntimeValue::USize(r)) => l < r,
                            // fallthrough / panic for invalid compares could be used instead
                            _ => panic!("Lt: unsupported operand types: {:?} {:?}", frame.regs[b], frame.regs[c]),
                        };
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::Le as u32 => {
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("Le: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = match (&frame.regs[b], &frame.regs[c]) {
                            (RuntimeValue::I32(l), RuntimeValue::I32(r)) => l <= r,
                            (RuntimeValue::USize(l), RuntimeValue::USize(r)) => l <= r,
                            _ => panic!("Le: unsupported operand types: {:?} {:?}", frame.regs[b], frame.regs[c]),
                        };
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::LNot as u32 => {
                        // A = target, Bx encodes source register in this encoding; compiler uses pack_i_abx(Opcode::LNot, reg, reg)
                        // In your code you emitted LNot as pack_i_abx(Opcode::LNot, reg, reg) earlier.
                        // We'll read source from bx (same as how LoadConst reads).
                        let src = bx as usize;
                        if a >= frame.regs.len() || src >= frame.regs.len() {
                            panic!("LNot: register OOB a={} src={}", a, src);
                        }
                        frame.regs[a] = RuntimeValue::Bool(truthy(&frame.regs[src]));
                    }
                    x if x == Opcode::LAnd as u32 => {
                        // A = target, B = left, C = right
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("LAnd: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = truthy(&frame.regs[b]) && truthy(&frame.regs[c]);
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::LOr as u32 => {
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("LOr: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = truthy(&frame.regs[b]) || truthy(&frame.regs[c]);
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::Jmp as u32 => {
                        // A = cond reg, Bx = target ip
                        let cond_reg = a;
                        let target = bx as usize;
                        if cond_reg >= frame.regs.len() {
                            panic!("Jmp: cond reg OOB a={}", cond_reg);
                        }
                        if truthy(&frame.regs[cond_reg]) {
                            frame.pc = target;
                        }
                    }
                    x if x == Opcode::Jn as u32 => {
                        // A = cond reg, Bx = target ip
                        let cond_reg = a;
                        let target = bx as usize;
                        if cond_reg >= frame.regs.len() {
                            panic!("Jn: cond reg OOB a={}", cond_reg);
                        }
                        if !truthy(&frame.regs[cond_reg]) {
                            frame.pc = target;
                        }
                    }
                    x if x == Opcode::BeginBlock as u32 => {
                        let current_stack_size = self.stack.len();
                        self.block_stack_bases.push(current_stack_size);
                        
                        let idx = self.frames.len() - 1;
                        let frame = &mut self.frames[idx];
                        
                        let mut local_base = frame.regs.len() - 1;
                        frame.local_var_bases.push(local_base);
                    }
                    
                    x if x == Opcode::EndBlock as u32 => {
                        if let Some(block_base) = self.block_stack_bases.pop() {
                            self.cleanup_stack(block_base);
                        } else {
                            panic!("EndBlock without matching BeginBlock");
                        }
                        
                        let idx = self.frames.len() - 1;
                        let frame = &mut self.frames[idx];
                        if let Some(local_base) = frame.local_var_bases.pop() {
                            for i in local_base..frame.regs.len() - 1 {
                                frame.regs[i] = RuntimeValue::Empty;
                            }
                        } else {
                            panic!("EndBlock without matching BeginBlock for locals");
                        }
                    }
                    _ => {
                        panic!("Unknown opcode {} in proto {}", opcode, frame.proto_index);
                    }
                }
            }
        }
    }

    fn push_frame(&mut self, proto_index: usize) -> usize {
        let stack_base = self.stack.len();  // Current stack size becomes this frame's base
        let regs = vec![RuntimeValue::Empty; self.frame_regs];
        let frame = CallFrame { 
            proto_index, 
            pc: 0, 
            regs, 
            stack_base,
            local_var_bases: Vec::new()
        };
        let len = self.frames.len();
        self.frames.push(frame);
        len
    }

    /// Pop the top frame and propagate its return value to the caller.
    /// Clean up all stack-allocated structs from this frame.
    fn pop_frame_and_propagate(&mut self, ret: Option<RuntimeValue>) {
        let callee = self.frames.pop().expect("pop_frame on empty stack");

        let is_top_level = self.frames.is_empty();

        // Clean up all structs allocated in this frame
        self.cleanup_stack(callee.stack_base);

        let return_val = if is_top_level {
            // No caller => no pointer check
            ret.or_else(|| callee.regs.get(0).cloned())
        } else {
            // Caller exists => run validity check
            ret.or_else(|| {
                match callee.regs.get(0) {
                    Some(RuntimeValue::Ptr(other, stack_idx)) => {
                        if *stack_idx > callee.stack_base {
                            println!("Warning: returning invalid struct pointer");
                            Some(RuntimeValue::Empty)
                        } else {
                            Some(RuntimeValue::Ptr(other.clone(), *stack_idx))
                        }
                    }
                    Some(other) => Some(other.clone()),
                    None => Some(RuntimeValue::Empty),
                }
            })
        };

        if let Some(caller) = self.frames.last_mut() {
            let callee_proto_idx = callee.proto_index;
            let target_reg = caller.regs.iter().position(|rv|
                matches!(rv, RuntimeValue::Function(idx) if *idx == callee_proto_idx)
            );

            if let Some(ridx) = target_reg {
                caller.regs[ridx] = return_val.unwrap_or(RuntimeValue::Empty);
            } else {
                caller.regs[0] = return_val.unwrap_or(RuntimeValue::Empty);
            }
        }
    }
}

fn apply_binop<F>(left: &RuntimeValue, right: &RuntimeValue, op: F) -> RuntimeValue
    where
        F: Fn(i32, i32) -> i32,
{
    match (left, right) {
        (RuntimeValue::I32(l), RuntimeValue::I32(r)) => RuntimeValue::I32(op(*l, *r)),
        _ => panic!("Binary operation applied to non-I32 operands: {:?} {:?}", left, right),
    }
}

fn truthy(rv: &RuntimeValue) -> bool {
    match rv {
        RuntimeValue::Bool(b) => *b,
        RuntimeValue::Empty => false,
        other => true
    }
}
