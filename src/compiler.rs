use std::collections::HashMap;
use crate::ast::{type_of, AstNode, Parameter};
use crate::v_type::VType;
use crate::vm::NativeFunction;

#[derive(Debug, Clone)]
pub struct RegAlloc {
    next: u32,
}

impl RegAlloc {
    pub fn new() -> Self { Self { next: 0 } }
    pub fn alloc(&mut self) -> u32 {
        let r = self.next;
        self.next += 1;
        r
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub reg: u32,
    pub ty: VType
}

#[derive(Debug, Clone)]
pub struct PrototypeFunction {
    pub name: String,
    pub params: Vec<Parameter>,
    pub code: Vec<Instruction>,
    pub ctx: CompileCtx,
    pub return_type: VType,
}

#[derive(Debug, Clone)]
pub struct StructPrototype {
    pub name: String,
    pub fields: Vec<(String, VType)>,
    pub struct_type: VType
}

#[derive(Debug, Clone)]
pub struct CompileCtx {
    pub consts: Vec<ConstValue>,
    pub symbols: Vec<Symbol>,
    pub protos: Vec<PrototypeFunction>,
    pub structs: Vec<StructPrototype>,
    pub regs: RegAlloc,
    pub native_map: HashMap<String, usize>,
}

impl CompileCtx {
    pub fn new() -> Self {
        Self {
            consts: vec![],
            symbols: vec![],
            protos: vec![],
            structs: vec![],
            regs: RegAlloc::new(),
            native_map: HashMap::new(),
        }
    }

    pub fn with_parent(parent: &CompileCtx) -> Self {
        Self {
            consts: (*parent.consts).to_vec(),
            symbols: (*parent.symbols).to_vec(),
            protos: (*parent.protos).to_vec(),
            structs: (*parent.structs).to_vec(),
            regs: RegAlloc::new(),
            native_map: parent.native_map.clone(),
        }
    }

    #[inline]
    pub fn add_const(&mut self, val: ConstValue) -> u32 {
        self.consts.push(val);
        (self.consts.len() - 1) as u32
    }

    pub fn find_const(&mut self, val: ConstValue) -> Option<u32> {
        match val {
            ConstValue::Function(idx) => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::Function(e) => {
                                *e == idx
                            }
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            ConstValue::NativeFunction(idx) => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::NativeFunction(e) => {
                                *e == idx
                            }
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            ConstValue::I32(i) => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::I32(e) => {
                                *e == i
                            }
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            ConstValue::USize(i) => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::USize(e) => {
                                *e == i
                            }
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            ConstValue::Str(i) => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::Str(e) => {
                                *e == i
                            }
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            ConstValue::Struct(i) => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::Struct(e) => {
                                *e == i
                            }
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            ConstValue::Type(i) => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::Type(e) => {
                                *e == i
                            }
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            ConstValue::Empty => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::Empty => true,
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            ConstValue::Bool(i) => {
                let v = self.consts.iter()
                    .position(|x| {
                        match x {
                            ConstValue::Bool(e) => {
                                *e == i
                            }
                            _ => false
                        }
                    });
                if let Some(n) = v {Some(n as u32)} else {None}
            }
            other => panic!("unhandled find_const val: '{:?}'", other)
        }
    }

    #[inline]
    pub fn find_or_add_const(&mut self, val: ConstValue) -> u32 {
        self.find_const(val.clone())
            .or_else(||{Some(self.add_const(val))})
            .expect("not found")
    }

    #[inline]
    pub fn add_symbol(&mut self, name: String, reg: u32, ty: VType) {
        self.symbols.push(Symbol { name, reg, ty });
    }

    #[inline]
    pub fn find_fn_addr_by_name(&mut self, name: &str) -> Option<usize> {
        self.protos
            .iter()
            .position(|p| p.name == name)
    }

    #[inline]
    pub fn find_symbol(&mut self, name: &str) -> Option<&Symbol> {
        self.symbols.iter()
            .find(|s| s.name == name)
    }

    #[inline]
    pub fn register_native_fns(&mut self, fns: &Vec<NativeFunction>) {
        for (idx, nf) in fns.iter().enumerate() {
            self.native_map.insert(nf.name.clone(), idx);
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    U8(u8),
    I8(i8),

    U16(u16),
    I16(i16),

    U32(u32),
    I32(i32),

    F32(f32),
    F64(f64),
    USize(usize),
    Str(String),
    Bool(bool),
    Empty,

    Function(usize),
    NativeFunction(usize),
    Struct(usize),
    Array(usize, usize),
    Type(VType),
}

pub type Instruction = u32;

#[repr(u32)]
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    LoadConst,
    Move,

    Call,
    Ret,
    BeginBlock,
    EndBlock,
    Noop,

    NewStruct,
    StoreStructField,
    LoadStructField,

    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Cast,
    
    NewArray,
    GetArrayIdx,
    SetArrayIdx,

    Jmp, // Will only jump to the ip if the value in register a is true
    Jn,

    Eq,
    Le,
    Lt,

    LNot,
    LAnd,
    LOr,

    BNot,
    BAnd,
    BOr,
    BXOr,
    BLShift,
    BRShift
}

impl TryFrom<u32> for Opcode {
    type Error = String;
    
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            x if x == Opcode::LoadConst as u32 => Ok(Opcode::LoadConst),
            x if x == Opcode::Move as u32 => Ok(Opcode::Move),
            x if x == Opcode::Call as u32 => Ok(Opcode::Call),
            x if x == Opcode::Ret as u32 => Ok(Opcode::Ret),
            x if x == Opcode::Add as u32 => Ok(Opcode::Add),
            x if x == Opcode::Sub as u32 => Ok(Opcode::Sub),
            x if x == Opcode::Mul as u32 => Ok(Opcode::Mul),
            x if x == Opcode::Div as u32 => Ok(Opcode::Div),
            x if x == Opcode::Pow as u32 => Ok(Opcode::Pow),
            x if x == Opcode::BLShift as u32 => Ok(Opcode::BLShift),
            x if x == Opcode::BRShift as u32 => Ok(Opcode::BRShift),
            x if x == Opcode::BXOr as u32 => Ok(Opcode::BXOr),
            x if x == Opcode::BOr as u32 => Ok(Opcode::BOr),
            x if x == Opcode::BAnd as u32 => Ok(Opcode::BAnd),
            x if x == Opcode::Eq as u32 => Ok(Opcode::Eq),
            x if x == Opcode::Lt as u32 => Ok(Opcode::Lt),
            x if x == Opcode::Le as u32 => Ok(Opcode::Le),
            x if x == Opcode::LNot as u32 => Ok(Opcode::LNot),
            x if x == Opcode::LAnd as u32 => Ok(Opcode::LAnd),
            x if x == Opcode::LOr as u32 => Ok(Opcode::LOr),
            x if x == Opcode::Jmp as u32 => Ok(Opcode::Jmp),
            x if x == Opcode::Jn as u32 => Ok(Opcode::Jn),
            x if x == Opcode::BeginBlock as u32 => Ok(Opcode::BeginBlock),
            x if x == Opcode::EndBlock as u32 => Ok(Opcode::EndBlock),
            x if x == Opcode::NewStruct as u32 => Ok(Opcode::NewStruct),
            x if x == Opcode::StoreStructField as u32 => Ok(Opcode::StoreStructField),
            x if x == Opcode::LoadStructField as u32 => Ok(Opcode::LoadStructField),
            x if x == Opcode::Noop as u32 => Ok(Opcode::Noop),
            _ => Err(format!("Unknown opcode: {}", value)),
        }
    }
}

pub const OPCODE_BITS: u32 = 6;

pub const A_BITS: u32 = 8;
pub const B_BITS: u32 = 9;
pub const C_BITS: u32 = 9;

pub const A_SHIFT: u32 = OPCODE_BITS;
pub const B_SHIFT: u32 = A_SHIFT + A_BITS;
pub const C_SHIFT: u32 = B_SHIFT + B_BITS;

#[inline]
pub fn pack_i_abx(op: Opcode, a: u32, bx: u32) -> Instruction {
    (op as u32)
    | (a << A_SHIFT)
    | (bx << B_SHIFT)
}

#[inline]
pub fn pack_i_abc(op: Opcode, a: u32, b: u32, c: u32) -> Instruction {
    (op as u32) 
    | (a << A_SHIFT) 
    | (b << B_SHIFT) 
    | (c << C_SHIFT)
}

pub fn compile_expr(ast: AstNode, ctx: &mut CompileCtx, reg: u32) -> Vec<Instruction> {
    let mut code = vec![];
    
    match ast {
        AstNode::Integer(n) => {
            let cidx = ctx.find_or_add_const(ConstValue::I32(n));
            code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
        }
        AstNode::String(s) => {
            let cidx = ctx.find_or_add_const(ConstValue::Str(s));
            code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
        }
        AstNode::Identifier(name) => {
            let st = name.as_str();
            if st == "true" {
                let cidx = ctx.find_or_add_const(ConstValue::Bool(true));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if st == "false" {
                let cidx = ctx.find_or_add_const(ConstValue::Bool(false));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if st == "empty" {
                let cidx = ctx.find_or_add_const(ConstValue::Empty);
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if let Some(idx) = ctx.find_fn_addr_by_name(name.as_str()) {
                let cidx = ctx.find_or_add_const(ConstValue::Function(idx));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if let Some(native_idx) = ctx.native_map.get(&name) {
                let cidx = ctx.find_or_add_const(ConstValue::NativeFunction(*native_idx));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else {
                let sym = ctx.find_symbol(&name)
                    .unwrap_or_else(|| panic!("Unknown identifier `{}`", name));

                if sym.reg != reg {
                    code.push(pack_i_abx(Opcode::Move, reg, sym.reg));
                }
            }
        }
        AstNode::BinaryOp { op, left, right } => {
            let (lreg, rreg) = (ctx.regs.alloc(), ctx.regs.alloc());

            let mut negate_flag = false;
            let oper_code = match op.as_str() {
                "+" => Opcode::Add,
                "-" => Opcode::Sub,
                "*" => Opcode::Mul,
                "/" => Opcode::Div,
                "==" => Opcode::Eq,
                "!=" => {
                    negate_flag = true;
                    Opcode::Eq
                }
                "<" => Opcode::Lt,
                ">" => {
                    negate_flag = true;
                    Opcode::Lt
                }
                "<=" => Opcode::Le,
                ">=" => {
                    negate_flag = true;
                    Opcode::Le
                }
                "<<" => Opcode::BLShift,
                ">>" => Opcode::BRShift,
                "|" => Opcode::BOr,
                "&" => Opcode::BAnd,

                "||" => Opcode::LOr,
                "&&" => Opcode::LAnd,
                
                "." => {
                    let struct_reg = ctx.regs.alloc();
                    code.extend(compile_expr(*left, ctx, struct_reg));
                    
                    let field_idx = resolve_field_access(&*right, ctx, &struct_reg);
                    let field_idx_reg = ctx.regs.alloc();
                    let field_idx_const = ctx.find_or_add_const(ConstValue::USize(field_idx));
                    code.push(pack_i_abx(Opcode::LoadConst, field_idx_reg, field_idx_const));
                    
                    code.push(pack_i_abc(Opcode::LoadStructField, reg, struct_reg, field_idx as u32));
                    return code;
                }

                other => panic!("unhandled operator: {}", other)
            };
            
            code.extend(compile_expr(*left, ctx, lreg));
            code.extend(compile_expr(*right, ctx, rreg));
            code.push(pack_i_abc(oper_code, reg, lreg, rreg));
            if negate_flag {
                code.push(pack_i_abx(Opcode::LNot, reg, reg))
            }
        }
        AstNode::Call { callee, args } => {
            code.extend(compile_expr(*callee, ctx, reg));

            let mut arg_regs = vec![];
            for _ in 0..args.len() {
                let a = ctx.regs.alloc();
                arg_regs.push(a);
            }

            for (i, arg) in args.into_iter().enumerate() {
                let arg_reg = arg_regs[i];
                code.extend(compile_expr(*arg, ctx, arg_reg));
            }
            
            let num_args = arg_regs.len() as u32;
            // Convention: CALL A Bx -> function in R[A], args in R[A+1 .. A+Bx], returns into R[A]
            code.push(pack_i_abx(Opcode::Call, reg, num_args));
        }
        AstNode::StructLiteral { name, fields } => {
             let struct_idx = ctx.structs.iter()
                .position(|s| s.name == name)
                .expect("Struct should exist");
            let struct_const_idx = ctx.find_or_add_const(ConstValue::Struct(struct_idx));

            let struct_proto = ctx.structs.iter()
                .find(|s| s.name == name)
                .unwrap_or_else(|| panic!("Unknown struct type: {}", name));
            
            code.push(pack_i_abx(Opcode::NewStruct, reg, struct_const_idx));
            
            let mut field_regs = vec![];
            for _ in 0..fields.len() {
                let a = ctx.regs.alloc();
                field_regs.push(a);
            }
            let mut field_idcs = vec![];
            for (i, (field_name, ..)) in fields.iter().enumerate() {
                let field_idx = struct_proto.fields.iter()
                    .position(|(fname, _)| fname == field_name)
                    .unwrap_or_else(|| panic!("Unknown field '{}' for struct '{}'", field_name, name));
                field_idcs.insert(i, field_idx);
            }

            for (i, (field_name, field_value)) in fields.iter().enumerate() {
                let field_idx = field_idcs[i];
                let field_reg = field_regs[i];
                
                code.extend(compile_expr(field_value.clone(), ctx, field_reg));
                
                code.push(pack_i_abc(Opcode::StoreStructField, reg, field_idx as u32, field_reg));
            }
        }
        AstNode::ArrayLiteral { exprs } => {

        }
        other => panic!("unknown expr node: {:?}", other),
    }

    code
}

fn resolve_field_access(node: &AstNode, ctx: &CompileCtx, struct_reg: &u32) -> usize {
    match node {
        AstNode::Identifier(field_name) => {
            //TODO: Needs proper typechecking
            for struct_proto in &ctx.structs {
                if let Some(field_idx) = struct_proto.fields.iter()
                    .position(|(fname, _)| fname == field_name) {
                    return field_idx;
                }
            }
            panic!("Field '{}' not found in any struct", field_name);
        }
        AstNode::BinaryOp { op, left: _, right } if op == "." => {
            resolve_field_access(right, ctx, struct_reg)
        }
        _ => panic!("Invalid field access pattern: expected Identifier, got {:?}", node)
    }
}

pub fn compile_stmt(ast: AstNode, ctx: &mut CompileCtx, code: &mut Vec<Instruction>) {
    match ast {
        AstNode::Assignment { assignee, value } => {
            match *assignee {
                AstNode::Identifier(name) => {
                    if let Some(symbol) = ctx.find_symbol(&name) {
                        let target_reg = symbol.reg;
                        code.extend(compile_expr(*value, ctx, target_reg));
                    } else {
                        panic!("Undefined variable: {}", name);
                    }
                }
                AstNode::BinaryOp { op, left, right } if op == "." => {
                    let value_reg = ctx.regs.alloc();
                    code.extend(compile_expr(*value, ctx, value_reg));
                    let (struct_reg, field_indices) = compile_field_access_chain(*left, *right, ctx);
                    
                    // For nested field access, need to traverse to the final struct
                    // and then store to the final field
                    if field_indices.len() == 1 {
                        // Simple case: struct.field = value
                        code.push(pack_i_abc(Opcode::StoreStructField, struct_reg, field_indices[0] as u32, value_reg));
                    } else {
                        // Complex case: struct.field1.field2 = value
                        let mut current_reg = struct_reg;
                        for &field_idx in &field_indices[..field_indices.len()-1] {
                            let next_reg = ctx.regs.alloc();
                            code.push(pack_i_abc(Opcode::LoadStructField, next_reg, current_reg, field_idx as u32));
                            current_reg = next_reg;
                        }
                        
                        // Store to the final field
                        let final_field_idx = field_indices[field_indices.len()-1];
                        code.push(pack_i_abc(Opcode::StoreStructField, current_reg, final_field_idx as u32, value_reg));
                    }
                }
                other =>  panic!("Invalid assignment target '{:?}' - only identifiers and index chains supported", other)
            }
        }

        AstNode::Declaration { name, value, v_type } => {
            let t = type_of(*(value.clone()));
            if t == v_type {
                panic!("Expected {:?} for variable binding type, got {:?} as value type", v_type, t)
            }
            else if let AstNode::Identifier(name_str) = *name {
                let reg = ctx.regs.alloc();
                ctx.add_symbol(name_str, reg, v_type);
                code.extend(compile_expr(*value, ctx, reg));
            }
        }

        AstNode::StructDecl { name, fields, struct_type } => {
            let str_type = VType::Struct(name.clone());
            let struct_idx = ctx.structs.len();
            let reg = ctx.regs.alloc();
            ctx.add_symbol(name.clone(), reg, str_type.clone());
            ctx.add_const(ConstValue::Struct(struct_idx));

            let mut actual_fields = Vec::new();
            for p in fields {
                actual_fields.push(
                    (p.ident, p.v_type)
                )
            }

            let proto = StructPrototype {
                name: name.clone(),
                fields: actual_fields,
                struct_type: str_type
            };
            ctx.structs.push(proto)
        }
        
        AstNode::Function { name, params, body, return_type } => {
            let mut func_ctx = CompileCtx::with_parent(ctx);
            
            for param in &params {
                let reg = func_ctx.regs.alloc();
                func_ctx.add_symbol(param.ident.clone(), reg, param.v_type.clone());
            }

            let mut func_code = vec![];
            for stmt in body {
               compile_stmt(*stmt, &mut func_ctx, &mut func_code);
            }

            let proto_idx = ctx.protos.len();
            let reg = ctx.regs.alloc();
            ctx.add_symbol(name.clone(), reg, VType::Function);
            ctx.add_const(ConstValue::Function(proto_idx));
            
            let proto = PrototypeFunction {
                name: name.clone(),
                params: params,
                code: func_code,
                ctx: func_ctx,
                return_type,
            };
            ctx.protos.push(proto);
        }
        
        AstNode::Call { callee, args } => {
            let temp_reg = ctx.regs.alloc();
            code.extend(compile_expr(AstNode::Call { callee, args }, ctx, temp_reg));
        }
        
        AstNode::Return { args } => {
            let a = if args.is_empty() {
                0
            } else {
                let ret_reg = ctx.regs.alloc();
                code.extend(compile_expr(args[0].clone(), ctx, ret_reg));
                ret_reg + 1
            };
            
            //code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
            code.push(pack_i_abx(Opcode::Ret, a, 0));
        }
        
        AstNode::Program(nodes) => {
            let native_entries: Vec<(String, usize)> =
                ctx.native_map.iter().map(|(n, &idx)| (n.clone(), idx)).collect();
            
            for (name, native_idx) in native_entries {
                let reg = ctx.regs.alloc();
                ctx.add_symbol(name.clone(), reg, VType::Function);
                ctx.add_const(ConstValue::NativeFunction(native_idx));
            }
            
            for node in nodes {
                compile_stmt(node.clone(), ctx, code);
            }
        }

        AstNode::DoBlock(block) => {
            code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
            
            for stmt in block {
                compile_stmt(*stmt, ctx, code);
            }
            
            code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
        }
        AstNode::WhileLoop { condition, body } => {
            let loop_start = code.len();
            let cond_reg = ctx.regs.alloc();
            code.extend(compile_expr(*condition.clone(), ctx, cond_reg));
            
            let jmp_false_pos = code.len();
            code.push(pack_i_abx(Opcode::Noop, 0, 0));
            code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
            for stmt in body {
                compile_stmt(*stmt, ctx, code);
            }
            code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
            code.push(pack_i_abx(Opcode::Jmp, cond_reg, loop_start as u32)); // Use cond_reg instead of 0

            let exit_idx = code.len() as u32;
            code[jmp_false_pos] = pack_i_abx(Opcode::Jn, cond_reg, exit_idx);
        }
        AstNode::ConditionalBranch { condition, body, next } => {
            match condition {
                Some(condition) => {
                    // Conditional branch (if/elif)
                    let cond_reg = ctx.regs.alloc();
                    code.extend(compile_expr(*condition, ctx, cond_reg));
                    
                    // Jump to next branch if condition is false
                    let jmp_to_next_pos = code.len();
                    code.push(pack_i_abx(Opcode::Noop, 0, 0)); // Placeholder for Jn instruction
                    
                    // Compile this branch's body
                    code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
                    for stmt in body {
                        compile_stmt(*stmt, ctx, code);
                    }
                    code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
                    
                    // Jump to end of entire conditional chain (skip remaining branches)
                    let jmp_to_end_pos = code.len();
                    code.push(pack_i_abx(Opcode::Noop, 0, 0)); // Placeholder for Jmp instruction
                    
                    // Mark where the next branch starts
                    let next_branch_start = code.len() as u32;
                    
                    // Patch the "jump to next branch" instruction
                    code[jmp_to_next_pos] = pack_i_abx(Opcode::Jn, cond_reg, next_branch_start);
                    
                    // Recursively compile the next branch in the chain
                    if let Some(next_branch) = next {
                        compile_stmt(*next_branch, ctx, code);
                    }
                    
                    // Mark the end of the entire conditional chain
                    let end_of_chain = code.len() as u32;
                    
                    // Patch the "jump to end" instruction
                    code[jmp_to_end_pos] = pack_i_abx(Opcode::Jmp, 0, end_of_chain);
                }
                None => {
                    // Unconditional branch (else)
                    code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
                    for stmt in body {
                        compile_stmt(*stmt, ctx, code);
                    }
                    code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
                    
                    // No need to compile next branch - this is the final else clause
                    // Any remaining next branches would be unreachable dead code
                }
            }
        }
        other => panic!("unhandled node '{:?}'", other),
    }
}

fn compile_field_access_chain(left: AstNode, right: AstNode, ctx: &mut CompileCtx) -> (u32, Vec<usize>) {
    let mut field_indices = Vec::new();
    fn collect_field_indices(node: &AstNode, indices: &mut Vec<usize>, ctx: &CompileCtx) {
        match node {
            AstNode::Identifier(field_name) => {
                // Find the field index - in a real implementation you'd want proper type tracking
                for struct_proto in &ctx.structs {
                    if let Some(field_idx) = struct_proto.fields.iter()
                        .position(|(fname, _)| fname == field_name) 
                    {
                        indices.push(field_idx);
                        return;
                    }
                }
                panic!("Field '{}' not found in any struct", field_name);
            }
            AstNode::BinaryOp { op, left: _, right } if op == "." => {
                collect_field_indices(right, indices, ctx);
            }
            _ => panic!("Invalid field access pattern in chain: {:?}", node)
        }
    }
    
    // Get the base struct register
    let struct_reg = match left {
        AstNode::Identifier(name) => {
            let symbol = ctx.find_symbol(&name)
                .unwrap_or_else(|| panic!("Unknown identifier `{}`", name));
            symbol.reg
        }
        AstNode::BinaryOp { op, left, right } if op == "." => {
            // Handle nested struct access
            let (base_reg, mut base_indices) = compile_field_access_chain(*left, *right, ctx);
            field_indices.append(&mut base_indices);
            base_reg
        }
        _ => panic!("Invalid base for field access: {:?}", left)
    };
    
    // Collect field indices from the right side
    collect_field_indices(&right, &mut field_indices, ctx);
    
    (struct_reg, field_indices)
}