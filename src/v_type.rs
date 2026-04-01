
#[derive(Debug, Clone, PartialEq)]
pub enum VType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    F32,
    F64,
    USize,
    Func,
    Empty,
    Bool,
    String,
    Struct(String),
    Function,
    Ref(
        Box<VType>
    ),
    Array(
        Box<VType>,
        usize
    ),
}

pub fn is_vtype(name: &str) -> bool {
    matches!(name,
    |   "U8"
    |   "I8"
    |   "U16"
    |   "I16"
    |   "I32"
    |   "U32"
    |   "F32"
    |   "F64"
    |   "Empty"
    |   "Func"
    |   "Bool"
    |   "String"
    |   "USize"
    |   "Int"
    |   "Array"
    )
}

pub fn vty_from_str(name: &str, a: Option<&str>, b: Option<&str>) -> Result<VType, String> {
    match name {
        "U8" => Ok(VType::U8),
        "I8" => Ok(VType::I8),
        "U16" => Ok(VType::U16),
        "I16" => Ok(VType::I16),
        "I32" => Ok(VType::I32),
        "U32" => Ok(VType::U32),
        "F32" => Ok(VType::F32),
        "F64" => Ok(VType::F64),
        "Empty" => Ok(VType::Empty),
        "Func" => Ok(VType::Func),
        "Bool" => Ok(VType::Bool),
        "String" => Ok(VType::String),
        "USize" => Ok(VType::USize),
        "Int" => Ok(VType::I32),
        "Array" => {
            if let Some(arg_a) = a {
                if let Some(arg_b) = b {
                    return Ok(
                        VType::Array(
                            Box::new(
                                vty_from_str(arg_a, None, None).unwrap()
                            ),
                            str::parse::<usize>(arg_b).unwrap()
                        )
                    )
                }
            }
            Ok(VType::Array(
                Box::new(VType::Empty),
                0
            ))
        }
        _ => Err("invalid enum name".to_string())
    }
}

pub fn sizeof(v: VType) -> usize {
    match v {
        VType::U8 => 1,
        VType::I8 => 1,
        VType::U16 => 2,
        VType::I16 => 2,
        _ => 1
    }
}

// pub fn type_of(v: Value) -> VType {
//     match v {
//         Value::Byte(..) => VType::I8,
//         Value::UByte(..) => VType::U8,
//         Value::Short(..) => VType::I16,
//         Value::UShort(..) => VType::U16,
//         Value::Int(..) => VType::I32,
//         Value::UInt(..) => VType::U32,
//         Value::Float(..) => VType::F32,
//         Value::Double(..) => VType::F64,
//         Value::Str(..) => VType::I8,
//         Value::Empty => VType::Empty,
//         Value::Bool(..) => VType::Bool,
//         _ => panic!("cannot get type of '{:?}'", v)
//     }
// }