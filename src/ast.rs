#![allow(unused)]

use crate::lexer::Token;
use crate::v_type::{VType, vty_from_str};
use core::option::Option::None;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::FromStr;
use std::vec::IntoIter;

#[derive(Debug, Clone)]
pub struct Parameter {
    pub ident: String,
    pub v_type: VType
}

#[derive(Debug, Clone)]
pub enum AstNode {
    // Literals
    Byte(i8),
    UByte(u8),
    Short(i16),
    UShort(u16),
    Integer(i32),
    UInteger(u32),
    Float(f32),
    Double(f64),
    String(String),

    // Variables
    Identifier(String),
    FieldKey(String),
    Declaration {
        name: Box<AstNode>,
        value: Box<AstNode>,
        v_type: VType,
    },
    Assignment {
        assignee: Box<AstNode>,
        value: Box<AstNode>,
    },

    // Operations
    BinaryOp {
        op: String,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    UnaryOp {
        op: String,
        value: Box<AstNode>
    },

    // Function-related
    Function {
        name: String,
        params: Vec<Parameter>,
        body: Vec<Box<AstNode>>,
        return_type: VType,
    },
    Call {
        callee: Box<AstNode>,
        args: Vec<Box<AstNode>>,
    },
    Index {
        target: Box<AstNode>,
        index: Box<AstNode>,
    },
    Return {
        args: Vec<AstNode>
    },

    // Struct-related
    StructDecl {
        name: String,
        fields: Vec<Parameter>,
        struct_type: VType
    },
    StructLiteral {
        name: String,
        fields: Vec<(String, AstNode)>,
    },
    ArrayLiteral {
        exprs: Vec<Box<AstNode>>
    },
    WhileLoop {
        condition: Box<AstNode>,
        body: Vec<Box<AstNode>>
    },
    ForLoopIter {
        iteratee: Box<AstNode>,
        params: Vec<(String, AstNode)>,
    },
    ConditionalBranch {
        condition: Option<Box<AstNode>>, // condition: None means the branch is unconditional
        body: Vec<Box<AstNode>>,
        next: Option<Box<AstNode>>
    },
    ForLoopIncr {
        counter_var_name: String,
        start: Box<AstNode>,
        end: Box<AstNode>,
        incr: Option<Box<AstNode>>,
    },
    DoBlock(Vec<Box<AstNode>>),
    Program(Vec<AstNode>),
}

pub struct Parser {
    defined_struct_types: HashMap<String, VType>,
    tokens: Peekable<IntoIter<Token<'static>>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token<'static>>) -> Self {
        Parser {
            defined_struct_types: HashMap::new(),
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<AstNode, String> {
        let mut program = vec![];

        while self.tokens.peek().is_some() {
            program.push(self.parse_statement()?);
        }

        Ok(AstNode::Program(program))
    }

    fn parse_statement(&mut self) -> Result<AstNode, String> {
        match self.tokens.peek() {
            Some(Token::DECLARE) => {
                self.tokens.next();
                self.parse_declaration()
            }
            Some(Token::FN) => {
                self.tokens.next();
                self.parse_function()
            }
            Some(Token::STRUCT) => {
                self.tokens.next();
                self.parse_struct_declaration()
            }
            Some(Token::WHILE) => {
                self.tokens.next();
                self.parse_while_loop()
            }
            Some(Token::DO) => {
                self.tokens.next();
                let r = self.parse_body();
                if let Ok(body) = r {
                    Ok(AstNode::DoBlock(body))
                }
                else {
                    Err(format!("error while parsing do block: {:?}", r).into())
                }
            }
            Some(Token::IF) => {
                self.tokens.next();
                self.parse_conditional_branch()
            }
            _ => self.parse_expression(),
        }
    }

    fn parse_conditional_branch(&mut self) -> Result<AstNode, String> {
        match self.tokens.next() {
            Some(Token::PAREN("(")) => {}
            other => return Err(format!("Expected '(' after if keyword, got '{:?}'", other))
        }
        let condition = self.parse_expression()?;
        match self.tokens.next() {
            Some(Token::PAREN(")")) => {}
            other => return Err(format!("Expected ')' after if condition, got '{:?}'", other))
        }

        let body = self.parse_body()?;
        let mut next = None;
        match self.tokens.peek() {
            Some(Token::ELSEIF) => {
                self.tokens.next();
                next = Some(Box::new(self.parse_conditional_branch()?));
            }
            Some(Token::ELSE) => {
                self.tokens.next();
                let body = self.parse_body()?;
                next = Some(Box::new(AstNode::ConditionalBranch { condition: None, body, next: None}));
            }
            _ => {}
        }

        Ok(AstNode::ConditionalBranch {
            condition: Some(Box::new(condition)),
            body,
            next
        })
    }

    fn parse_for_loop(&mut self) -> Result<AstNode, String> {
        todo!("unfinished!");
    }

    fn parse_while_loop(&mut self) -> Result<AstNode, String> {
        match self.tokens.next() {
            Some(Token::PAREN("(")) => {}
            other => return Err(format!("Expected '(' after while keyword, got '{:?}'", other))
        }
        let condition = self.parse_expression()?;
        match self.tokens.next() {
            Some(Token::PAREN(")")) => {}
            other => return Err(format!("Expected ')' after while condition, got '{:?}'", other))
        }

        let body = self.parse_body()?;
        Ok(AstNode::WhileLoop {
            condition: Box::new(condition),
            body
        })
    }
    
    fn parse_struct_declaration(&mut self) -> Result<AstNode, String> {
        let name = match self.tokens.next() {
            Some(Token::IDENT(name)) => name.to_string(),
            _ => return Err("Expected struct name".to_string()),
        };

        match self.tokens.next() {
            Some(Token::PAREN("{")) => {}
            _ => return Err("Expected '{' after struct name".to_string()),
        }

        let mut fields = Vec::new();
        let struct_type = VType::Struct(name.clone());
        self.defined_struct_types.insert(name.clone(), struct_type.clone());
        
        loop {
            match self.tokens.peek() {
                Some(Token::PAREN("}")) => {
                    self.tokens.next();
                    break;
                }
                Some(Token::IDENT(field_name)) => {
                    let field_name = field_name.to_string();
                    self.tokens.next();

                    match self.tokens.next() {
                        Some(Token::PUNCT(":")) => {}
                        _ => return Err("Expected ':' after field name".to_string()),
                    }

                    let v_type = self.parse_type()?;
                    fields.push(Parameter { ident: field_name, v_type });
                    match self.tokens.peek() {
                        Some(Token::PUNCT(",")) => { self.tokens.next(); }
                        Some(Token::PAREN("}")) => {}
                        _ => return Err("Expected ',' or '}' after struct field".to_string()),
                    }
                }
                _ => return Err("Unexpected token in struct declaration".to_string()),
            }
        }
        Ok(AstNode::StructDecl { name, fields, struct_type })
    }

    fn parse_struct_literal(&mut self, name: String) -> Result<AstNode, String> {
        match self.tokens.next() {
            Some(Token::PAREN("{")) => {}
            _ => return Err("Expected '{' after struct type name".to_string()),
        }

        let mut fields = Vec::new();
        loop {
            match self.tokens.peek() {
                Some(Token::PAREN("}")) => {
                    self.tokens.next();
                    break;
                }
                Some(Token::IDENT(field_name)) => {
                    let field_name = field_name.to_string();
                    self.tokens.next();

                    match self.tokens.next() {
                        Some(Token::PUNCT(":")) => {}
                        _ => return Err("Expected ':' after field name".to_string()),
                    }

                    let value = self.parse_expression()?;
                    fields.push((field_name, value));

                    match self.tokens.peek() {
                        Some(Token::PUNCT(",")) => { self.tokens.next(); }
                        Some(Token::PAREN("}")) => {}
                        _ => return Err("Expected ',' or '}' after struct field value".to_string()),
                    }
                }
                _ => return Err("Unexpected token in struct literal".to_string()),
            }
        }

        Ok(AstNode::StructLiteral { name, fields })
    }

    fn parse_array_literal(&mut self) -> Result<AstNode, String> {
        let mut exprs = vec![];
        loop {
            let tok = self.tokens.peek();
            match tok {
                Some(Token::PAREN("}")) => {
                    break;
                }
                Some(..) => {
                    let expr = self.parse_expression()?;
                    exprs.push(Box::new(expr));
                }
                None => return Err("array literal ended prematurely".into())
            }
        }
        Ok(AstNode::ArrayLiteral {exprs})
    }

    fn parse_function(&mut self) -> Result<AstNode, String> {
        let name = match self.tokens.next() {
            Some(Token::IDENT(name)) => name,
            _ => return Err("Expected function name".to_string())
        };
        match self.tokens.next() {
            Some(Token::PAREN("(")) => {},
            _ => return Err("Expected '(' after 'fn'".to_string()),
        }
        let mut params: Vec<Parameter> = Vec::new();
        loop {
            let token_opt = self.tokens.peek().cloned();
            match token_opt {
                Some(Token::PAREN(")")) => {
                    self.tokens.next();
                    break;
                }
                Some(Token::IDENT(name)) => {
                    self.tokens.next();

                    match self.tokens.next() {
                        Some(Token::PUNCT(":")) => {}
                        _ => return Err("Parameter must be followed by type assignment".to_string()),
                    }
                    let v_type = self.parse_type()?;
                    params.push(Parameter { ident: name.to_string(), v_type });

                    let after_param = self.tokens.peek().cloned();
                    match after_param {
                        Some(Token::PAREN(")")) => continue,
                        Some(Token::PUNCT(",")) => { self.tokens.next(); }
                        _ => return Err("Expected ',' or ')' after parameter".to_string()),
                    }
                }
                _ => return Err("Expected identifier or ')' in parameter list".to_string()),
            }
        }

        let has_arrow = match self.tokens.peek() {
            Some(Token::ARROW) => {
                self.tokens.next();
                true
            }
            _ => false
        };

        let return_type = if has_arrow {
            self.parse_type()?
        } else {
            VType::Empty
        };

        let body = self.parse_body()?;
        Ok(AstNode::Function { name: name.to_string(), params, body, return_type })
    }

    fn parse_type(&mut self) -> Result<VType, String> {
        match self.tokens.next() {
            Some(Token::IDENT(type_name)) => {
                if let Ok(t) = vty_from_str(type_name, None, None) {
                    if let Some(Token::BINOP("<")) = self.tokens.peek() {
                        // Consume opening angle bracket <
                        self.tokens.next();

                        let mut a: String = "Empty".into();
                        let mut b: String = "Empty".into();
                        let mut i: usize = 0;
                        while (*self.tokens.peek().unwrap() != Token::BINOP(">")) {
                            let token = self.tokens.next().unwrap();
                            i += 1;
                            if let Token::IDENT(generic_arg) = token {
                                if i == 1 {
                                    a = generic_arg.to_string();
                                }
                                else if i == 2 {
                                    b = generic_arg.to_string();
                                }
                            }
                            else if let Token::INT(integer) = token {
                                let par = integer.to_string();
                                if i == 1 {
                                    a = par;
                                }
                                else if i == 2 {
                                    b = par;
                                }
                            }
                            let next = self.tokens.next();
                            if let Some(Token::PUNCT(",")) = next {}
                            else if let Some(Token::BINOP(">")) = next {break}
                            else {
                                return Err("Expected comma after generic type arg".into())
                            }
                        }

                        Ok(vty_from_str(type_name, Some(a.as_str()), Some(b.as_str()))?)
                    }
                    else {
                        Ok(t)
                    }
                } 
                else if self.defined_struct_types.contains_key(type_name) {
                    Ok(VType::Struct(type_name.to_string()))
                } else {
                    return Err(format!("Unknown type '{}'", type_name));
                }
            }
            Some(Token::BINOP("&")) => {
                if true {
                    return Err("& reference type not yet supported".into())
                }
                let t = self.parse_type();
                if let Ok(inner) = t {
                    Ok(VType::Ref(Box::new(inner)))
                }
                else {
                    t
                }
            }
            _ => Err("Expected type name in struct field".to_string()),
        }
    }

    fn parse_body(&mut self) -> Result<Vec<Box<AstNode>>, String> {
        match self.tokens.next() {
            Some(Token::PAREN("{")) => {},
            _ => return Err("Expected '{' before code block".to_string()),
        }
        let mut body = Vec::new();
        loop {
            match self.tokens.peek() {
                Some(Token::PAREN("}"))/*Some(Token::END)*/ => {
                    self.tokens.next();
                    break;
                }
                None => return Err("Unexpected end of input in code block".to_string()),
                _ => {
                    let stmt = self.parse_statement()?;
                    body.push(Box::new(stmt));
                }
            }
        }
        Ok(body)
    }

    fn parse_declaration(&mut self) -> Result<AstNode, String> {
        let name = match self.tokens.next() {
            Some(Token::IDENT(ident)) => AstNode::Identifier(ident.to_string()),
            // Some(Token::FN) => {
            //     let r = self.parse_function();
            //     return r
            // }
            // Some(Token::STRUCT) => {
            //     let r = self.parse_struct_declaration();
            //     return r
            // }
            _ => return Err("Expected identifier after 'let'".to_string()),
        };
        
        // Consume colon ':'
        match self.tokens.next() {
            Some(Token::PUNCT(":")) => {},
            _ => return Err("Expected colon after variable name".to_string()),
        };
        let v_type = self.parse_type()?;

        match self.tokens.next() {
            Some(Token::BINOP("=")) => {},
            _ => return Err("Expected '=' after type in declaration".to_string()),
        }
        
        let value = self.parse_expression()?;

        Ok(AstNode::Declaration {
            name: Box::new(name),
            value: Box::new(value),
            v_type
        })
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, min_precedence: i32) -> Result<AstNode, String> {
        let mut left = self.parse_unary_expression()?; 
        
        loop {
            let token = match self.tokens.peek() {
                Some(t) => t,
                None => break,
            }.clone();

            // Check if token is a binary operator
            let (is_binop, op_str) = match token {
                Token::BINOP(op) => (true, op),
                _ => (false, ""),
            };

            if !is_binop {
                break;
            }

            let precedence = get_operator_precedence(op_str);

            if precedence < min_precedence {
                break;
            }

            let is_assignment_op = matches!(op_str, "=" | "+=" | "-=" | "*=" | "/=" | "%=");
            if is_assignment_op {
                self.tokens.next();
                let right = self.parse_binary_expression(1)?;

                if op_str == "=" {
                    return Ok(AstNode::Assignment {
                        assignee: Box::new(left),
                        value: Box::new(right),
                    });
                }
                else {
                    match &left {
                        AstNode::Identifier(name) => {
                            let base_op = op_str.chars().nth(0).unwrap().to_string();
                            return Ok(AstNode::Assignment {
                                assignee: Box::new(AstNode::Identifier(name.clone())),
                                value: Box::new(AstNode::BinaryOp {
                                    op: base_op,
                                    left: Box::new(left),
                                    right: Box::new(right)
                                })
                            });
                        }
                        _ => {
                            return Err(format!("Cannot apply compound assignment to: '{:#?}'", left).into());
                        }
                    }
                }
            }
            self.tokens.next();
            let right = self.parse_binary_expression(precedence + 1)?;

            left = AstNode::BinaryOp {
                op: op_str.to_string(),
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<AstNode, String> {
        let token_opt = self.tokens.peek().cloned();

        if let Some(Token::UNARY(op)) = token_opt {
            if op == "-" || op == "!" || op == "~" {
                self.tokens.next();
                let expr = self.parse_postfix()?;
                return Ok(AstNode::UnaryOp {
                    op: op.to_string(),
                    value: Box::new(expr),
                });
            }
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<AstNode, String> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.tokens.peek() {
                Some(Token::PAREN("(")) => {
                    self.tokens.next();
                    expr = self.finish_parse_call(expr)?;
                }
                Some(Token::PAREN("[")) => {
                    self.tokens.next();
                    expr = self.finish_parse_index(expr)?;
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<AstNode, String> {
        match self.tokens.next() {
            Some(Token::PAREN("(")) => {
                let expr = self.parse_expression()?;
                match self.tokens.next() {
                    Some(Token::PAREN(")")) => {}
                    _ => return Err("Expected ')'".to_string()),
                }
                Ok(expr)
            }
            Some(Token::INT(n)) => Ok(AstNode::Integer(n)),
            Some(Token::FLOAT(f)) => Ok(AstNode::Double(f)),
            Some(Token::STRING(s)) => Ok(AstNode::String(s.to_string())),
            Some(Token::IDENT(name)) => {
                if let Some(Token::PAREN("{")) = self.tokens.peek() {
                    // Struct literal
                    self.parse_struct_literal(name.to_string())
                }
                else {
                    Ok(AstNode::Identifier(name.to_string()))
                }
            }
            Some(Token::PAREN("{")) => {
                self.parse_array_literal()
            }
            Some(Token::RETURN) => {
                let args = match self.tokens.peek() {
                    // These tokens indicate end of statement or block - no return value
                    Some(Token::PUNCT(";")) | 
                    Some(Token::PAREN("}")) |
                    None => {
                        // Empty return
                        vec![]
                    }
                    // Any other token could potentially start an expression
                    _ => {
                        vec![self.parse_expression()?]
                    }
                };
                Ok(AstNode::Return { args })
            }
            token => Err(format!("Unexpected token in expression: {:?}", token)),
        }
    }

    fn finish_parse_call(&mut self, callee: AstNode) -> Result<AstNode, String> {
        let mut args = Vec::new();

        if let Some(Token::PAREN(")")) = self.tokens.peek() {
            self.tokens.next();
        } else {
            loop {
                let arg = self.parse_expression()?;
                args.push(Box::new(arg));

                match self.tokens.peek() {
                    Some(Token::PUNCT(",")) => { self.tokens.next(); }
                    Some(Token::PAREN(")")) => { self.tokens.next(); break; }
                    _ => return Err(format!("Expected ',' or ')' in call args")),
                }
            }
        }

        Ok(AstNode::Call { callee: Box::new(callee), args })
    }

    fn finish_parse_index(&mut self, target: AstNode) -> Result<AstNode, String> {
        let index_expr = self.parse_expression()?;

        match self.tokens.next() {
            Some(Token::PAREN("]")) => {}
            _ => return Err("Expected ']'".to_string()),
        }

        Ok(AstNode::Index { target: Box::new(target), index: Box::new(index_expr) })
    }
}

fn get_operator_precedence(op: &str) -> i32 {
    match op {
        "." => 15,
        "::" => 14,
        "*" | "/" | "%" => 12,
        "+" | "-" => 11,
        "<<" | ">>" => 10,
        "<" | "<=" | ">" | ">=" => 9,
        "==" | "!=" => 8,
        "&" => 7,
        "^" => 6,
        "|" => 5,
        "&&" => 4,
        "||" => 3,
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" => 2,
        _ => 0,
    }
}

pub fn parse(tokens: Vec<Token<'static>>) -> Result<AstNode, String> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

pub fn type_of(node: AstNode) -> VType
{
    match node {
        AstNode::Integer(..) => VType::I32,
        other => panic!("Unhandled ast::type_of case: {:#?}", other)
    }
}