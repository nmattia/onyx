/* Types */

pub mod parse;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    String,
    Bool,
    Function { param_ty: Box<Type>, ret: Box<Type> },
    AttributeSet { attributes: Vec<(String, Type)> },
    Union { left: Box<Type>, right: Box<Type> },
    Never,
}

impl std::str::FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Type, ()> {
        // TODO: this deserves to have an explicit error
        Ok(parse::parse(s.to_string()))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Integer => write!(f, "integer"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "boolean"),
            Type::Function { param_ty, ret } => match &**param_ty {
                Type::Function { .. } => write!(f, "({}) -> {}", param_ty, ret),
                _ => write!(f, "{} -> {}", param_ty, ret),
            },
            Type::AttributeSet { attributes } => {
                let attributes = attributes
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");
                if attributes.len() == 0 {
                    write!(f, "{{}}")
                } else {
                    write!(f, "{{ {} }}", attributes)
                }
            }
            Type::Union { left, right } => {
                write!(f, "{} | {}", left, right)
            }
            Type::Never => write!(f, "never"),
        }
    }
}
