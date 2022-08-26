/* Types */

pub mod parse;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    String,
    Boolean,
    Function { param_ty: Box<Type>, ret: Box<Type> },

    AttributeSet { attributes: Vec<(String, Type)> },
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Integer => write!(f, "integer"),
            Type::String => write!(f, "string"),
            Type::Boolean => write!(f, "boolean"),
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
                write!(f, "{{ {} }}", attributes)
            }
        }
    }
}

