/* Types */

pub mod parse;
pub mod parse_utils;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    String,
    Bool,
    Function {
        quantifier: Option<String>,
        param_ty: Box<Type>,
        ret: Box<Type>,
    },
    AttributeSet {
        attributes: Vec<(String, Type)>,
    },
    Never,
    Var(String),
    List(Box<Type>),
}

impl Type {
    pub fn mentions(&self, tyvar: &str) -> bool {
        match self {
            Type::Integer => false,
            Type::String => false,
            Type::Bool => false,
            Type::Function {
                quantifier: _,
                param_ty,
                ret,
            } => param_ty.mentions(tyvar) || ret.mentions(tyvar),
            Type::AttributeSet { attributes } => {
                attributes.iter().any(|(_, ty)| ty.mentions(tyvar))
            }
            Type::Never => false,
            Type::Var(varname) => tyvar == varname,
            Type::List(ty) => ty.mentions(tyvar),
        }
    }

    pub fn subst(&self, s: &str, ty: &Type) -> Self {
        match self {
            Type::Integer => Type::Integer,
            Type::String => Type::String,
            Type::Bool => Type::Bool,
            Type::Function {
                quantifier,
                param_ty,
                ret,
            } => Type::Function {
                quantifier: quantifier.clone(),
                param_ty: Box::new(param_ty.subst(s, ty)),
                ret: Box::new(ret.subst(s, ty)),
            },
            Type::AttributeSet { attributes } => Type::AttributeSet {
                attributes: attributes
                    .iter()
                    .map(|(k, v)| (k.clone(), v.subst(s, ty)))
                    .collect(),
            },
            Type::Never => Type::Never,
            Type::Var(varname) => {
                if varname == s {
                    ty.clone()
                } else {
                    Type::Var(varname.clone())
                }
            }
            Type::List(ty) => ty.subst(s, ty),
        }
    }

    pub fn apply_substs(&self, substs: &Vec<(String, Type)>) -> Self {
        let mut ty = self.clone();

        for (tyvar, substed_ty) in substs {
            ty = ty.subst(&tyvar, &substed_ty);
        }

        ty
    }
}

impl std::str::FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Type, ()> {
        // TODO: this deserves to have an explicit error
        parse::parse_type(s.to_string()).map_err(|_| ())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn display_fn(quantifier: &Option<String>, param_ty: &Type, ret_ty: &Type) -> String {
            let quantifier_prefix = match quantifier {
                Some(quantifier) => format!("{}.", quantifier),
                None => format!(""),
            };

            let f = match *param_ty {
                Type::Function { .. } => format!("({}) -> {}", param_ty, ret_ty),
                _ => format!("{} -> {}", param_ty, ret_ty),
            };

            format!("{}{}", quantifier_prefix, f)
        }

        match self {
            Type::Integer => write!(f, "integer"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "boolean"),
            Type::Function {
                quantifier,
                param_ty,
                ret,
            } => write!(f, "{}", display_fn(quantifier, param_ty, ret)),
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
            Type::Never => write!(f, "never"),
            Type::Var(v) => write!(f, "{}", v),
            Type::List(ty) => write!(f, "{}[]", ty),
        }
    }
}
