use std::str::FromStr;
use syn;
use synom;

use function::{Function, LispFnType};

/// Arguments of the lisp_fn attribute.
pub struct LispFnArgs {
    /// Desired Lisp name of the function.
    /// If not given, derived as the Rust name with "_" -> "-".
    pub name: String,
    /// Desired C name of the related statics (with F and S appended).
    /// If not given, same as the Rust name.
    pub c_name: String,
    /// Minimum number of required arguments.
    /// If not given, all arguments are required for normal functions,
    /// and no arguments are required for MANY functions.
    pub min: i16,
    /// Used to define if the lisp function is interactive.
    pub intspec: Option<String>,
}

pub fn parse(input: &str, function: &Function) -> Result<LispFnArgs, &'static str> {
    let kv = match parse_arguments(input) {
        synom::IResult::Done(_, o) => o,
        synom::IResult::Error => return Err("failed to parse `lisp_fn` arguments"),
    };

    Ok(parse_kv(kv, function))
}

fn parse_kv(kv_list: Vec<(syn::Ident, syn::StrLit)>, function: &Function) -> LispFnArgs {
    let mut name = None;
    let mut c_name = None;
    let mut intspec = None;
    let mut min = match function.fntype {
        LispFnType::Many => 0,
        LispFnType::Normal(n) => n,
    };
    for (ident, string) in kv_list {
        match ident.as_ref() {
            "name" => name = Some(string.value),
            "c_name" => c_name = Some(string.value),
            "min" => min = i16::from_str(&string.value).unwrap(),
            "intspec" => intspec = Some(string.value),
            _ => (), // TODO: throw a warning?
        }
    }
    LispFnArgs {
        name: name.unwrap_or_else(|| function.name.to_string().replace("_", "-")),
        c_name: c_name.unwrap_or_else(|| function.name.to_string()),
        min: min,
        intspec: intspec,
    }
}

named!(parse_arguments -> Vec<(syn::Ident, syn::StrLit)>,
    opt_vec!(
        do_parse!(
                 punct!("(")
        >> args: separated_list!(punct!(","), key_value)
        >>       punct!(")")
        >>       (args)
        )
    )
);

named!(key_value -> (syn::Ident, syn::StrLit),
    do_parse!(
       key: call!(syn::parse::ident)
    >>      option!(alt!(punct!(" ") | punct!("\t") | punct!("\n")))
    >>      punct!("=")
    >>      option!(alt!(punct!(" ") | punct!("\t") | punct!("\n")))
    >> val: call!(syn::parse::string)
    >>      (key, val)
    )
);


#[test]
fn parse_args_str() {
    let args =
        parse_arguments(r#"(name = "foo", min = "0")"#).expect("cannot parse lisp_fn attributes");

    // name = "foo"
    assert_eq!(format!("{}", args[0].0), "name");
    assert_eq!(format!("{}", args[0].1.value), "foo");

    // min = "0"
    assert_eq!(format!("{}", args[1].0), "min");
    assert_eq!(format!("{}", args[1].1.value), "0");
}
