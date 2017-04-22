use std::str::FromStr;
use std::collections::HashMap;
use syn;
use synom;

pub struct LispFnArgs {
    pub name: String,
    pub min: i16,
}

pub fn parse(input: &str) -> Result<LispFnArgs, &'static str> {
    let kv = match parse_arguments(input) {
        synom::IResult::Done(_, o) => o,
        synom::IResult::Error => return Err("failed to parse `lisp_fn` arguments"),
    };

    Ok(parse_kv(kv))
}

fn parse_kv(kv_list: Vec<(syn::Ident, syn::StrLit)>) -> LispFnArgs {
    let mut arguments: HashMap<String, String> = HashMap::new();
    for (ident, string) in kv_list {
        let key = format!("{}", ident);
        let value = string.value;
        arguments.insert(key, value);
    }

    let mut lisp_fn_args = LispFnArgs {
        name: String::new(),
        min: 0,
    };

    for (key, value) in arguments {
        match &*key {
            "name" => lisp_fn_args.name = value.clone(),
            "min" => lisp_fn_args.min = i16::from_str(&*value).unwrap(),
            _ => (), // TODO: throw a warning?
        }
    }

    lisp_fn_args
}

named!(pub parse_arguments -> Vec<(syn::Ident, syn::StrLit)>,
    do_parse!(
        punct!("(")
    >>  args: separated_list!(punct!(","), key_value)
    >>  punct!(")")
    >>  (args)
    )
);


named!(key_value ->(syn::Ident, syn::StrLit),
  do_parse!(
     key: call!(syn::parse::ident)
  >>      option!(alt!(punct!(" ") | punct!("\t") | punct!("\n")))
  >>      punct!("=")
  >>      option!(alt!(punct!(" ") | punct!("\t") | punct!("\n")))
  >> val: call!(syn::parse::string)
  >>      (key, val)
  )
);
