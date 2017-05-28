#[macro_use]
extern crate synom;
extern crate syn;

#[path = "../lisp_attr.rs"]
mod lisp_attr;

#[test]
fn parse_args_str() {
    let args = lisp_attr::parse_arguments(r#"(name = "foo", min = "0")"#).expect("cannot parse lisp_fn attributes");

    // name = "foo"
    assert_eq!(&*format!("{}", args[0].0), "name");
    assert_eq!(&*format!("{}", args[0].1.value), "foo");

    // min = "0"
    assert_eq!(&*format!("{}", args[1].0), "min");
    assert_eq!(&*format!("{}", args[1].1.value), "0");
}
