fn main() {
    println!("Hello, world!");
}

struct Lambda {}

fn format_lambda(lambda : Lambda) -> String {
  "".to_string()
}

#[test]
fn format_an_empty_lamnbda() {
    assert!(format_lambda(Lambda{}) == "");
}

// Acceptance 
// fn lambda(input: &str) -> String {
// //  target/goal is to write a REPL ->
//     let expr = parse(input);
//     let result = eval(expr);
//     format_lambda(result)
// }
// 
// #[test]
// fn if_then_else() {
//     assert!(lambda("((lambda (a b) a) 1 2)") == "1");
// }
