fn main() {
    println!("Hello, world!");
}

enum Lambda {
    Num(i64),
    Empty,
}

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

#[cfg(test)]
mod tests {

    use crate::Lambda;

    fn format_lambda(lambda: Lambda) -> String {
        match lambda {
            Lambda::Empty => "".to_string(),
            Lambda::Num(x) => x.to_string(),
        }
    }

    #[test]
    fn format_an_empty_lamnbda() {
        assert!(format_lambda(Lambda::Empty) == "");
    }

    quickcheck! {
        fn format_a_number(x: i64) -> bool {
            format_lambda(Lambda::Num(x)) == x.to_string()
        }
    }
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
