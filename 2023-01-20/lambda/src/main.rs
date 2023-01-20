fn main() {
    println!("Hello, world!");
}

enum Lambda {
    Num(i64),
    Var(String),
    Ap(Box<Lambda>, Box<Lambda>),
    Lam(String, Box<Lambda>),
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
            Lambda::Var(x) => x,
            Lambda::Num(x) => x.to_string(),
            Lambda::Ap(l1,l2) => format!("({} {})", format_lambda(*l1), format_lambda(*l2)),
            Lambda::Lam(arg,body) => format!("(lambda ({}) {})", arg, format_lambda(*body))
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

        fn format_a_var(x: String) -> bool {
            format_lambda(Lambda::Var(x.clone())) == x
        }
    }

    #[test]
    fn format_an_application() {
        assert!(format_lambda(Lambda::Ap(Box::new(Lambda::Var("x".to_string())), 
                                         Box::new(Lambda::Var("y".to_string())))) == "(x y)");
    }

    #[test]
    fn format_a_lambda() {
        assert!(format_lambda(Lambda::Lam("x".to_string(), 
                                          Box::new(Lambda::Var("y".to_string())))) == "(lambda (x) y)");
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
