use std::env;

fn main() {
    let input = match env::args().nth(1) {
        Some(i) => i,
        None => {
            println!("usage: day10 <input value>");
            return
        }
    };

    let mut current = input.clone();
    for _ in 0..40 {
        current = encode(&current);
    }
    println!("The result is {} characters long when run 40 times", current.chars().count());

    for _ in 0..10 {
        current = encode(&current);
    }
    println!("The result is {} characters long when run 50 times", current.chars().count());
}

fn encode(input: &str) -> String {
    let mut encoded = String::new();
    let mut current_digit = None;
    let mut current_counter = 1;

    for chr in input.chars() {
        match current_digit {
            None => current_digit = Some(chr),
            Some(prev) if prev == chr => current_counter += 1,
            Some(prev) => {
                encoded.push_str(&format!("{}", current_counter));
                encoded.push(prev);
                current_counter = 1;
                current_digit = Some(chr);
            }
        }
    }

    if let Some(prev) = current_digit {
        encoded.push_str(&format!("{}", current_counter));
        encoded.push(prev);
    }
    encoded
}
