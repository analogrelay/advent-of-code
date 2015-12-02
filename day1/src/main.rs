use std::{env,fs,process};
use std::io::Read;

fn main() {
    // Read args
    let file_name = match env::args().nth(1) {
        Some(f) => f,
        None => {
            println!("Usage: day1 [input file]");
            process::exit(1);
        }
    };

    // Load the input file
    let mut file = fs::File::open(file_name).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();

    // Run the script
    let result = eval(&content);
    println!("Santa should go to floor: {}", result);
}

fn eval(input: &str) -> i64 {
    input.chars().fold(0, |acc, item| match item {
        '(' => acc + 1,
        ')' => acc - 1,
        _ => acc // Skip invalid input
    })
}
