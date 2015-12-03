extern crate adventutils;

fn main() {
    // Load the input file
    let content = adventutils::read_input_file();

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
