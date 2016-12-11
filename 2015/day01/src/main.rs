extern crate adventutils;

fn main() {
    // Load the input file
    let content = adventutils::read_input_file();

    // Run the script
    let result = eval(&content);
    println!("Santa should go to floor: {}", result);
}

fn eval(input: &str) -> i64 {
    input.chars().enumerate().fold((false, 0), |(entered_basement, acc), (idx, item)| {
        let new_val = match item {
            '(' => acc + 1,
            ')' => acc - 1,
            _ => acc // Skip invalid input
        };
        if new_val < 0 && !entered_basement {
            println!("entered basement at index: {}", idx);
            (true, new_val)
        } else {
            (entered_basement, new_val)
        }
    }).1
}
