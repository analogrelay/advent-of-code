use std::str::FromStr;

extern crate adventutils;

struct Present {
    length: u64,
    width: u64,
    height: u64
}

fn main() {
    // Load the input file
    let content = adventutils::read_input_file();

    // Load the input data
    let presents = eval(&content);

    // Fold to create result
    let result = presents.iter().fold(0, process_present);
    println!("The elves need {} sqft of wrapping paper!", result);
}

fn process_present(acc: u64, pres: &Present) -> u64 {
    let sides = [
        (pres.length * pres.width),
        (pres.width * pres.height),
        (pres.height * pres.length)];
    let extra = sides.iter().min().unwrap();

    acc + sides.iter().fold(0, |acc, side| acc + (2 * side)) + extra
}

fn eval(input: &str) -> Vec<Present> {
    input.lines().map(|line| {
        let mut splat = line.split('x');
        Present {
            length: FromStr::from_str(splat.next().unwrap()).unwrap(),
            width: FromStr::from_str(splat.next().unwrap()).unwrap(),
            height: FromStr::from_str(splat.next().unwrap()).unwrap()
        }
    }).collect()
}
