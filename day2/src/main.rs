use std::{env,fs,process};
use std::io::{self,BufRead,Read};
use std::str::FromStr;

struct Present {
    length: u64,
    width: u64,
    height: u64
}

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
    let mut reader = io::BufReader::new(file);

    // Load the input data
    let presents = eval(&mut reader);

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

fn eval<T>(input: &mut T) -> Vec<Present> where T: io::BufRead {
    input.lines().map(|r| {
        let line = r.unwrap();
        let mut splat = line.split('x');
        Present {
            length: FromStr::from_str(splat.next().unwrap()).unwrap(),
            width: FromStr::from_str(splat.next().unwrap()).unwrap(),
            height: FromStr::from_str(splat.next().unwrap()).unwrap()
        }
    }).collect()
}
