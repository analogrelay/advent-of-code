use wasm_bindgen::prelude::*;

fn compute_calibration(document: &str) -> u32 {
    document.lines().map(|line| {
        // Find the first and last digit, which may be the same character
        let numbers: Vec<_> = line.chars().filter(|c| c.is_numeric()).collect();
        if numbers.len() < 1 {
            0
        } else {
            let first: u32 = numbers[0].to_digit(10).unwrap();
            let last: u32 = numbers[numbers.len() - 1].to_digit(10).unwrap();
            (first * 10) + last
        }
    }).sum::<u32>()
}

#[wasm_bindgen]
pub fn day01_part1(input: &str) -> String {
    compute_calibration(input).to_string()
}

const DIGIT_MAPPINGS: [&'static str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
];

#[wasm_bindgen]
pub fn day01_part2(input: &str) -> String {
    input.lines().map(|line| {
        let mut digits = Vec::new();
        let chars: Vec<_> = line.chars().collect();
        for i in 0..chars.len() {
            if let Some(d) = chars[i].to_digit(10) {
                // We found a numeric digit, add it to the list for this line
                digits.push(d);
                continue;
            } 

            let word_digit_pos = DIGIT_MAPPINGS.iter().position(|digit| {
                (i + 1).checked_sub(digit.len())
                    .map(|lookback| {
                        digit == &chars[lookback..=i].iter().collect::<String>()
                    }).unwrap_or(false)
            });

            if let Some(pos) = word_digit_pos {
                digits.push(pos as u32 + 1);
            }
        }
        
        if digits.len() < 1 {
            0
        } else {
            let first: u32 = digits[0];
            let last: u32 = digits[digits.len() - 1];
            (first * 10) + last
        }
    }).sum::<u32>().to_string()
}