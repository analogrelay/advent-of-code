#![feature(iter_arith)]

use rustc_serialize::json::Json;

extern crate adventutils;
extern crate rustc_serialize;

fn main() {
    let input = adventutils::read_input_file();

    // Load the document
    let json = Json::from_str(&input).unwrap();

    println!("Sum of all numbers: {}", count_numbers(&json, true));
    println!("Sum of all numbers (without red): {}", count_numbers(&json, false));
}

fn count_numbers(json: &Json, count_red: bool) -> i64 {
    match json {
        &Json::I64(i) => i,
        &Json::U64(i) => i as i64,
        &Json::F64(f) => f as i64,
        &Json::Array(ref a) => a.iter().map(|v| count_numbers(v, count_red)).sum(),
        &Json::Object(ref o) => {
            let mut count = 0;
            for v in o.values() {
                count += count_numbers(v, count_red);
                if let &Json::String(ref s) = v {
                    if !count_red && s == "red" {
                        return 0;
                    }
                }
            }
            count
        },
        _ => 0
    }
}
