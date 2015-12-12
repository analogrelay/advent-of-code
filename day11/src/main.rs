use std::env;

// Rust doesn't make it easy to do things like: 'a' + x to get the letter matching a digit.
static DIGITS: [char; 26] = [
    'a', 'b', 'c', 'd',
    'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l',
    'm', 'n', 'o', 'p',
    'q', 'r', 's', 't',
    'u', 'v', 'w', 'x',
    'y', 'z'];

fn main() {
    let input = match env::args().nth(1) {
        Some(i) => i,
        None => {
            println!("usage: day11 <input value>");
            return
        }
    };

    let next = find_next_password(&input);
    println!("found next valid password: {}", next);
    let next_next = find_next_password(&input);
    println!("found next next valid password: {}", next_next);
}

fn find_next_password(input: &str) -> String {
    let mut candidate = increment_string(&input);
    while !is_acceptable_password(&candidate) {
        candidate = increment_string(&candidate);
    }
    candidate
}

fn is_acceptable_password(candidate: &str) -> bool {
    let chars : Vec<_> = candidate.chars().collect();
    !candidate.contains('i') &&
        !candidate.contains('o') &&
        !candidate.contains('l') &&
        has_straight_and_pairs(&chars)
}

fn has_straight_and_pairs(candidate: &Vec<char>) -> bool {
    let mut idx = 0;
    let mut pairs = 0;
    let mut straight = false;
    while idx < candidate.len() {
        straight |= is_straight(candidate, idx);
        if idx > 0 && candidate[idx] == candidate[idx - 1] {
            pairs += 1;
            idx += 1;
        }
        idx += 1;
    }
    straight && pairs > 1
}

fn is_straight(candidate: &Vec<char>, idx: usize) -> bool {
    idx > 1 && {
        let c = candidate[idx];
        candidate[idx - 1] == std::char::from_u32(c as u32 - 1).unwrap() &&
            candidate[idx - 2] == std::char::from_u32(c as u32 - 2).unwrap()
    }
}

fn increment_string(input: &str) -> String {
    fn increment_char(c: char) -> char {
        std::char::from_u32((c as u32) + 1).unwrap()
    }

    let mut carry = true;

    let out : Vec<_> = input.chars().rev().scan(&mut carry, |carry, item| {
        use std::mem::replace;

        match (**carry, item) {
            (true, 'z') => Some('a'),
            (true, c) => { **carry = false; Some(increment_char(c)) },
            (false, c) => Some(c)
        }
    }).collect();

    let mut result : String = out.iter().rev().cloned().collect();

    if carry {
        result.insert(0, 'a');
    }
    result
}

//fn to_base26_string(mut input: usize) -> String {
    //// This isn't pure base-26. For example, the count goes from 'z' to 'aa' rather than 'ba'
    //// (because 'a' should represent 0). However, it fits the needs of the puzzle.
    //let mut result = String::new();
    //while input > 25 {
        //let digit = input / 26;
        //let new_input = input % 26;

        //println!("cycle: {} => {},{}", input, digit, new_input);
        //result.push(DIGITS[digit]);
        //input = new_input
    //}
    //result.push(DIGITS[input + 1]);
    //result
//}

//fn from_base26_string(input: &str) -> usize {
    //let chars : Vec<_> = input.chars().collect();
    //let mut value = 0;
    //for (idx, c) in chars.iter().enumerate() {
        //let mut digit = (*c as u32 - 'a' as u32) as usize;
        //if idx < (chars.len() - 1) {
            //digit += 1
        //}
        //value = (value * 26) + digit;
    //}
    //value
//}
