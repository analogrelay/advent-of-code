use std::collections::HashSet;

extern crate adventutils;

fn main() {
    let content = adventutils::read_input_file();

    let result = eval(&content);
    println!("Santa visits {} unique houses", result);
}

fn eval(content: &str) -> usize {
    let mut x = 0;
    let mut y = 0;
    let mut visited = HashSet::<(i64, i64)>::new();

    // Visit the first house
    visited.insert((x, y));
    println!("new visit: {},{}", x, y);
    for c in content.chars() {
        match c {
            '^' => y -= 1,
            '>' => x += 1,
            '<' => x -= 1,
            'v' => y += 1,
            _ => continue // Skip invalid input
        }
        if visited.insert((x, y)) {
            println!("new visit: {},{}", x, y);
        } else {
            println!("re-visit : {},{}", x, y);
        }
    }

    visited.len()
}
