use std::collections::HashSet;

extern crate adventutils;

fn main() {
    let content = adventutils::read_input_file();

    let result = eval(&content);
    println!("Santa visits {} unique houses", result);
}

fn eval(content: &str) -> usize {
    let (mut s_x, mut s_y) = (0, 0);
    let (mut r_x, mut r_y) = (0, 0);
    let mut visited = HashSet::<(i64, i64)>::new();

    // Visit the first house
    visited.insert((s_x, s_y));
    for (idx, c) in content.chars().enumerate() {
        let (dx, dy) = match c {
            '^' => (0, -1),
            '>' => (1, 0),
            '<' => (-1, 0),
            'v' => (0, 1),
            _ => continue
        };

        if idx % 2 == 0 {
            // Santa
            s_x += dx;
            s_y += dy;
            visited.insert((s_x, s_y));
        } else {
            // Robo-santa
            r_x += dx;
            r_y += dy;
            visited.insert((r_x, r_y));
        }
    }

    visited.len()
}
