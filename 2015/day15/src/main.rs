#![feature(slice_patterns,convert)]

extern crate adventutils;

struct Ingredient {
    name: String,
    capacity: isize,
    durability: isize,
    flavor: isize,
    texture: isize,
    calories: isize
}

fn main() {
    let input = adventutils::read_input_file();
    let cookies : Vec<_> = input.lines().map(&parse_line).collect();

    for cookie in cookies {
        println!("{} cap: {}, dur: {}, fla: {}, tex: {}, cal: {}", 
                 cookie.name,
                 cookie.capacity,
                 cookie.durability,
                 cookie.flavor,
                 cookie.texture,
                 cookie.calories);
    }
}

fn parse_line(line: &str) -> Ingredient {
    let tokens : Vec<_> = line.split_whitespace().collect();
    if let [ name, "capacity", capacity, "durability", durability, "flavor", flavor, "texture", texture, "calories", calories ] = tokens.as_slice() {
        Ingredient {
            name: name.to_string(),
            capacity: parse_int(capacity).unwrap(),
            durability: parse_int(durability).unwrap(),
            flavor: parse_int(flavor).unwrap(),
            texture: parse_int(texture).unwrap(),
            calories: parse_int(calories).unwrap()
        }
    }
    else {
        panic!("Unrecognized line!");
    }
}

fn parse_int(s: &str) -> Option<isize> {
    if s[0] == '-' {

}
