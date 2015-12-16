#![feature(slice_patterns,convert)]

use std::collections::HashSet;

extern crate adventutils;
extern crate permutohedron;

#[derive(Clone)]
struct Entry {
    pub subject: String,
    pub other: String,
    pub happiness: isize
}

fn main() {
    let input = adventutils::read_input_file();
    let mut entries : Vec<_> = input.lines().map(&parse_line).collect();

    let result = calculate_best_happiness(entries.clone());
    println!("Part 1 Max happiness is: {}", result);

    // Add myself to the list
    let people : Vec<_> = get_distinct_people(&entries);
    for person in people {
        entries.push(Entry {
            subject: person.to_string(),
            other: "Andrew".to_string(),
            happiness: 0
        });
        entries.push(Entry {
            subject: "Andrew".to_string(),
            other: person.to_string(),
            happiness: 0
        });
    }
    let result = calculate_best_happiness(entries);
    println!("Part 2 Max happiness is: {}", result);
}

fn get_distinct_people(entries: &Vec<Entry>) -> Vec<String> {
    let people_hash : HashSet<_> = entries.iter().map(|e| e.subject.clone()).collect();
    people_hash.iter().cloned().collect()
}

fn calculate_best_happiness(entries: Vec<Entry>) -> isize {
    let mut people = get_distinct_people(&entries);

    // Permute all the people
    let permutations = permutohedron::Heap::new(&mut people);
    permutations
        .map(|p| calculate_happiness(&p, &entries))
        .max()
        .unwrap()
}

fn calculate_happiness(order: &Vec<String>, entries: &Vec<Entry>) -> isize {
    let mut total = 0;
    for (idx, person) in order.iter().enumerate() {
        let neighbor = if idx < order.len() - 1 {
            &order[idx + 1]
        } else {
            &order[0]
        };

        total += entries.iter().find(|e| e.subject == *person && e.other == *neighbor).unwrap().happiness;
        total += entries.iter().find(|e| e.other == *person && e.subject == *neighbor).unwrap().happiness;
    }
    total
}

fn parse_line(line: &str) -> Entry {
    let tokens : Vec<_> = line.split_whitespace().collect();

    match tokens.as_slice() {
        [ subject, "would", "gain", happiness, "happiness", "units", "by", "sitting", "next", "to", other ] => Entry {
            subject: subject.into(),
            other: other.trim_right_matches('.').into(),
            happiness: isize::from_str_radix(happiness, 10).unwrap()
        },
        [ subject, "would", "lose", happiness, "happiness", "units", "by", "sitting", "next", "to", other ] => Entry {
            subject: subject.into(),
            other: other.trim_right_matches('.').into(),
            happiness: -(isize::from_str_radix(happiness, 10).unwrap())
        },
        _ => panic!("Unexpected line: {}", line)
    }
}
