#![feature(slice_patterns,convert)]

use std::collections::HashSet;

extern crate adventutils;

#[derive(Clone)]
struct Entry {
    pub subject: String,
    pub other: String,
    pub happiness: isize
}

struct HappinessList(Vec<Entry>, HashSet<String>);

impl HappinessList {
    pub fn new(entries: Vec<Entry>) -> HappinessList {
        let people = entries.iter().map(|e| &e.subject).collect();
        HappinessList(entries, people)
    }

    pub fn entries(&self) -> &Vec<Entry> {
        self.0
    }

    pub fn people(&self) -> &HashSet<String> {
        self.1
    }
}

struct Table(Vec<String>);

impl Table {
    pub fn new() -> Table {
        Table(Vec::new())
    }

    pub fn seat(&mut self, entry: &str) {
        self.0.push(entry.to_string());
    }

    pub fn neighbors(&mut self, person: &str) -> (Option<&str>, Option<&str>) {
        let pos = match self.0.iter().position(|p| p == person) {
            Some(p) => p,
            None => return (None, None)
        };

        (self.get_neighbor(pos, -1), self.get_neighbor(pos, 1))
    }

    fn get_neighbor(&self, current: usize, offset: isize) -> Option<&str> {
        let pos = current as isize + offset;
        let idx = if pos < 0 {
            self.0.len() as isize + pos
        } else {
            pos
        } as usize;

        if idx == current {
            None
        } else {
            Some(&self.0[idx])
        }
    }
}

fn main() {
    let input = adventutils::read_input_file();
    let happiness = HappinessList::new(input.lines().map(&parse_line).collect());
    let mut people = happiness.people().clone();
    let mut table = Table::new();
    let mut current : Option<&str> = None;

    while !people.is_empty() {
        // Find the best starting entry and place it
        let best_entry = entries.iter()
            .filter(|e| people.contains(&e.other) && (current.is_none() || current.unwrap() == e.subject))
            .max_by(|e| e.happiness)
            .unwrap();
        table.seat(&best_entry.other);

        // Set up for the next iteration
        current = Some(&best_entry.other);
        people.remove(&best_entry.other);
    }
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
