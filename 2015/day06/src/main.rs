#![feature(slice_patterns,convert,iter_arith)]

use std::ops;

extern crate adventutils;

pub struct Lights([usize; 1000*1000]);

impl Lights {
    pub fn new() -> Lights {
        Lights([0; 1000*1000])
    }

    pub fn as_slice(&self) -> &[usize] {
        &self.0
    }
}

impl ops::Index<(usize, usize)> for Lights {
    type Output = usize;

    fn index<'a>(&'a self, (x, y): (usize, usize)) -> &'a usize {
        &self.0[(x * 1000) + y]
    }
}

impl ops::IndexMut<(usize, usize)> for Lights {
    fn index_mut<'a>(&'a mut self, (x, y): (usize, usize)) -> &'a mut usize {
        &mut self.0[(x * 1000) + y]
    }
}

fn main() {
    let input = adventutils::read_input_file();
    part1(&input);
    part2(&input);
}

fn part1(input: &str) {
    let mut lights = Lights::new();

    for line in input.lines() {
        eval(&mut lights, line, &part1::turn_on, &part1::turn_off, &part1::toggle);
    }

    println!("There are {} lights lit by part 1", lights.as_slice().iter().filter(|f| (**f) > 0).count());
}

fn part2(input: &str) {
    let mut lights = Lights::new();

    for line in input.lines() {
        eval(&mut lights, line, &part2::turn_on, &part2::turn_off, &part2::toggle);
    }

    println!("The total brightness is: {}", lights.as_slice().iter().sum::<usize>());
}

fn parse_coord(coord: &str) -> (usize, usize) {
    let items : Vec<&str> = coord.split(',').collect();
    assert_eq!(2, items.len());

    (
        usize::from_str_radix(items[0], 10).unwrap(),
        usize::from_str_radix(items[1], 10).unwrap()
    )
}

fn each_coord<F>((x1,y1): (usize, usize), (x2,y2): (usize, usize), mut act: F) where F: FnMut((usize, usize)) {
    for x in x1..(x2 + 1) {
        for y in y1..(y2 + 1) {
            act((x,y))
        }
    }
}

fn eval<F1, F2, F3>(lights: &mut Lights, line: &str, on: F1, off: F2, toggle: F3)
        where F1: Fn(&mut Lights, (usize, usize), (usize, usize)),
              F2: Fn(&mut Lights, (usize, usize), (usize, usize)),
              F3: Fn(&mut Lights, (usize, usize), (usize, usize)) {
    let words : Vec<&str> = line.split_whitespace().collect();
    match words.as_slice() {
        ["turn", "on", start, "through", end] => on(lights, parse_coord(start), parse_coord(end)),
        ["turn", "off", start, "through", end] => off(lights, parse_coord(start), parse_coord(end)),
        ["toggle", start, "through", end] => toggle(lights, parse_coord(start), parse_coord(end)),
        _ => panic!("Unknown instruction: {}", line)
    }
}

mod part1 {
    use super::{Lights,each_coord};

    pub fn turn_on(lights: &mut Lights, start: (usize, usize), end: (usize, usize)) {
        each_coord(start, end, |c| {
            lights[c] = 1;
        });
    }

    pub fn turn_off(lights: &mut Lights, start: (usize, usize), end: (usize, usize)) {
        each_coord(start, end, |c| {
            lights[c] = 0;
        });
    }

    pub fn toggle(lights: &mut Lights, start: (usize, usize), end: (usize, usize)) {
        each_coord(start, end, |c| {
            lights[c] = match lights[c] {
                0 => 1,
                1 => 0,
                _ => panic!("Invalid light value!")
            };
        });
    }
}

mod part2 {
    use super::{Lights,each_coord};

    pub fn turn_on(lights: &mut Lights, start: (usize, usize), end: (usize, usize)) {
        each_coord(start, end, |c| {
            lights[c] += 1;
        });
    }

    pub fn turn_off(lights: &mut Lights, start: (usize, usize), end: (usize, usize)) {
        each_coord(start, end, |c| {
            if lights[c] > 0 {
                lights[c] -= 1;
            }
        });
    }

    pub fn toggle(lights: &mut Lights, start: (usize, usize), end: (usize, usize)) {
        each_coord(start, end, |c| {
            lights[c] += 2;
        });
    }
}
