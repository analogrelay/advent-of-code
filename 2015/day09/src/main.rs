#![feature(slice_patterns,convert,iter_cmp,box_patterns)]

use std::fmt;
use std::collections::HashSet;
use Route::*;

extern crate adventutils;

#[derive(Clone)]
pub enum Route {
    NextLeg(Box<Route>, Leg),
    Start
}

impl Route {
    pub fn distance(&self) -> usize {
        match self {
            &NextLeg(ref prev, ref leg) => prev.distance() + leg.distance,
            &Start => 0
        }
    }

    pub fn already_visited(&self, location: &str) -> bool {
        match self {
            &NextLeg(ref prev, ref leg) => leg.start == location || leg.end == location || prev.already_visited(location),
            &Start => false
        }
    }
}

impl fmt::Display for Route {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &NextLeg(box Start, ref leg) => write!(fmt, "{} -> {}", leg.start, leg.end),
            &NextLeg(ref prev, ref leg) => write!(fmt, "{} -> {}", prev, leg.end),
            &Start => Ok(())
        }
    }
}

#[derive(Clone)]
pub struct Leg {
    start: String,
    end: String,
    distance: usize
}

impl Leg {
    pub fn new(start: String, end: String, distance: usize) -> Leg {
        Leg {
            start: start,
            end: end,
            distance: distance
        }
    }
}

fn main() {
    let input = adventutils::read_input_file();
    let legs : Vec<_> = input.lines().flat_map(&parse_line).collect();

    // Collect locations
    let start_points : HashSet<_> = legs.iter().map(|l| l.start.clone()).collect();
    let end_points : HashSet<_> = legs.iter().map(|l| l.end.clone()).collect();
    let all_locations : HashSet<_> = start_points.union(&end_points).collect();

    let shortest_route = start_points.iter()
        .map(|s| find_shortest_route(s, &legs, &all_locations, Route::Start))
        .filter_map(|r| r)
        .min_by(|r| r.distance());

    match shortest_route {
        Some(r) => println!("Found shortest route: {} ({})", r, r.distance()),
        None => println!("Failed to find a shortest route!")
    }

    let longest_route = start_points.iter()
        .map(|s| find_longest_route(s, &legs, &all_locations, Route::Start))
        .filter_map(|r| r)
        .max_by(|r| r.distance());

    match longest_route {
        Some(r) => println!("Found longest route: {} ({})", r, r.distance()),
        None => println!("Failed to find a route!")
    }
}

// Find the shortest possible route from the specified start to all remaining locations
fn find_shortest_route(start: &str, legs: &Vec<Leg>, locations: &HashSet<&String>, current_route: Route) -> Option<Route> {
    // Have we already visited everything?
    if locations.iter().all(|l| current_route.already_visited(l)) {
        Some(current_route)
    } else {
        legs.iter()
            .filter(|l| l.start == start && !current_route.already_visited(&l.end))
            .map(|l| find_shortest_route(&l.end, legs, locations, Route::NextLeg(Box::new(current_route.clone()), l.clone())))
            .filter_map(|r| r)
            .min_by(|r| r.distance())
    }
}

// Find the shortest possible route from the specified start to all remaining locations
fn find_longest_route(start: &str, legs: &Vec<Leg>, locations: &HashSet<&String>, current_route: Route) -> Option<Route> {
    // Have we already visited everything?
    if locations.iter().all(|l| current_route.already_visited(l)) {
        Some(current_route)
    } else {
        legs.iter()
            .filter(|l| l.start == start && !current_route.already_visited(&l.end))
            .map(|l| find_longest_route(&l.end, legs, locations, Route::NextLeg(Box::new(current_route.clone()), l.clone())))
            .filter_map(|r| r)
            .max_by(|r| r.distance())
    }
}

fn parse_line(input: &str) -> Vec<Leg> {
    let tokens : Vec<_> = input.split_whitespace().collect();
    if let [ start, "to", end, "=", distance ] = tokens.as_slice() {
        vec![
            Leg::new(start.to_string(), end.to_string(), usize::from_str_radix(distance, 10).unwrap()),
            Leg::new(end.to_string(), start.to_string(), usize::from_str_radix(distance, 10).unwrap())
        ]
    } else {
        panic!("Unrecognized string: {}", input);
    }
}
