#![feature(slice_patterns,convert)]

extern crate adventutils;

#[derive(Clone,Copy)]
enum RaceState {
    Start,
    Flying(u64),
    Resting(u64)
}

struct Reindeer {
    name: String,
    speed: u64,
    stamina: u64,
    recovery: u64,
    state: RaceState,
    distance_travelled: u64,
    points: u64
}

impl Reindeer {
    pub fn tick(&mut self) {
        let new_state = match self.state {
            RaceState::Start => self.fly(),
            RaceState::Flying(_) => self.fly(),
            RaceState::Resting(_) => self.rest()
        };
        self.state = new_state;
    }

    fn rest(&mut self) -> RaceState {
        match self.state {
            RaceState::Resting(1) => RaceState::Flying(self.stamina),
            RaceState::Resting(x) => RaceState::Resting(x - 1),
            x => x
        }
    }

    fn fly(&mut self) -> RaceState {
        self.distance_travelled += self.speed;

        match self.state {
            RaceState::Start if self.stamina > 1 => RaceState::Flying(self.stamina - 1), // We already spent 1 stamina!
            RaceState::Start => RaceState::Resting(self.recovery),
            RaceState::Flying(1) => RaceState::Resting(self.recovery),
            RaceState::Flying(x) => RaceState::Flying(x - 1),
            x => x
        }
    }
}

fn main() {
    let input = adventutils::read_input_file();
    let mut reindeer : Vec<_> = input.lines().map(&parse_line).collect();

    // Simulate the race!
    for _ in 0..2503 {
        tick(&mut reindeer);
    }

    let winner_1 = reindeer.iter().max_by_key(|r| r.distance_travelled).unwrap();
    println!("{} won the part 1 race at a distance of {} km", winner_1.name, winner_1.distance_travelled);
    let winner_2 = reindeer.iter().max_by_key(|r| r.points).unwrap();
    println!("{} won the part 2 race with a points total of {}", winner_2.name, winner_2.points);
}

fn tick(reindeer: &mut Vec<Reindeer>) {
    for deer in reindeer.iter_mut() {
        deer.tick();
    }

    // Score the reindeer
    let winning_distance = reindeer.iter().map(|r| r.distance_travelled).max().unwrap();
    for deer in reindeer.iter_mut().filter(|r| r.distance_travelled == winning_distance) {
        deer.points += 1;
    }
}

fn parse_line(line: &str) -> Reindeer {
    let tokens : Vec<_> = line.split_whitespace().collect();
    if let [ name, "can", "fly", speed, "km/s", "for", stamina, "seconds,", "but", "then", "must", "rest", "for", recovery, "seconds." ] = tokens.as_slice() {
        Reindeer {
            name: name.to_string(),
            speed: u64::from_str_radix(speed, 10).unwrap(),
            stamina: u64::from_str_radix(stamina, 10).unwrap(),
            recovery: u64::from_str_radix(recovery, 10).unwrap(),
            state: RaceState::Start,
            distance_travelled: 0,
            points: 0
        }
    }
    else {
        panic!("Unexpected line!");
    }
}
