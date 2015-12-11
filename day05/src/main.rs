extern crate adventutils;

fn main() {
    let content = adventutils::read_input_file();

    println!("There are {} nice strings by part 1",
             content.lines().filter(&is_nice).count());
    println!("There are {} nice strings by part 2",
             content.lines().filter(&is_nice_2).count());
}

static VOWELS : [char; 5] = [ 'a', 'e', 'i', 'o', 'u' ];
static EXCLUDED_STRINGS : [&'static str; 4] = [ "ab", "cd", "pq", "xy" ];
fn is_nice(input: &&str) -> bool {
    let vowels = input.chars().filter(|v| VOWELS.iter().any(|c| c == v)).count();
    let (_, doubled_up_letter) = input.chars().fold(('\0', false), |(prev, success), item| (item, success || prev == item));
    let contains_excluded_string = EXCLUDED_STRINGS.iter().any(|s| input.contains(s));

    let result = !contains_excluded_string && vowels >= 3 && doubled_up_letter;

    result
}

fn is_nice_2(input: &&str) -> bool {
    let chars = input.chars().collect();
    is_pairs_nice(&chars) && is_repeat_nice(&chars)
}

fn is_pairs_nice(input: &Vec<char>) -> bool {
    for x in 0..(input.len() - 3) {
        for y in (x+2)..(input.len() - 1) {
            if input[x..(x+2)] == input[y..(y+2)] {
                return true
            }
        }
    }

    false
}

fn is_repeat_nice(input: &Vec<char>) -> bool {
    for i in 0..(input.len() - 2) {
        if input[i] == input[i + 2] {
            return true;
        }
    }

    false
}
