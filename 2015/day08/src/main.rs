#![feature(iter_arith)]

extern crate adventutils;

fn main() {
    let input = adventutils::read_input_file();

    let result : usize = input.lines()
        .map(|s| s.chars().count() - parse_line(s).chars().count())
        .sum();

    println!("The part 1 result is: {}", result);

    let result : usize = input.lines()
        .map(|s| encode_line(s).chars().count() - s.chars().count())
        .sum();

    println!("The part 2 result is: {}", result);
}

pub fn encode_line(s: &str) -> String {
    let encoded = format!("\"{}\"", s.replace("\\", "\\\\").replace("\"", "\\\""));

    println!("encoded: {} => {} ({})", s, encoded, encoded.chars().count() - s.chars().count());

    encoded
}

pub struct ParseState {
    pub escape: Vec<char>,
    pub hex_escape_value: u32
}

impl ParseState {
    pub fn new() -> ParseState {
        ParseState {
            escape: Vec::new(),
            hex_escape_value: 0
        }
    }
}

fn parse_line(line: &str) -> String {
    let parsed = line.chars().skip(1).scan(ParseState::new(), |state, chr| {
        match (chr, state.escape.len(), chr.to_digit(16)) {
            ('\\', 0, _) => {
                // Enter the escape sequence
                state.escape.push(chr);

                // Emit nothing
                Some(None)
            },
            ('x', 1, _) => {
                // Continue escaping (hex escape)
                state.escape.push(chr);

                // Emit nothing
                Some(None)
            },
            ('\\', 1, _) => {
                // End the escape
                state.escape.clear();

                // Emit the character
                Some(Some('\\'))
            },
            ('\"', 1, _) => {
                // End the escape
                state.escape.clear();

                // Emit the character
                Some(Some('\"'))
            },
            (_, 2, Some(x)) => {
                // Continued hex escape (first digit)
                state.escape.push(chr);

                // Collect the escaped value
                state.hex_escape_value = x << 4;

                // Emit nothing
                Some(None)
            },
            (_, 3, Some(x)) => {
                // End the escape
                state.escape.clear();

                // Collect the value
                state.hex_escape_value |= x;

                // Emit the character
                Some(Some(std::char::from_u32(state.hex_escape_value).unwrap()))
            },
            ('\"', 0, _) => {
                None // Stop parsing
            },
            (c, _, _) => {
                Some(Some(c))
            }
        }
    }).filter_map(|x| x).collect();

    parsed
}
