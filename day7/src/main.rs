#![feature(slice_patterns,convert)]

use std::collections::HashMap;

extern crate adventutils;

#[derive(Debug,Clone)]
pub enum Literal {
    Constant(u16),
    Signal(String)
}

#[derive(Debug,Clone)]
pub enum Expression {
    Lit(Literal),
    Not(Literal),
    And(Literal, Literal),
    Or(Literal, Literal),
    Lshift(Literal, Literal),
    Rshift(Literal, Literal),
}

#[derive(Clone)]
pub struct Wire {
    pub name: String,
    pub source: Expression,
    pub value: Option<u16>
}

impl Wire {
    pub fn new(source: Expression, name: String) -> Wire {
        Wire {
            name: name,
            source: source,
            value: None
        }
    }
}

fn main() {
    let input = adventutils::read_input_file();

    let mut wires = parse(&input);

    // Sort into a hash map
    let mut wiremap : HashMap<String, Wire> = wires.iter().map(|w| (w.name.clone(), w.clone())).collect();

    // Evaluate the chain!
    let result = eval("a", &mut wiremap);
    println!("The result for part 1 is: {}", result);

    // Reset all the wires
    for (_, wire) in wiremap.iter_mut() {
        wire.value = None;
    }

    // Set 'b' to the result from the first part
    wiremap.get_mut("b").unwrap().value = Some(result);

    // Re-evaluate!
    let result = eval("a", &mut wiremap);
    println!("The result for part 2 is: {}", result);
}

fn eval(wire_name: &str, wires: &mut HashMap<String, Wire>) -> u16 {
    use Expression::*;

    let source = {
        let wire = wires.get(wire_name).unwrap();

        if let Some(val) = wire.value {
            // Cached value
            return val;
        }

        println!("evaluating: {} ({:?})", wire.name, wire.source);
        wire.source.clone()
    };

    let val = match source {
        Lit(ref l) => eval_lit(l, wires),
        Not(ref l) => !eval_lit(l, wires),
        And(ref l1, ref l2) => eval_lit(l1, wires) & eval_lit(l2, wires),
        Or(ref l1, ref l2) => eval_lit(l1, wires) | eval_lit(l2, wires),
        Lshift(ref l1, ref l2) => eval_lit(l1, wires) << eval_lit(l2, wires),
        Rshift(ref l1, ref l2) => eval_lit(l1, wires) >> eval_lit(l2, wires)
    };

    wires.get_mut(wire_name).unwrap().value = Some(val);
    val
}

fn eval_lit(lit: &Literal, wires: &mut HashMap<String, Wire>) -> u16 {
    use Literal::*;

    match lit {
        &Constant(i) => i,
        &Signal(ref s) => eval(s, wires)
    }
}

fn parse(input: &str) -> Vec<Wire> {
    input.lines().map(parse_line).collect()
}

fn parse_line(input: &str) -> Wire {
    use Expression::*;

    let segments : Vec<_> = input.split_whitespace().collect();
    match segments.as_slice() {
        [ x, "->", sig ] => Wire::new(Lit(parse_lit(x)),sig.to_string()),
        [ "NOT", x, "->", sig ] => Wire::new(Not(parse_lit(x)),sig.to_string()),
        [ x, "AND", y, "->", sig ] => Wire::new(And(parse_lit(x), parse_lit(y)),sig.to_string()),
        [ x, "OR", y, "->", sig ] => Wire::new(Or(parse_lit(x), parse_lit(y)),sig.to_string()),
        [ x, "LSHIFT", y, "->", sig ] => Wire::new(Lshift(parse_lit(x), parse_lit(y)),sig.to_string()),
        [ x, "RSHIFT", y, "->", sig ] => Wire::new(Rshift(parse_lit(x), parse_lit(y)),sig.to_string()),
        _ => panic!("unknown instruction: {}", input)
    }
}

fn parse_lit(input: &str) -> Literal {
    match usize::from_str_radix(input, 10) {
        Ok(i) => Literal::Constant(i as u16),
        _ => Literal::Signal(input.to_string())
    }
}
