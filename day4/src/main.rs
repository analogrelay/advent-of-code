extern crate crypto;

use std::{process,env};
use std::iter::Iterator;
use crypto::md5::Md5;
use crypto::digest::Digest;

fn main() {
    // Read args
    let input = match env::args().nth(1) {
        Some(f) => f,
        None => {
            println!("missing input data");
            process::exit(1);
        }
    };

    let mut md5 = Md5::new();

    // Work to find the input
    let mut counter = 1;
    let mut output = [0u8; 16];
    loop {
        let val = format!("{}{}", input, counter);

        md5.input_str(&val);
        let bytestr = md5.result_str();
        md5.reset();

        if bytestr.starts_with("000000") {
            println!("{}", bytestr);
            println!("Key: {}", counter);
            return;
        }
        if counter % 1000 == 0 {
            println!("Counter = {}, Hash = {}", counter, bytestr);
        }
        counter += 1;
    }
}
