use std::{env,fs,process};
use std::io::Read;

pub fn read_input_file() -> String
{
    // Read args
    let file_name = match env::args().nth(1) {
        Some(f) => f,
        None => {
            println!("missing input file");
            process::exit(1);
        }
    };
    read_all_text(&file_name)
}

pub fn read_all_text(path: &str) -> String
{
    // Load the input file
    let mut file = fs::File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    content
}
