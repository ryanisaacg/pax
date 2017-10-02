extern crate esparse;

use std::{env, io, fs};
use std::io::prelude::*;
use esparse::push::{self, lazy, Deferred};

fn main() {
    let mut contents = String::new();

    let arg = env::args().nth(1);
    let file_name = match arg.as_ref().map(|s| &**s) {
        None | Some("-") => {
            let stdin = io::stdin();
            let mut stdin = stdin.lock();
            stdin.read_to_string(&mut contents).unwrap();
            "<stdin>"
        }
        Some(file_name) => {
            let file = fs::File::open(&file_name).unwrap();
            let mut buf_reader = io::BufReader::new(file);
            buf_reader.read_to_string(&mut contents).unwrap();
            file_name
        },
    };

    let mut parser = push::Lazy::new(file_name, &contents);
    let result = parser.parse_expr_primary(Default::default()).unwrap();
    println!("{:#?}", result);

    match result.kind {
        lazy::ExprKind::Array(values) => {
            println!("{:#?}", values.parse());
        }
        lazy::ExprKind::Template(head, parts) => {
            println!("{:#?}, {:#?}", head, parts.parse());
        }
        lazy::ExprKind::Object(defs) => {
            println!("{:#?}", defs.parse());
        }
        _ => {}
    }

    // let mut parser = push::Strict::new(file_name, &contents);
    // let result = parser.parse_expr_primary(Default::default()).unwrap();
    // println!("{:#?}", result);
}
