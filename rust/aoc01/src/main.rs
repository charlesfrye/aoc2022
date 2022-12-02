use std::env;
use std::fs;

fn main() {
    // get args from command line
    let args: Vec<String> = env::args().collect();
    let filename: &str = parse_args(&args);

    let contents = get_contents(filename);

    let elf_sacks = parse_contents(&contents);

    let mut sack_totals: Vec<u32> = Vec::new();
    for sack in elf_sacks {
        let sack_int: Vec<u32> = sack
            .split('\n')
            .map(|string| string.parse::<u32>().unwrap())
            .collect();
        let sack_total: u32 = sack_int.iter().sum();
        sack_totals.push(sack_total);
    }

    sack_totals.sort_unstable(); // order of unequals not important
    let mut grand_total: u32 = 0;
    for _ in 1..=3 {
        let value = sack_totals.pop().unwrap();
        println!("\t{:}", value);
        grand_total += value;
    }
    println!("{:}", grand_total);
}

fn parse_contents(contents: &str) -> Vec<&str> {
    let elf_sacks = contents.split("\n\n");
    elf_sacks.collect::<Vec<&str>>()
}

fn parse_args(args: &[String]) -> &str {
    let filename: &String = &args[1];

    filename
}

fn get_contents(filename: &str) -> String {
    // read file
    let contents: String = fs::read_to_string(filename).expect(r#"Error reading file"#);
    contents
}

#[test]
fn test_parse() {
    assert_eq!(
        parse_contents(&get_contents("test.txt"))[0],
        "1000\n2000\n3000"
    )
}
