use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    // get args from command line
    let args: Vec<String> = env::args().collect();
    let filename: &str = parse_args(&args);

    let contents = get_contents(filename);

    let result_first = solve_first(contents.clone());

    println!("{}", result_first);

    let result_second = solve_second(contents);

    println!("{}", result_second);
}

fn solve_first(input: String) -> u32 {
    let rucksacks = input.lines();
    rucksacks
        .map(|rucksack| to_priority(get_common_item(to_compartments(rucksack).into_iter())))
        .sum()
}

fn solve_second(input: String) -> u32 {
    let mut groups = Vec::new();

    let mut ctr: u8 = 0;
    let mut cur_group: Vec<&str> = Vec::new();
    for line in input.lines() {
        if ctr >= 3 {
            groups.push(cur_group);
            cur_group = Vec::new();
            ctr = 0;
        }
        cur_group.push(line);
        ctr += 1;
    }
    groups.push(cur_group);

    groups
        .into_iter()
        .map(|group| to_priority(get_common_item(group.into_iter())))
        .sum()
}

fn get_common_item<'a, I>(mut iter: I) -> char
where
    I: Iterator<Item = &'a str>,
{
    // take first item as starting HashSet, panic if iter empty
    let mut result_set = iter
        .next()
        .expect(r#"no elements in iterable"#)
        .chars()
        .collect::<HashSet<_>>();
    // loop through, taking intersections
    for item in iter {
        let new_chars: HashSet<char> = item.chars().collect();
        let intersection = result_set.intersection(&new_chars);
        result_set = intersection.cloned().collect();
    }
    // return final result, panicking if length is shorter than 1
    let result = *result_set.iter().next().expect(r#"empty intersection"#);
    result
}

fn to_priority(c: char) -> u32 {
    let ord: u32 = c.into();
    (ord - 38) % 58
}

fn to_compartments(rucksack: &str) -> [&str; 2] {
    let size = rucksack.len();
    let compartments = rucksack.split_at(size / 2);

    [compartments.0, compartments.1]
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
fn test_solve() {
    assert_eq!(solve_first(get_contents("test.txt")), 157);
    assert_eq!(solve_second(get_contents("test.txt")), 70);
}
