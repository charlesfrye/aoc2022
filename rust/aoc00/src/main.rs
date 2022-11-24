use std::env;
use std::fs;


fn main() {
  // get args from command line
  let args: Vec<String> = env::args().collect();
  let filename: &str = parse_args(&args);

  let contents = get_contents(filename);

  println!("{}", contents);
}

fn parse_args(args: &[String]) -> &str {
  let filename: &String = &args[1];

  filename
}

fn get_contents(filename: &str) -> String {
  // read file
  let contents: String = fs::read_to_string(filename)
        .expect(r#"Error reading file"#);
  contents
}

#[test]
fn test_main() {
  assert_eq!(get_contents("test.txt"), "hello, world!\n")
}
