use std::{
    collections::HashMap,
    env, fs,
    io::{prelude::*, BufReader},
};

use lazy_static::lazy_static;

#[derive(PartialEq, Eq, Debug, Hash)]
enum RPS {
    Rock,
    Paper,
    Scissors,
}
impl RPS {
    fn beats(&self, other: &RPS) -> bool {
        matches!(
            (self, other),
            (&RPS::Rock, &RPS::Scissors)
                | (&RPS::Paper, &RPS::Rock)
                | (&RPS::Scissors, &RPS::Paper)
        )
    }
    fn ties(&self, other: &RPS) -> bool {
        self == other
    }
    fn loses_to(&self) -> RPS {
        match self {
            RPS::Rock => RPS::Paper,
            RPS::Paper => RPS::Scissors,
            RPS::Scissors => RPS::Rock,
        }
    }
    fn wins_against(&self) -> RPS {
        match self {
            RPS::Rock => RPS::Scissors,
            RPS::Paper => RPS::Rock,
            RPS::Scissors => RPS::Paper,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash)]
enum Outcome {
    Win,
    Lose,
    Draw,
}

lazy_static! {
    static ref MY_MAP: HashMap<char, RPS> =
        HashMap::from([('X', RPS::Rock), ('Y', RPS::Paper), ('Z', RPS::Scissors)]);
}

lazy_static! {
    static ref OUTCOME_MAP: HashMap<char, Outcome> = HashMap::from([
        ('X', Outcome::Lose),
        ('Y', Outcome::Draw),
        ('Z', Outcome::Win)
    ]);
}

lazy_static! {
    static ref OPPONENT_MAP: HashMap<char, RPS> =
        HashMap::from([('A', RPS::Rock), ('B', RPS::Paper), ('C', RPS::Scissors)]);
}

lazy_static! {
    static ref PLAY_SCORES: HashMap<RPS, u32> =
        HashMap::from([(RPS::Rock, 1), (RPS::Paper, 2), (RPS::Scissors, 3)]);
}

lazy_static! {
    static ref OUTCOME_SCORES: HashMap<Outcome, u32> =
        HashMap::from([(Outcome::Lose, 0), (Outcome::Draw, 3), (Outcome::Win, 6)]);
}

fn main() {
    // get args from command line
    let args: Vec<String> = env::args().collect();
    // read the filename from the args
    let filename = parse_args(&args);

    // parse the games from the file
    let contents = get_contents(filename);
    let games = parse_lines(contents);

    // get the scores based on the mapping
    let scores: Vec<u32> = games.iter().map(score_round).collect();

    println!("{:?}", scores.iter().sum::<u32>());

    // get the scores based on the strategy
    let played_scores: Vec<u32> = games.iter().map(play_round).collect();

    println!("{:?}", played_scores.iter().sum::<u32>())
}

fn score_round(round: &[char; 2]) -> u32 {
    let mut score = 0;
    let opponent_play = OPPONENT_MAP
        .get(&round[0])
        .expect(r#"error parsing opponent play"#);

    let my_play = MY_MAP.get(&round[1]).expect(r#"error parsing my play"#);
    score += PLAY_SCORES.get(my_play).expect(r#"error getting scores"#);

    if my_play.ties(opponent_play) {
        score += 3
    } else if my_play.beats(opponent_play) {
        score += 6
    }

    score
}

fn play_round(round: &[char; 2]) -> u32 {
    let mut score: u32 = 0;

    let opponent_play = OPPONENT_MAP
        .get(&round[0])
        .expect(r#"error parsing opponent play"#);

    let target_outcome = OUTCOME_MAP
        .get(&round[1])
        .expect(r#"error parsing target outcome"#);

    score += OUTCOME_SCORES
        .get(target_outcome)
        .expect(r#"error scoring outcome"#);

    let drawing_play = opponent_play;
    let winning_play = opponent_play.loses_to();
    let losing_play = opponent_play.wins_against();
    let my_play = match target_outcome {
        Outcome::Draw => drawing_play,
        Outcome::Win => &winning_play,
        Outcome::Lose => &losing_play,
    };

    score += PLAY_SCORES.get(my_play).expect(r#"error scoring my play"#);

    score
}

fn parse_lines(contents: Vec<String>) -> Vec<[char; 2]> {
    let mut games = Vec::new();
    for line in contents.iter() {
        let mut game = [' '; 2];
        for (ii, c) in line.char_indices() {
            if ii == 0 {
                game[0] = c;
            }
            if ii == 2 {
                game[1] = c;
            }
        }
        games.push(game)
    }
    games
}

fn get_contents(filename: &str) -> Vec<String> {
    // read file
    let file = fs::File::open(filename).expect(r#"Error reading file"#);
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect(r#"error parsing line"#))
        .collect()
}

fn parse_args(args: &[String]) -> &str {
    let filename: &String = &args[1];

    filename
}

#[test]
fn test_main() {}
