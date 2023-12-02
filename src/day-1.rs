use regex::{Match, Regex};
use std::io;

fn part1() -> u32 {
    io::stdin().lines().fold(0, |acc, line| {
        let line = line.unwrap();
        line.chars()
            .find_map(|c| c.to_digit(10))
            .and_then(|n| {
                line.chars()
                    .rev()
                    .find_map(|c| c.to_digit(10))
                    .map(|m| 10 * n + m)
            })
            .unwrap()
            + acc
    })
}

const NUM_WORDS: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

fn part2() -> u32 {
    let disj = NUM_WORDS.join("|");
    let re_front = Regex::new(&format!(r"^.*?(\d|{disj}).*$")).unwrap();
    let re_back = Regex::new(&format!(r"^.*(\d|{disj}).*?$")).unwrap();

    io::stdin().lines().fold(0, |acc, line| {
        let line = line.unwrap();

        let match_dig = |re: &Regex| {
            re.captures(&line)
                .unwrap()
                .iter()
                .skip(1)
                .next()
                .flatten()
                .map(|m| m.as_str())
                .and_then(|cap| {
                    NUM_WORDS
                        .iter()
                        .enumerate()
                        .find_map(|(i, &word)| cap.eq(word).then_some((i + 1) as u32))
                        .or_else(|| cap.chars().next().unwrap().to_digit(10))
                })
                .unwrap()
        };

        acc + 10 * match_dig(&re_front) + match_dig(&re_back)
    })
}

fn main() {
    let ans = part2();
    println!("{ans}");
}
