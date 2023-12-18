use std::io::stdin;

use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{self, digit1},
    multi::{many0, many1},
    sequence::tuple,
    IResult,
};

fn parse_num(input: &str) -> IResult<&str, u32> {
    let (input, _) = many0(complete::char(' '))(input)?;
    let (input, num) = digit1(input)?;
    Ok((input, num.parse().unwrap()))
}

fn parse_card(input: &str) -> IResult<&str, u32> {
    let (input, _) = tuple((take_until(":"), tag(":")))(input)?;
    let (input, mut winning_nums) = many1(parse_num)(input)?;
    let (input, _) = tag(" | ")(input)?;
    let (input, mut my_nums) = many1(parse_num)(input)?;

    winning_nums.sort();
    my_nums.sort();

    Ok((input, calc_score(winning_nums, my_nums, 0)))
}

fn calc_score(
    winning_nums: impl IntoIterator<Item = u32>,
    my_nums: impl IntoIterator<Item = u32>,
    points: u32,
) -> u32 {
    let mut winning_nums = winning_nums.into_iter().peekable();
    let mut my_nums = my_nums.into_iter().peekable();
    if let (Some(&w), Some(&m)) = (winning_nums.peek(), my_nums.peek()) {
        if w > m {
            calc_score(winning_nums, my_nums.skip(1), points)
        } else if w < m {
            calc_score(winning_nums.skip(1), my_nums, points)
        } else {
            calc_score(
                winning_nums,
                my_nums.skip(1),
                if points == 0 { 1 } else { points * 2 },
            )
        }
    } else {
        points
    }
}

fn part1() -> u32 {
    stdin().lines().into_iter().fold(0, |acc, line| {
        acc + parse_card(line.unwrap().as_str()).unwrap().1
    })
}

fn main() {
    println!("{}", part1());
}
