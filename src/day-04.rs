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

    Ok((
        input,
        calc_score(&mut winning_nums.into_iter(), &mut my_nums.into_iter(), 0),
    ))
}

fn parse_card2(input: &str) -> IResult<&str, (u32, u32)> {
    let (input, (_, _, card_num, _)) =
        tuple((tag("Card"), many0(complete::char(' ')), digit1, tag(":")))(input)?;
    let (input, mut winning_nums) = many1(parse_num)(input)?;
    let (input, _) = tag(" | ")(input)?;
    let (input, mut my_nums) = many1(parse_num)(input)?;

    winning_nums.sort();
    my_nums.sort();

    Ok((
        input,
        (
            card_num.parse().unwrap(),
            count_matches(&mut winning_nums.into_iter(), &mut my_nums.into_iter(), 0),
        ),
    ))
}

fn calc_score(
    winning_nums: &mut dyn Iterator<Item = u32>,
    my_nums: &mut dyn Iterator<Item = u32>,
    points: u32,
) -> u32 {
    let mut winning_nums = winning_nums.peekable();
    let mut my_nums = my_nums.peekable();
    if let (Some(&w), Some(&m)) = (winning_nums.peek(), my_nums.peek()) {
        if w > m {
            calc_score(&mut winning_nums, &mut my_nums.skip(1), points)
        } else if w < m {
            calc_score(&mut winning_nums.skip(1), &mut my_nums, points)
        } else {
            calc_score(
                &mut winning_nums,
                &mut my_nums.skip(1),
                if points == 0 { 1 } else { points * 2 },
            )
        }
    } else {
        points
    }
}

fn count_matches(
    winning_nums: &mut dyn Iterator<Item = u32>,
    my_nums: &mut dyn Iterator<Item = u32>,
    matches: u32,
) -> u32 {
    let mut winning_nums = winning_nums.peekable();
    let mut my_nums = my_nums.peekable();
    if let (Some(&w), Some(&m)) = (winning_nums.peek(), my_nums.peek()) {
        if w > m {
            count_matches(&mut winning_nums, &mut my_nums.skip(1), matches)
        } else if w < m {
            count_matches(&mut winning_nums.skip(1), &mut my_nums, matches)
        } else {
            count_matches(&mut winning_nums, &mut my_nums.skip(1), matches + 1)
        }
    } else {
        matches
    }
}

fn part1() -> u32 {
    stdin().lines().into_iter().fold(0, |acc, line| {
        acc + parse_card(line.unwrap().as_str()).unwrap().1
    })
}

fn part2() -> u32 {
    let mut card_counts = [1; 202];
    stdin().lines().into_iter().for_each(|line| {
        let (_, (card_num, matches)) = parse_card2(line.unwrap().as_str()).unwrap();
        let card_num = card_num - 1;
        let count_of_card_num = card_counts[card_num as usize];
        card_counts[(card_num + 1) as usize..(card_num + 1 + matches) as usize]
            .iter_mut()
            .for_each(|n| *n += count_of_card_num);
    });
    card_counts.iter().sum()
}

fn main() {
    println!("{}", part2());
}
