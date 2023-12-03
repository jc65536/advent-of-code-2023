use std::cmp::max;
use std::io::stdin;

use nom::{
    branch::alt, bytes::complete::tag, character::complete::digit1, error::Error, sequence::tuple,
    IResult,
};

#[derive(Clone, Copy)]
enum Color {
    RED,
    GREEN,
    BLUE,
}

impl Color {
    fn as_str(&self) -> &'static str {
        match self {
            Color::RED => "red",
            Color::GREEN => "green",
            Color::BLUE => "blue",
        }
    }
}

fn parse_color<'a>(color: Color, max: u32) -> impl FnMut(&'a str) -> IResult<&str, bool> {
    nom::combinator::map(
        tuple((digit1, tag(" "), tag(color.as_str()))),
        move |(digits, _, _)| str::parse::<u32>(digits).unwrap() <= max,
    )
}

fn parse_color2<'a>(color: Color) -> impl FnMut(&'a str) -> IResult<&str, (Color, u32)> {
    nom::combinator::map(
        tuple((digit1, tag(" "), tag(color.as_str()))),
        move |(digits, _, _)| (color, str::parse(digits).unwrap()),
    )
}

fn parse_any_color(input: &str) -> IResult<&str, bool> {
    alt((
        parse_color(Color::RED, 12),
        parse_color(Color::GREEN, 13),
        parse_color(Color::BLUE, 14),
    ))(input)
}

fn parse_any_color2(input: &str) -> IResult<&str, (Color, u32)> {
    alt((
        parse_color2(Color::RED),
        parse_color2(Color::GREEN),
        parse_color2(Color::BLUE),
    ))(input)
}

fn parse_show(input: &str) -> IResult<&str, bool> {
    let (input, possible) = parse_any_color(input)?;

    if !possible {
        Ok((input, false))
    } else {
        match tag::<&str, &str, Error<&str>>(", ")(input) {
            Ok((input, _)) => parse_show(input),
            _ => Ok((input, true)),
        }
    }
}

fn parse_show2(input: &str, (r, g, b): (u32, u32, u32)) -> IResult<&str, (u32, u32, u32)> {
    let (input, (color, amt)) = parse_any_color2(input)?;

    let rgb = match color {
        Color::RED => (r + amt, g, b),
        Color::GREEN => (r, g + amt, b),
        Color::BLUE => (r, g, b + amt),
    };

    match tag::<&str, &str, Error<&str>>(", ")(input) {
        Ok((input, _)) => parse_show2(input, rgb),
        _ => Ok((input, rgb)),
    }
}

fn parse_game(input: &str) -> IResult<&str, bool> {
    let (input, possible) = parse_show(input)?;

    if !possible {
        Ok((input, false))
    } else {
        match tag::<&str, &str, Error<&str>>("; ")(input) {
            Ok((input, _)) => parse_game(input),
            _ => Ok((input, true)),
        }
    }
}

fn parse_game2(input: &str, (rmax, gmax, bmax): (u32, u32, u32)) -> IResult<&str, (u32, u32, u32)> {
    let (input, (r, g, b)) = parse_show2(input, (0, 0, 0))?;

    let rgb = (max(r, rmax), max(g, gmax), max(b, bmax));

    match tag::<&str, &str, Error<&str>>("; ")(input) {
        Ok((input, _)) => parse_game2(input, rgb),
        _ => Ok((input, rgb)),
    }
}

fn parse_line(input: &str) -> IResult<&str, Option<u32>> {
    let (input, (_, game, _)) = tuple((tag("Game "), digit1, tag(": ")))(input)?;
    let (input, possible) = parse_game(input)?;
    return Ok((input, possible.then(|| str::parse(game).unwrap())));
}

fn parse_line2(input: &str) -> IResult<&str, u32> {
    let (input, _) = tuple((tag("Game "), digit1, tag(": ")))(input)?;
    let (input, (r, g, b)) = parse_game2(input, (0, 0, 0))?;
    return Ok((input, r * g * b));
}

fn part1() -> u32 {
    stdin().lines().into_iter().fold(0, |acc, line| {
        acc + parse_line(&line.unwrap()).unwrap().1.unwrap_or(0)
    })
}

fn part2() -> u32 {
    stdin().lines().into_iter().fold(0, |acc, line| {
        acc + parse_line2(&line.unwrap()).unwrap().1
    })
}

fn main() {
    println!("{}", part2());
}
