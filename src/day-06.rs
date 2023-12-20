use std::io::stdin;
use nom::{IResult, bytes::complete::tag, multi::separated_list1, character::complete::{multispace1, digit1}, sequence::tuple};

fn parse_times(input: &str) -> IResult<&str, Vec<u64>> {
    let (input, _) = tuple((tag("Time:"), multispace1))(input)?;
    let (input, result) = separated_list1(multispace1, digit1)(input)?;
    Ok((input, result.into_iter().map(|s| s.parse().unwrap()).collect()))
}

fn parse_dists(input: &str) -> IResult<&str, Vec<u64>> {
    let (input, _) = tuple((tag("Distance:"), multispace1))(input)?;
    let (input, result) = separated_list1(multispace1, digit1)(input)?;
    Ok((input, result.into_iter().map(|s| s.parse().unwrap()).collect()))
}

fn parse_times2(input: &str) -> IResult<&str, u64> {
    let (input, _) = tuple((tag("Time:"), multispace1))(input)?;
    let (input, result) = separated_list1(multispace1, digit1)(input)?;
    Ok((input, result.join("").parse().unwrap()))
}

fn parse_dists2(input: &str) -> IResult<&str, u64> {
    let (input, _) = tuple((tag("Distance:"), multispace1))(input)?;
    let (input, result) = separated_list1(multispace1, digit1)(input)?;
    Ok((input, result.join("").parse().unwrap()))
}

fn part1() -> u64 {
    let mut input = stdin().lines();
    let (_, times) = parse_times(&input.next().unwrap().unwrap()).unwrap();
    let (_, dists) = parse_dists(&input.next().unwrap().unwrap()).unwrap();
    times.into_iter().zip(dists.into_iter()).fold(1, |a, (t, d)| {
        let tf = t as f64;
        let df = d as f64;
        let disc = f64::sqrt(tf * tf - 4.0 * df);
        let c1 = (tf - disc) / 2.0;
        a * (t - 2 * (c1.ceil() as u64) + 1)
    })
}

fn part2() -> u64 {
    let mut input = stdin().lines();
    let (_, t) = parse_times2(&input.next().unwrap().unwrap()).unwrap();
    let (_, d) = parse_dists2(&input.next().unwrap().unwrap()).unwrap();
    let tf = t as f64;
    let df = d as f64;
    let disc = f64::sqrt(tf * tf - 4.0 * df);
    let c1 = (tf - disc) / 2.0;
    t - 2 * (c1.ceil() as u64) + 1
}

fn main() {
    println!("{}", part2());
}
