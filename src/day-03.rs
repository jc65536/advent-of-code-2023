use std::collections::{HashMap, HashSet};
use std::io::stdin;

use nom::{
    branch::alt,
    bytes::complete::*,
    character::complete::{digit1, none_of},
    error::Error,
    multi::many0,
    Err, InputLength,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Loc(u32, u32);

#[derive(Clone, Copy)]
struct StrLoc<'a>(&'a str, Loc);

impl<'a> InputLength for StrLoc<'a> {
    fn input_len(&self) -> usize {
        self.0.input_len()
    }
}

fn parse_dots(StrLoc(input, Loc(r, c)): StrLoc) -> Result<(StrLoc, ()), Err<()>> {
    is_a(".")(input)
        .and_then(|(input, dots)| Ok((StrLoc(input, Loc(r, c + dots.len() as u32)), ())))
        .map_err(|_: nom::Err<Error<&str>>| Err::Error(()))
}

fn parse_number<'a>(
    num: &'a mut Vec<(Loc, u32, u32)>,
) -> impl FnMut(StrLoc) -> Result<(StrLoc, ()), Err<()>> + 'a {
    |StrLoc(input, Loc(r, c))| {
        digit1(input)
            .map(|(input, digits)| {
                num.push((Loc(r, c), digits.len() as u32, digits.parse().unwrap()));
                (StrLoc(input, Loc(r, c + digits.len() as u32)), ())
            })
            .map_err(|_: nom::Err<Error<&str>>| Err::Error(()))
    }
}

fn parse_symbol<'a>(
    sym: &'a mut HashSet<Loc>,
) -> impl FnMut(StrLoc) -> Result<(StrLoc, ()), Err<()>> + 'a {
    |StrLoc(input, Loc(r, c))| {
        none_of(".0123456789")(input)
            .map(|(input, _)| {
                sym.insert(Loc(r, c));
                (StrLoc(input, Loc(r, c + 1)), ())
            })
            .map_err(|_: nom::Err<Error<&str>>| Err::Error(()))
    }
}

fn parse_symbol2<'a>(
    sym: &'a mut HashMap<Loc, (char, u32, u32)>,
) -> impl FnMut(StrLoc) -> Result<(StrLoc, ()), Err<()>> + 'a {
    |StrLoc(input, Loc(r, c))| {
        none_of(".0123456789")(input)
            .map(|(input, ch)| {
                sym.insert(Loc(r, c), (ch, 0, 1));
                (StrLoc(input, Loc(r, c + 1)), ())
            })
            .map_err(|_: nom::Err<Error<&str>>| Err::Error(()))
    }
}

fn parse_line<'a>(
    num: &'a mut Vec<(Loc, u32, u32)>,
    sym: &'a mut HashSet<Loc>,
) -> impl FnMut(StrLoc) -> Result<(StrLoc, ()), Err<()>> + 'a {
    |strloc| {
        many0(alt((parse_dots, parse_number(num), parse_symbol(sym))))(strloc)
            .map(|(StrLoc(input, Loc(r, _)), _)| (StrLoc(input, Loc(r + 1, 0)), ()))
    }
}

fn parse_line2<'a>(
    num: &'a mut Vec<(Loc, u32, u32)>,
    sym: &'a mut HashMap<Loc, (char, u32, u32)>,
) -> impl FnMut(StrLoc) -> Result<(StrLoc, ()), Err<()>> + 'a {
    |strloc| {
        many0(alt((parse_dots, parse_number(num), parse_symbol2(sym))))(strloc)
            .map(|(StrLoc(input, Loc(r, _)), _)| (StrLoc(input, Loc(r + 1, 0)), ()))
    }
}

fn adjacent(Loc(r, c): Loc, len: u32) -> Vec<Loc> {
    let mut v: Vec<Loc> = Vec::new();

    for ra in (if r == 0 { r } else { r - 1 })..=(r + 1) {
        for ca in (if c == 0 { c } else { c - 1 })..=(c + len) {
            if !(ra == r && (c..c + len).contains(&ca)) {
                v.push(Loc(ra, ca));
            }
        }
    }

    v
}

fn part1() -> u32 {
    let mut num: Vec<(Loc, u32, u32)> = Vec::new();
    let mut sym: HashSet<Loc> = HashSet::new();

    {
        let mut parse_line = parse_line(&mut num, &mut sym);
        stdin().lines().into_iter().fold(Loc(0, 0), |loc, line| {
            let line = line.unwrap();
            parse_line(StrLoc(line.as_str(), loc)).unwrap().0 .1
        });
    }

    num.iter().fold(0, |acc, (loc, len, n)| {
        let adj = adjacent(*loc, *len);
        if adj.iter().any(|loc| sym.contains(loc)) {
            acc + n
        } else {
            acc
        }
    })
}

fn part2() -> u32 {
    let mut num: Vec<(Loc, u32, u32)> = Vec::new();
    let mut sym: HashMap<Loc, (char, u32, u32)> = HashMap::new();

    {
        let mut parse_line = parse_line2(&mut num, &mut sym);
        stdin().lines().into_iter().fold(Loc(0, 0), |loc, line| {
            let line = line.unwrap();
            parse_line(StrLoc(line.as_str(), loc)).unwrap().0 .1
        });
    }

    num.iter().for_each(|(loc, len, n)| {
        let adj = adjacent(*loc, *len);
        adj.iter().for_each(|loc| {
            if let Some((ch, count, prod)) = sym.get_mut(loc) {
                if *ch == '*' {
                    *count += 1;
                    *prod *= n;
                }
            }
        });
    });

    sym.iter().fold(
        0,
        |acc, (_, (_, count, prod))| {
            if *count == 2 {
                acc + prod
            } else {
                acc
            }
        },
    )
}

fn main() {
    println!("{}", part2());
}
