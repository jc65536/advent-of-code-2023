use std::io::stdin;

use nom::{
    bytes::complete::tag,
    character::complete::digit1,
    error::Error,
    multi::{many1, separated_list1},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
struct Map(Vec<(u64, u64, u64)>);

impl Map {
    fn add(&mut self, dest_start: u64, src_start: u64, range_len: u64) {
        self.0.push((dest_start, src_start, range_len));
    }

    fn get(&self, src: u64) -> u64 {
        for &(dest_start, src_start, range_len) in &self.0 {
            if (src_start..(src_start + range_len)).contains(&src) {
                return src - src_start + dest_start;
            }
        }
        src
    }
}

fn unwrap<T, E>(r: Result<T, E>) -> T {
    match r {
        Ok(t) => t,
        _ => panic!(),
    }
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<u64>,
    seed_soil_map: Map,
    soil_fertilizer_map: Map,
    fertilizer_water_map: Map,
    water_light_map: Map,
    light_temperature_map: Map,
    temperature_humidity_map: Map,
    humidity_location_map: Map,
}

impl Almanac {
    fn get_lowest_loc(&self) -> u64 {
        let mut min_location: u64 = u64::MAX;

        for &seed in &self.seeds {
            let soil = self.seed_soil_map.get(seed);
            let fertilizer = self.soil_fertilizer_map.get(soil);
            let water = self.fertilizer_water_map.get(fertilizer);
            let light = self.water_light_map.get(water);
            let temperature = self.light_temperature_map.get(light);
            let humidity = self.temperature_humidity_map.get(temperature);
            let location = self.humidity_location_map.get(humidity);

            min_location = min_location.min(location);
        }

        min_location
    }
}

fn parse_seeds<'b>(
    mut input: Box<dyn Iterator<Item = String>>,
) -> IResult<Box<dyn Iterator<Item = String>>, Vec<u64>> {
    let line = input.next().unwrap();
    let (line, _) = tag::<&str, &str, Error<&str>>("seeds:")(&line).unwrap();
    let (_, seeds) = many1(tuple((tag(" "), digit1::<&str, Error<&str>>)))(line).unwrap();
    let seeds: Vec<u64> = seeds.iter().map(|(_, s)| s.parse().unwrap()).collect();
    Ok((input, seeds))
}

fn parse_entry(
    input: Box<dyn Iterator<Item = String>>,
) -> Result<(Box<dyn Iterator<Item = String>>, (u64, u64, u64)), Box<dyn Iterator<Item = String>>> {
    let mut input = Box::new(input.peekable());
    let line = match input.peek() {
        Some(x) => x,
        None => return Err(input),
    };

    let Ok((_, result)) = separated_list1(tag(" "), digit1::<&str, ()>)(line) else {
        return Err(input);
    };

    let result: Vec<u64> = result.iter().map(|s| s.parse().unwrap()).collect();
    let [a, b, c] = result[..] else { panic!() };

    Ok((Box::new(input.skip(1)), (a, b, c)))
}

fn parse_map<'a>(
    input: Box<dyn Iterator<Item = String>>,
) -> IResult<Box<dyn Iterator<Item = String>>, Map> {
    let mut input: Box<dyn Iterator<Item = String>> = Box::new(input.skip(1));
    let mut m = Map(Vec::new());
    loop {
        match parse_entry(input) {
            Ok((inp, (a, b, c))) => {
                m.add(a, b, c);
                input = inp;
            }
            Err(input) => return Ok((input, m)),
        }
    }
}

fn parse_almanac<'a>(
    input: Box<dyn Iterator<Item = String>>,
) -> IResult<Box<dyn Iterator<Item = String>>, Almanac> {
    let (input, seeds) = unwrap(parse_seeds(input));
    let input = Box::new(input.skip(1));
    let (input, seed_soil_map) = unwrap(parse_map(input));
    let input = Box::new(input.skip(1));
    let (input, soil_fertilizer_map) = unwrap(parse_map(input));
    let input = Box::new(input.skip(1));
    let (input, fertilizer_water_map) = unwrap(parse_map(input));
    let input = Box::new(input.skip(1));
    let (input, water_light_map) = unwrap(parse_map(input));
    let input = Box::new(input.skip(1));
    let (input, light_temperature_map) = unwrap(parse_map(input));
    let input = Box::new(input.skip(1));
    let (input, temperature_humidity_map) = unwrap(parse_map(input));
    let input = Box::new(input.skip(1));
    let (input, humidity_location_map) = unwrap(parse_map(input));
    Ok((
        input,
        Almanac {
            seeds,
            seed_soil_map,
            soil_fertilizer_map,
            fertilizer_water_map,
            water_light_map,
            light_temperature_map,
            temperature_humidity_map,
            humidity_location_map,
        },
    ))
}

fn part1() -> u64 {
    let (_, almanac) = unwrap(parse_almanac(Box::new(stdin().lines().map(|x| x.unwrap()))));
    println!("{almanac:?}");
    almanac.get_lowest_loc()
}

fn main() {
    println!("{}", part1());
}
