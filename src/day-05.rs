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

// To avoid overflow panic
const MAX: u64 = u64::MAX / 2;

fn _merge(
    v: &[(u64, u64, u64)],
    w: &[(u64, u64, u64)],
    mut dest: Vec<(u64, u64, u64)>,
) -> Vec<(u64, u64, u64)> {
    if v.is_empty() || w.is_empty() {
        dest
    } else {
        let &(d1, s1, l1) = v.first().unwrap();
        let &(d2, s2, l2) = w.first().unwrap();
        if d1 + l1 <= s2 {
            _merge(&v[1..], w, dest)
        } else if s2 + l2 <= d1 {
            _merge(v, &w[1..], dest)
        } else if v.len() == 1 {
            let lb = d1.max(s2);
            let ub = (d1 + l1).min(s2 + l2);
            let s3 = s1 + lb - d1;
            let d3 = d2 + lb - s2;
            let l3 = ub - lb;
            dest.push((d3, s3, l3));
            _merge(v, &w[1..], dest)
        } else {
            _merge(&v[1..], w, _merge(&v[0..1], w, dest))
        }
    }
}

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

    fn complete(&mut self) {
        let v = &mut self.0;
        v.sort_by(|(_, s1, _), (_, s2, _)| s1.cmp(s2));
        let mut additional: Vec<(u64, u64, u64)> = Vec::new();

        let first = v.first().unwrap().1;
        if first > 0 {
            additional.push((0, 0, first));
        }

        v.windows(2).for_each(|w| {
            let ((_, s1, l1), (_, s2, _)) = (w[0], w[1]);
            if s1 + l1 < s2 {
                additional.push((s1 + l1, s1 + l1, s2 - (s1 + l1)))
            }
        });

        let last_entry = v.last().unwrap();
        let last = last_entry.1 + last_entry.2;
        if last <= MAX {
            additional.push((last, last, MAX - last + 1))
        }

        v.append(&mut additional);
    }

    fn merge(&mut self, other: &mut Map) -> Map {
        let v = &mut self.0;
        v.sort_by(|(d1, _, _), (d2, _, _)| d1.cmp(d2));

        let w = &mut other.0;
        w.sort_by(|(_, s1, _), (_, s2, _)| s1.cmp(s2));

        Map(_merge(v, w, Vec::new()))
    }

    fn print(&self) {
        for &(a, b, c) in &self.0 {
            println!("{a:20}{b:20}{c:20}");
        }
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

    fn complete(&mut self) {
        self.seed_soil_map.complete();
        self.soil_fertilizer_map.complete();
        self.fertilizer_water_map.complete();
        self.water_light_map.complete();
        self.light_temperature_map.complete();
        self.temperature_humidity_map.complete();
        self.humidity_location_map.complete();
    }

    fn merge_all(&mut self) -> Map {
        self.seed_soil_map
            .merge(&mut self.soil_fertilizer_map)
            .merge(&mut self.fertilizer_water_map)
            .merge(&mut self.water_light_map)
            .merge(&mut self.light_temperature_map)
            .merge(&mut self.temperature_humidity_map)
            .merge(&mut self.humidity_location_map)
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
    almanac.get_lowest_loc()
}

fn part2() -> u64 {
    let (_, mut almanac) = unwrap(parse_almanac(Box::new(stdin().lines().map(|x| x.unwrap()))));

    almanac.complete();

    // let mut d = almanac.seed_soil_map.merge(&mut almanac.soil_fertilizer_map);
    // println!("seed soil:");
    // almanac.seed_soil_map.print();
    // println!("soil fertilizer:");
    // almanac.soil_fertilizer_map.print();
    // println!("merged:");
    // d.0.sort_by(|(_, s1, _), (_, s2, _)| s1.cmp(s2));
    // d.print();

    let mut seeds = Map(almanac.seeds.chunks(2).map(|ch| (ch[0], ch[0], ch[1])).collect());

    let mut m = seeds.merge(&mut almanac.merge_all());
    m.0.sort_by(|(d1, _, _), (d2, _, _)| d1.cmp(d2));

    let first = m.0.first().unwrap();

    first.0
}

fn main() {
    println!("{}", part2());
}
