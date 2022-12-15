use std::cmp::{max, min};
use std::collections::BTreeSet;
use std::env::args;
use std::fs;
use std::ops::RangeInclusive;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Pt {
    x: i64,
    y: i64,
}

impl Pt {
    pub fn new(x: i64, y: i64) -> Self {
        Self { x, y }
    }

    pub fn dist(&self, other: &Self) -> i64 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

type Sensors = Vec<(Pt, Pt, i64)>;

fn parse_sensors(input: &str) -> Sensors {
    let mut sensors = Sensors::new();

    for line in input.lines() {
        let sx0 = line.find("x=").unwrap() + 2;
        let sx1 = sx0 + line[sx0..].find(',').unwrap();
        let sx = line[sx0..sx1].parse::<i64>().unwrap();
        let sy0 = sx1 + line[sx1..].find("y=").unwrap() + 2;
        let sy1 = sy0 + line[sy0..].find(':').unwrap();
        let sy = line[sy0..sy1].parse::<i64>().unwrap();

        let bx0 = sy1 + line[sy1..].find("x=").unwrap() + 2;
        let bx1 = bx0 + line[bx0..].find(',').unwrap();
        let bx = line[bx0..bx1].parse::<i64>().unwrap();
        let by0 = bx1 + line[bx1..].find("y=").unwrap() + 2;
        let by = line[by0..].parse::<i64>().unwrap();

        let sensor_pt = Pt::new(sx, sy);
        let beacon_pt = Pt::new(bx, by);

        sensors.push((sensor_pt, beacon_pt, sensor_pt.dist(&beacon_pt)));
    }

    sensors
}

fn blocked_range_in_row(sensor_pt: &Pt, sensor_range: i64, y: i64) -> RangeInclusive<i64> {
    let y_delta = (sensor_pt.y - y).abs();
    let x_range = sensor_range - y_delta;

    (sensor_pt.x - x_range)..=(sensor_pt.x + x_range)
}

// we could also merge adjacent ranges (e.g. 2..=4 and 5..=10), but i'm not bothering, it's fine.
fn merge_ranges(ranges: &mut Vec<RangeInclusive<i64>>) {
    let mut overlap_found = false;
    for r1_idx in 0..ranges.len() {
        let r1 = &ranges[r1_idx];
        let r2_idx_match = ranges.iter().enumerate().find_map(|(r2_idx, r2)| {
            if r1_idx != r2_idx
                && (r1.contains(r2.start())
                    || r1.contains(r2.end())
                    || r2.contains(r1.start())
                    || r2.contains(r1.end()))
            {
                Some(r2_idx)
            } else {
                None
            }
        });

        match r2_idx_match {
            Some(r2_idx) => {
                let r2 = &ranges[r2_idx];
                let merged_r = *min(r1.start(), r2.start())..=*max(r1.end(), r2.end());
                ranges[r1_idx] = merged_r;
                ranges.remove(r2_idx);
                overlap_found = true;
                break;
            }
            None => continue,
        }
    }

    if overlap_found {
        merge_ranges(ranges);
    }
}

fn blocked_ranges_in_row(sensors: &Sensors, y_target: i64) -> Vec<RangeInclusive<i64>> {
    let mut ranges: Vec<RangeInclusive<i64>> = Vec::new();

    for (s_pt, _, s_range) in sensors {
        if s_range >= &(s_pt.y - y_target).abs() {
            let new_r = blocked_range_in_row(s_pt, *s_range, y_target);
            ranges.push(new_r.clone());
            merge_ranges(&mut ranges);
        }
    }

    ranges
}

fn p1(sensors: &Sensors, y_target: i64) {
    let blocked_ranges = blocked_ranges_in_row(sensors, y_target);
    let mut blocked_pt_count = blocked_ranges
        .iter()
        .fold(0, |accum, r| accum + 1 + (r.end() - r.start()));
    // p1 only counts squares without something else in them (sensor or beacon), so subtract those
    let mut discount_pts = BTreeSet::new();

    for (s_pt, b_pt, _) in sensors {
        if s_pt.y == y_target && blocked_ranges.iter().any(|r| r.contains(&s_pt.x)) {
            discount_pts.insert(s_pt);
        } else if b_pt.y == y_target && blocked_ranges.iter().any(|r| r.contains(&b_pt.x)) {
            discount_pts.insert(b_pt);
        }
    }
    blocked_pt_count -= discount_pts.len() as i64;
    println!(
        "p1: at y == {}, {} positions cannot contain a beacon",
        y_target, blocked_pt_count
    );
}

fn p2(sensors: &Sensors, max_xy: i64) {
    for y in 0..=max_xy {
        let blocked_ranges = blocked_ranges_in_row(sensors, y);
        let blocked_pt_count = blocked_ranges.iter().fold(0, |accum, r| {
            let clamped_r_start = max(0, *r.start());
            let clamped_r_end = min(max_xy, *r.end());
            accum + 1 + (clamped_r_end - clamped_r_start)
        });
        let open_ct = max_xy + 1 - blocked_pt_count;

        if open_ct == 1 {
            for x in 0..=max_xy {
                if !blocked_ranges.iter().any(|r| r.contains(&x)) {
                    println!("p2: found distress signal at ({},{})", x, y);
                    let tuning_freq = (x * 4000000) + y;
                    println!("p2: tuning frequency is {}", tuning_freq);
                    return;
                }
            }
        }
    }
    panic!("p2: did not find a unique possible position");
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let sensors = parse_sensors(&input);
    let p1_y = args
        .next()
        .expect("provide p1 y val")
        .parse::<i64>()
        .expect("should be a number");

    p1(&sensors, p1_y);

    let p2_max = p1_y * 2;
    p2(&sensors, p2_max);
}
