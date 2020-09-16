use rand::thread_rng;
use rand::Rng;

pub type Int = i64;

pub fn rand_range(lo: &Int, hi: &Int) -> Int {
    thread_rng().gen_range(lo, hi)
}
