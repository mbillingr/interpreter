use num_bigint::{BigInt, RandBigInt};
use rand::thread_rng;

pub type Int = BigInt;

pub fn rand_range(lo: &Int, hi: &Int) -> Int {
    thread_rng().gen_bigint_range(lo, hi)
}
