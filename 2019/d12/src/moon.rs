pub type Vector = (i32, i32, i32);

#[derive(Clone)]
#[derive(Debug)]
#[derive(Eq)]
#[derive(PartialEq)]
pub struct Moon {
    pub pos: Vector,
    pub vel: Vector,
}
