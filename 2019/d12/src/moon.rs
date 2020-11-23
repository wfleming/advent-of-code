pub type Vector = (i32, i32, i32);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Moon {
    pub pos: Vector,
    pub vel: Vector,
}
