// TODO: use better trait bounds
pub trait Extension: Default + Clone + std::fmt::Debug + PartialEq {
    type Thing: Buildable;
    type InteractionAffordance: Buildable;
    type Form: Buildable;
    type DataSchema: Buildable;
}

pub trait Builder: Default + Clone + std::fmt::Debug + PartialEq {
    type B: Buildable<B = Self>;

    fn build(&self) -> Self::B;
}

pub trait Buildable: Default + Clone + std::fmt::Debug + PartialEq {
    type B: Builder<B = Self>;

    fn builder() -> Self::B {
        Self::B::default()
    }
}
