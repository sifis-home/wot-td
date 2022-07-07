pub trait Buildable: Default {
    type B: Builder;

    fn builder() -> Self::B {
        Self::B::default()
    }
}

pub trait Builder: Default {
    type B: Buildable;

    fn build(&self) -> Self::B;
}
