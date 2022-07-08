use crate::thing::Thing;
use crate::traits::*;

#[derive(Default, Debug)]
pub struct ThingBuilder<E: Extension> {
    other: <E::Thing as Buildable>::B,
}

impl<E: Extension> Buildable for Thing<E> {
    type B = ThingBuilder<E>;
}

impl<E: Extension> Builder for ThingBuilder<E> {
    type B = Thing<E>;

    fn build(&self) -> Self::B {
        let other = self.other.build();

        Self::B {
            other,
            ..Default::default()
        }
    }
}

impl<E: Extension> ThingBuilder<E> {
    fn other<Fun>(&mut self, mut f: Fun) -> &mut Self
    where
        Fun: FnMut(&mut <E::Thing as Buildable>::B) -> &mut <E::Thing as Buildable>::B,
    {
        f(&mut self.other);

        self
    }
}

#[cfg(test)]
mod test {
    use std::sync::{Arc, Mutex};

    use super::*;
    use crate::hlist::Nil;
    use crate::thing::Thing;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Deserialize, Serialize, Default, Clone, PartialEq)]
    struct TestThingExtension {
        test: String,
    }

    #[derive(Default, Debug)]
    struct TestThingBuilder {
        test: String,
        something: Option<Arc<Mutex<usize>>>,
    }

    impl TestThingBuilder {
        fn test(&mut self, test: impl Into<String>) -> &mut Self {
            self.test = test.into();
            self
        }
    }

    impl Buildable for TestThingExtension {
        type B = TestThingBuilder;

        fn builder() -> Self::B {
            TestThingBuilder::default()
        }
    }

    impl Builder for TestThingBuilder {
        type B = TestThingExtension;

        fn build(&self) -> Self::B {
            TestThingExtension {
                test: self.test.clone(),
            }
        }
    }

    #[derive(Debug, Deserialize, Serialize, Default, Clone, PartialEq)]
    struct TestExtension;

    impl Extension for TestExtension {
        type Thing = TestThingExtension;
        type InteractionAffordance = Nil;
        type Form = Nil;
        type DataSchema = Nil;
    }

    #[test]
    fn build_thing() {
        let t = Thing::<Nil>::builder().build();

        dbg!(&t);

        let mut tb = Thing::<TestExtension>::builder();

        let t = tb.other(|o| o.test("Success")).build();
        let t2 = tb.other(|o| o.test("Second")).build();

        dbg!(&t);
        dbg!(&t2);
    }
}
