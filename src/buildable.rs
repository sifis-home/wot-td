use crate::thing::Thing;
use crate::traits::*;
use std::marker::PhantomData;

#[derive(Default)]
pub struct ThingBuilder<T, I, F, D>
where
    T: Builder,
    I: Builder,
    F: Builder,
    D: Builder,
{
    properties: PhantomData<I>, // HashMap<String, PropertyAffordance<I, F, D>>>,

    actions: PhantomData<I>, // HashMap<String, ActionAffordance<I, F, D>>>,

    events: PhantomData<I>, // HashMap<String, EventAffordance<I, F, D>>>,
    forms: PhantomData<F>,  // Vec<FormBuilder<F>>,
    uri_variables: PhantomData<D>, // HashMap<String, DataSchemaBuilder<D>>,
    other: T,
}

impl<T, I, F, D> Buildable for Thing<T, I, F, D>
where
    T: Buildable,
    I: Buildable,
    F: Buildable,
    D: Buildable,
{
    type B = ThingBuilder<T::B, I::B, F::B, D::B>;
}

impl<T, I, F, D> Builder for ThingBuilder<T, I, F, D>
where
    T: Builder,
    I: Builder,
    F: Builder,
    D: Builder,
{
    type B = Thing<T::B, I::B, F::B, D::B>;

    fn build(&self) -> Self::B {
        let other = self.other.build();

        Self::B {
            other,
            ..Default::default()
        }
    }
}

impl<T, I, F, D> ThingBuilder<T, I, F, D>
where
    T: Builder,
    I: Builder,
    F: Builder,
    D: Builder,
{
    fn other<Fun>(&mut self, mut f: Fun) -> &mut Self
    where
        Fun: FnMut(&mut T) -> &mut T,
    {
        f(&mut self.other);

        self
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hlist::Nil;
    use crate::thing::Thing;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Deserialize, Serialize, Default)]
    struct TestThingExtension {
        test: String,
    }

    #[derive(Default)]
    struct TestThingBuilder {
        test: String,
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

    #[test]
    fn build_thing() {
        let t = Thing::<Nil, Nil, Nil, Nil>::builder().build();

        dbg!(&t);

        let t = Thing::<TestThingExtension, Nil, Nil, Nil>::builder()
            .other(|o| o.test("Success"))
            .build();

        dbg!(&t);
    }
}
