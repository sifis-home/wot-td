use std::{borrow::Cow, fmt, marker::PhantomData};

use qcell::{TLCell, TLCellOwner};
use serde::{
    de::{self, DeserializeSeed, Visitor},
    Deserialize, Deserializer,
};

pub struct SeedMarker;
pub type Contexts<'de> = Vec<(Cow<'de, str>, &'de str)>;
pub type SeedCell<'de> = TLCell<SeedMarker, Contexts<'de>>;
pub type SeedOwner = TLCellOwner<SeedMarker>;
pub struct Seed<'a, 'b, 'de> {
    pub cell: &'a SeedCell<'de>,
    pub owner: &'b mut SeedOwner,
}

impl<'a, 'b, 'de> Seed<'a, 'b, 'de> {
    // TODO: find a better name
    #[inline]
    pub fn reseed(&mut self) -> Seed<'a, '_, 'de> {
        let &mut Self {
            cell,
            ref mut owner,
        } = self;
        Seed { cell, owner }
    }
}

pub trait Seedable {
    type Seed<'a, 'b, 'de>
    where
        'de: 'a;

    fn new_seed<'a, 'b, 'de>(seed: Seed<'a, 'b, 'de>) -> Self::Seed<'a, 'b, 'de>
    where
        'de: 'a;
}

macro_rules! make_seed {
    ( @inner $ty:ident => $name:ident ) => {
        pub struct $name<'a, 'b, 'de> ($crate::seedable::Seed<'a, 'b, 'de>);

        impl $crate::seedable::Seedable for $ty
        {
            type Seed<'a, 'b, 'de> = $name<'a, 'b, 'de>
            where
                'de: 'a;

            #[inline]
            fn new_seed<'a, 'b, 'de>(
                seed: $crate::seedable::Seed<'a, 'b, 'de>,
            ) -> Self::Seed<'a, 'b, 'de>
            where
                'de: 'a
            {
                $name(seed)
            }
        }
    };

    ( @inner $ty:ident < $($generic:ident $( : $bound1:ident $(+ $bound:ident)* )? ),+ > => $name:ident ) => {
        pub struct $name<'a, 'b, 'de, $( $generic $( : $bound1 $(+ $bound)? )? ),+> {
            inner: $crate::seedable::Seed<'a, 'b, 'de>,
            _marker: ::core::marker::PhantomData<$( $generic ),+>,
        }

        impl<$( $generic ),+ > $crate::seedable::Seedable for $ty < $( $generic ),+ >
        where
            $( $generic : $crate::seedable::Seedable $( + $bound1 $(+ $bound)? )? ),+
        {
            type Seed<'a, 'b, 'de> = $name<'a, 'b, 'de, $( $generic ),+ >
            where
                'de: 'a;

            #[inline]
            fn new_seed<'a, 'b, 'de>(
                seed: $crate::seedable::Seed<'a, 'b, 'de>,
            ) -> Self::Seed<'a, 'b, 'de>
            where
                'de: 'a
            {
                $name {
                    inner: seed,
                    _marker: ::core::marker::PhantomData,
                }
            }
        }
    };

    ( $( $ty:ident $(< $($generic:ident $( : $bound1:ident $(+ $bound:ident)* )? ),+ >)? => $name:ident ),+ $(,)? ) => {
        $(
            make_seed!(@inner $ty $(< $($generic $( : $bound1 $(+ $bound)* )? ),+ >)? => $name);
        )+
    };
}
pub(crate) use make_seed;

use crate::serde_helpers::size_hint;

make_seed!(
    Option<T> => OptionSeed,
    Vec<T> => VecSeed,
    String => StringSeed,
    usize => UsizeSeed,
);

struct OptionVisitor<'a, 'b, 'de, T> {
    inner: Seed<'a, 'b, 'de>,
    _marker: PhantomData<T>,
}

impl<'a, 'b, 'de, T> Visitor<'de> for OptionVisitor<'a, 'b, 'de, T>
where
    T: Seedable,
    for<'c> T::Seed<'a, 'c, 'de>: DeserializeSeed<'de, Value = T>,
{
    type Value = Option<T>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("option")
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(None)
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(None)
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        let seed = <T as Seedable>::new_seed(self.inner);
        seed.deserialize(deserializer).map(Some)
    }
}

impl<'a, 'b, 'de, T> DeserializeSeed<'de> for OptionSeed<'a, 'b, 'de, T>
where
    T: Seedable,
    for<'c> T::Seed<'a, 'c, 'de>: DeserializeSeed<'de, Value = T>,
{
    type Value = Option<T>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        let Self { inner, _marker: _ } = self;
        deserializer.deserialize_option(OptionVisitor {
            inner,
            _marker: PhantomData,
        })
    }
}

impl<'a, 'b, 'de> DeserializeSeed<'de> for StringSeed<'a, 'b, 'de> {
    type Value = String;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        String::deserialize(deserializer)
    }
}

struct VecVisitor<'a, 'b, 'de, T> {
    inner: Seed<'a, 'b, 'de>,
    _marker: PhantomData<T>,
}

impl<'a, 'b, 'de, T> Visitor<'de> for VecVisitor<'a, 'b, 'de, T>
where
    T: Seedable,
    for<'c> T::Seed<'a, 'c, 'de>: DeserializeSeed<'de, Value = T>,
{
    type Value = Vec<T>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a sequence")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let capacity = size_hint::cautious(seq.size_hint());
        let mut values = capacity.map(Vec::with_capacity).unwrap_or_default();

        let Self {
            inner: Seed { cell, owner },
            _marker: _,
        } = self;

        while let Some(value) = seq.next_element_seed(T::new_seed(Seed {
            cell,
            owner: &mut *owner,
        }))? {
            values.push(value);
        }

        Ok(values)
    }
}

impl<'a, 'b, 'de, T> DeserializeSeed<'de> for VecSeed<'a, 'b, 'de, T>
where
    T: Seedable,
    for<'c> T::Seed<'a, 'c, 'de>: DeserializeSeed<'de, Value = T>,
{
    type Value = Vec<T>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        let Self { inner, _marker: _ } = self;
        deserializer.deserialize_seq(VecVisitor {
            inner,
            _marker: PhantomData,
        })
    }
}

impl<'a, 'b, 'de> DeserializeSeed<'de> for UsizeSeed<'a, 'b, 'de> {
    type Value = usize;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        usize::deserialize(deserializer)
    }
}

#[cfg(test)]
macro_rules! deserialize {
    ($de:expr, $ty:ty => $contexts:expr) => {{
        let mut seed_owner = SeedOwner::new();
        let seed_cell = SeedCell::new(Contexts::default());

        let seed = <$ty>::new_seed(Seed {
            owner: &mut seed_owner,
            cell: &seed_cell,
        });
        seed.deserialize($de.into_deserializer())
            .map(|deserialized| (deserialized, seed_cell.into_inner()))
    }};

    ($de:expr, $ty:ty) => {
        $crate::seedable::deserialize!($de, $ty => $crate::seedable::Contexts::default())
    }
}

#[cfg(test)]
pub(crate) use deserialize;
