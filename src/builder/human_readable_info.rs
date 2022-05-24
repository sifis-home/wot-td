use crate::thing::MultiLanguage;

use super::MultiLanguageBuilder;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct HumanReadableInfo {
    pub(super) attype: Option<Vec<String>>,
    pub(super) title: Option<String>,
    pub(super) titles: Option<MultiLanguage>,
    pub(super) description: Option<String>,
    pub(super) descriptions: Option<MultiLanguage>,
}

pub trait BuildableHumanReadableInfo {
    fn attype(self, value: impl Into<String>) -> Self;
    fn title(self, value: impl Into<String>) -> Self;
    fn titles<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>;
    fn description(self, value: impl Into<String>) -> Self;
    fn descriptions<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>;
}

impl BuildableHumanReadableInfo for HumanReadableInfo {
    fn attype(mut self, value: impl Into<String>) -> Self {
        self.attype
            .get_or_insert_with(Default::default)
            .push(value.into());
        self
    }

    fn title(mut self, value: impl Into<String>) -> Self {
        self.title = Some(value.into());
        self
    }

    fn titles<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);
        self.titles = Some(builder.values);
        self
    }

    fn description(mut self, value: impl Into<String>) -> Self {
        self.description = Some(value.into());
        self
    }

    fn descriptions<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);
        self.descriptions = Some(builder.values);
        self
    }
}

macro_rules! impl_delegate_buildable_hr_info {
    ($( $ty:ident $( < $( $generic:ident $(: $bound:ident)? ),+ > )? on $($inner_path:ident).+ ),+ $(,)?) => {
        $(
            impl $(< $($generic $(: $bound)? ),+ >)? BuildableHumanReadableInfo for $ty $(< $($generic),+ >)?
            {
                #[inline]
                fn attype(mut self, value: impl Into<String>) -> Self {
                    self. $($inner_path).+ = self. $($inner_path).+ .attype(value);
                    self
                }

                #[inline]
                fn title(mut self, value: impl Into<String>) -> Self {
                    self. $($inner_path).+ = self. $($inner_path).+ .title(value);
                    self
                }

                #[inline]
                fn titles<F>(mut self, f: F) -> Self
                where
                    F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
                {
                    self. $($inner_path).+ = self. $($inner_path).+ .titles(f);
                    self
                }

                #[inline]
                fn description(mut self, value: impl Into<String>) -> Self {
                    self. $($inner_path).+ = self. $($inner_path).+ .description(value);
                    self
                }

                #[inline]
                fn descriptions<F>(mut self, f: F) -> Self
                where
                    F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
                {
                    self. $($inner_path).+ = self. $($inner_path).+ .descriptions(f);
                    self
                }
            }
        )+
    };
}

pub(super) use impl_delegate_buildable_hr_info;
