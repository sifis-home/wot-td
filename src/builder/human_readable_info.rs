//! Human readable information and semantic tagging
//!
//! This module contains the logic shared across multiple builders for the respective
//! Thing Description Vocabulary definitions.

use crate::thing::MultiLanguage;

use super::MultiLanguageBuilder;

/// Human readable informations and semantic tagging
#[derive(Debug, Default, Clone, PartialEq)]
pub struct HumanReadableInfo {
    /// JSON-LD @type
    /// The meaning of each @type is given by the current JSON-LD @context.
    pub(super) attype: Option<Vec<String>>,
    /// Human readable title in the default language
    pub(super) title: Option<String>,
    /// Human redable title, multilanguage
    pub(super) titles: Option<MultiLanguage>,
    /// Human readable description in the default language
    pub(super) description: Option<String>,
    /// Human readable description, multilanguage
    pub(super) descriptions: Option<MultiLanguage>,
}

/// Trait shared across builders dealing with the same information
pub trait BuildableHumanReadableInfo {
    /// Set JSON-LD @type
    ///
    /// It can be called as many times as needed to add multiple @types.
    fn attype(self, value: impl Into<String>) -> Self;
    /// Set the title
    ///
    /// Calling it multiple times overwrites the field.
    fn title(self, value: impl Into<String>) -> Self;
    /// Set the translations of the title
    ///
    /// Calling it multiple times overwrites the field.
    fn titles<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>;
    /// Set the description
    ///
    /// Calling it multiple times overwrites the field.
    fn description(self, value: impl Into<String>) -> Self;
    /// Set the translations of the description
    ///
    /// Calling it multiple times overwrites the field.
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
