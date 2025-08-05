// String tokenizing submodule
// TODO: Implement string-specific automata construction

use deterministic_automata::{either_automaton::Either, DeterministicAutomatonBlueprint};

use crate::low_level::LowTokenMatchVal;

#[derive(Clone, Debug)]
pub(crate) struct StringStyle {
    start: Vec<LowTokenMatchVal>,
    end: Vec<LowTokenMatchVal>,
    multi_line: bool,
    escapable: bool
}

#[derive(Clone, Debug)]
pub(crate) struct CustomStringStyles(Vec<StringStyle>);

#[derive(Clone, Debug)]
pub(crate) enum StringStyles {
    Custom(CustomStringStyles),
    Rust
}

impl StringStyles {
    pub(crate) fn to_either(self) -> Either<CustomStringStyles,RustStringStyles> {
        match self {
            StringStyles::Custom(custom_string_styles) => Either::Left(custom_string_styles),
            StringStyles::Rust => Either::Right(RustStringStyles),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum StringStateSort {
    Reject,
    Unfound,
    Found,
    Accept
}

#[derive(Clone, Debug)]
pub(crate) struct CustomStringState {}

impl DeterministicAutomatonBlueprint for CustomStringStyles {
    type State = CustomStringState;

    type Alphabet = LowTokenMatchVal;

    type StateSort = StringStateSort;

    type ErrorType = String;

    fn initial_state(&self) -> Self::State {
        todo!()
    }

    fn state_sort_map(&self, state: &Self::State) -> Result<Self::StateSort,Self::ErrorType> {
        todo!()
    }

    fn transition_map(&self, state: &Self::State, character: &Self::Alphabet) -> Result<Self::State, Self::ErrorType> {
        todo!()
    }
}

pub(crate) struct RustStringStyles;

#[derive(Clone, Copy, Debug)]
pub(crate) struct RustStringState {

}

impl DeterministicAutomatonBlueprint for RustStringStyles {
    type State = RustStringState;

    type Alphabet = LowTokenMatchVal;

    type StateSort = StringStateSort;

    type ErrorType = String;

    fn initial_state(&self) -> Self::State {
        todo!()
    }

    fn state_sort_map(&self, state: &Self::State) -> Result<Self::StateSort,Self::ErrorType> {
        todo!()
    }

    fn transition_map(&self, state: &Self::State, character: &Self::Alphabet) -> Result<Self::State, Self::ErrorType> {
        todo!()
    }
}