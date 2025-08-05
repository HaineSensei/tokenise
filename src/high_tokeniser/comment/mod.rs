// Comment tokenizing submodule
// TODO: Implement comment-specific automata construction

use deterministic_automata::DeterministicAutomatonBlueprint;

use crate::low_level::LowTokenMatchVal;


// always multi-line by default, end with newline generic LowTokenMatchVal for single-line 
#[derive(Clone, Debug)]
pub(crate) struct CommentStyle {
    start: Vec<LowTokenMatchVal>,
    end: Vec<LowTokenMatchVal>
}

#[derive(Clone, Debug)]
pub(crate) struct CommentStyles {
    val: Vec<CommentStyle>
}

#[derive(Clone, Debug)]
pub(crate) struct CommentState {

}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum CommentStateSort {
    Reject,
    Unfound,
    Found,
    Accept
}

impl DeterministicAutomatonBlueprint for CommentStyles {
    type State = CommentState;

    type Alphabet = LowTokenMatchVal;

    type StateSort = CommentStateSort;

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