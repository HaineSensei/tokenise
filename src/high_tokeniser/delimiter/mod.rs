use std::collections::HashMap;

use deterministic_automata::{DeterministicAutomatonBlueprint};

use crate::low_level::{LowTokenMatchVal, LowTokenSort, LowTokenVal};

// String parsing design decision: All strings are assumed to be multi-line.
// No single-line validation layer is implemented to reduce complexity.

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum DelimiterStateSort {
    MaximalAccept,
    InitialAccept,
    Initial,
    Reject
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum StrOrComm {
    Str,
    Comm
}

#[derive(Clone, Debug, PartialEq)]
enum DelimiterStyle {
    Singleton {
        start: Vec<LowTokenMatchVal>,
        end: Vec<LowTokenMatchVal>,
        escapeable: bool, // all escapes will be handled via `\` tokens.
        str_comm: StrOrComm
    },
    Rust {
        style: RustStringStyles
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RustStringStyles {
    starts: HashMap<LowTokenVal, Raw>
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Raw(bool);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RustStringStylesState {
    Reject,
    Initial,
    Hashes(usize, Raw),
    End(usize)
}

impl DeterministicAutomatonBlueprint for RustStringStyles {
    type State = RustStringStylesState;

    type Alphabet = LowTokenMatchVal;

    type StateSort = DelimiterStateSort;

    type ErrorType = String;

    fn initial_state(&self) -> Self::State {
        use RustStringStylesState::*;
        Initial
    }

    fn state_sort_map(&self, state: &Self::State) -> Result<Self::StateSort,Self::ErrorType> {
        use RustStringStylesState::*;
        Ok(match state {
            Reject => DelimiterStateSort::Reject,
            Initial => DelimiterStateSort::Initial,
            Hashes(n,raw) => {
                if *raw == Raw(false) && *n > 0 {
                    return Err("Reached positive Hashes when not raw.".to_string());
                }
                DelimiterStateSort::Initial
            },
            End(_) => DelimiterStateSort::MaximalAccept,
        })
    }

    fn transition_map(&self, state: &Self::State, character: &Self::Alphabet) -> Result<Self::State, Self::ErrorType> {
        use RustStringStylesState::*;
        Ok(match state {
            Reject => Reject,
            Initial => {
                if let Some(ungeneric) = character.ungeneric() {
                    match self.starts.get(&ungeneric) {
                        Some(raw) => {
                            Hashes(0,*raw)
                        },
                        None => {
                            Reject
                        },
                    }
                } else {
                    Reject
                }
            },
            Hashes(n,raw) => {
                match character.ungeneric() {
                    Some(ungeneric) => {
                        if ungeneric == (LowTokenVal {
                                val: String::from("#"),
                                sort: LowTokenSort::Symbol,
                            }) 
                        {
                            if raw.0 {
                                Hashes(n + &1, *raw)
                            } else {
                                Reject
                            }
                        } else if ungeneric == (LowTokenVal {
                            val: String::from(r#"""#),
                            sort: LowTokenSort::Symbol
                        }){
                            End(*n)
                        } else {
                            Reject
                        }
                    },
                    None => todo!(),
                }
            },
            End(_) => Reject,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DelimiterState {
    Rust(RustStringStylesState),
    Other(Option<usize>)
}

impl DeterministicAutomatonBlueprint for DelimiterStyle {
    type State = DelimiterState;

    type Alphabet = LowTokenMatchVal;

    type StateSort = DelimiterStateSort;

    type ErrorType = String;

    fn initial_state(&self) -> Self::State {
        match self {
            DelimiterStyle::Singleton { start: _, end: _, escapeable: _, str_comm:_ } => DelimiterState::Other(Some(0)),
            DelimiterStyle::Rust { style: _ } => DelimiterState::Rust(RustStringStylesState::Initial),
        }
    }

    fn state_sort_map(&self, state: &Self::State) -> Result<Self::StateSort,Self::ErrorType> {
        match self {
            DelimiterStyle::Singleton { start, end: _, escapeable: _, str_comm: _} => {
                match state {
                    DelimiterState::Rust(_rust_string_styles_state) => Err("In Rust state despite in Singleton delimiter.".into()),
                    DelimiterState::Other(x) => match x {
                        Some(x) => if *x == start.len() {
                            Ok(DelimiterStateSort::MaximalAccept)
                        } else {
                            Ok(DelimiterStateSort::Initial)
                        },
                        None => Ok(DelimiterStateSort::Reject),
                    },
                }
            },
            DelimiterStyle::Rust { style } => match state {
                DelimiterState::Rust(rust_string_styles_state) => {
                    style.state_sort_map(rust_string_styles_state)
                },
                DelimiterState::Other(_) => Err("In Other state despite in Rust delimiter.".into()),
            }
        }
    }

    fn transition_map(&self, state: &Self::State, character: &Self::Alphabet) -> Result<Self::State, Self::ErrorType> {
        match (self,state) {
            (DelimiterStyle::Singleton { start: _, end: _, escapeable: _, str_comm: _}, DelimiterState::Rust(_)) => Err("In Rust state despite in Singleton delimiter.".into()),
            (DelimiterStyle::Singleton { start, end: _, escapeable: _, str_comm: _}, DelimiterState::Other(state)) => Ok({
                match state {
                    Some(n) => {
                        if Some(character) == start.get(*n) {
                            DelimiterState::Other(Some(n+1))
                        } else {
                            DelimiterState::Other(None)
                        }
                    },
                    None => DelimiterState::Other(None),
                }
            }),
            (DelimiterStyle::Rust { style }, DelimiterState::Rust(state)) => Ok(DelimiterState::Rust(style.transition_map(state, character)?)),
            (DelimiterStyle::Rust { style:_ }, DelimiterState::Other(_)) => Err("In Other state despite in Rust delimiter.".into()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct DelimiterSpace {
    styles: Vec<DelimiterStyle>
}

// TODO: Implement DelimiterSpace as a MutationAutomaton which uses instances of it's DelimiterStyles' Automata to track releaseable tokens alongside buffer tokens.
