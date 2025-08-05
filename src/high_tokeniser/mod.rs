// TODO: Implement unified high-level tokeniser that handles both strings and comments
// This module will combine the functionality of the previous string_tokeniser and comment_tokeniser
// modules to handle proper precedence between string and comment parsing.

pub(crate) mod string;
pub(crate) mod comment;

use deterministic_automata::{either_automaton::Either, DeterministicAutomatonBlueprint};

use crate::{high_tokeniser::{comment::CommentStyles, string::{CustomStringStyles, RustStringStyles}}, low_level::LowTokenMatchVal};


struct StringCommentStyles {
    string_styles: Either<string::CustomStringStyles,string::RustStringStyles>,
    comment_styles: CommentStyles
}

#[derive(Clone, Debug)]
struct HighLevelState {
    string_state: Option<<Either<CustomStringStyles,RustStringStyles> as DeterministicAutomatonBlueprint>::State>,
    comment_state: Option<<CommentStyles as DeterministicAutomatonBlueprint>::State>
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StringOrComment {
    String,
    Comment
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum HighStateSort {
    Reject,
    Unfound(Option<StringOrComment>),
    Found(Option<StringOrComment>),
    Accept(StringOrComment)
}

impl DeterministicAutomatonBlueprint for StringCommentStyles {
    type State = HighLevelState;

    type Alphabet = LowTokenMatchVal;

    type StateSort = HighStateSort;

    type ErrorType = String;

    fn initial_state(&self) -> Self::State {
        HighLevelState {
            string_state: Some(self.string_styles.initial_state()),
            comment_state: Some(self.comment_styles.initial_state())
        }
    }
    
    //will assume that string starting delimiters do not contain full comments and vice versa.
    fn state_sort_map(&self, state: &Self::State) -> Result<Self::StateSort,Self::ErrorType> {
        match (&state.string_state, &state.comment_state) {
            (None, None) => Err("No state found".to_string()),
            (None, Some(x)) => Ok(match self.comment_styles.state_sort_map(x)? {
                comment::CommentStateSort::Reject => HighStateSort::Reject,
                comment::CommentStateSort::Unfound => HighStateSort::Unfound(Some(StringOrComment::Comment)),
                comment::CommentStateSort::Found => HighStateSort::Found(Some(StringOrComment::Comment)),
                comment::CommentStateSort::Accept => HighStateSort::Accept(StringOrComment::Comment),
            }),
            (Some(x), None) => Ok(match self.string_styles.state_sort_map(x)? {
                string::StringStateSort::Reject => HighStateSort::Reject,
                string::StringStateSort::Unfound => HighStateSort::Unfound(Some(StringOrComment::String)),
                string::StringStateSort::Found => HighStateSort::Found(Some(StringOrComment::String)),
                string::StringStateSort::Accept => HighStateSort::Accept(StringOrComment::String),
            }),
            // some of these things may need to be changed to Err. Since you shouldn't be able to reach a state pair of accepts for example. 
            (Some(x), Some(y)) => match (self.string_styles.state_sort_map(x)?, self.comment_styles.state_sort_map(y)?) {
                (string::StringStateSort::Reject, comment::CommentStateSort::Reject) => Ok(HighStateSort::Reject),
                (string::StringStateSort::Reject, comment::CommentStateSort::Unfound) => Ok(HighStateSort::Unfound(Some(StringOrComment::Comment))),
                (string::StringStateSort::Reject, comment::CommentStateSort::Found) => Ok(HighStateSort::Found(Some(StringOrComment::Comment))),
                (string::StringStateSort::Reject, comment::CommentStateSort::Accept) => Ok(HighStateSort::Accept(StringOrComment::Comment)),
                (string::StringStateSort::Unfound, comment::CommentStateSort::Reject) => Ok(HighStateSort::Unfound(Some(StringOrComment::String))),
                (string::StringStateSort::Unfound, comment::CommentStateSort::Unfound) => Ok(HighStateSort::Unfound(None)),
                (string::StringStateSort::Unfound, comment::CommentStateSort::Found) => Ok(HighStateSort::Found(None)),
                (string::StringStateSort::Unfound, comment::CommentStateSort::Accept) => Ok(HighStateSort::Accept(StringOrComment::Comment)),
                (string::StringStateSort::Found, comment::CommentStateSort::Reject) => Ok(HighStateSort::Found(Some(StringOrComment::String))),
                (string::StringStateSort::Found, comment::CommentStateSort::Unfound) => Ok(HighStateSort::Found(None)),
                (string::StringStateSort::Found, comment::CommentStateSort::Found) => Ok(HighStateSort::Found(None)),
                (string::StringStateSort::Found, comment::CommentStateSort::Accept) => Ok(HighStateSort::Accept(StringOrComment::Comment)),
                (string::StringStateSort::Accept, comment::CommentStateSort::Reject) => Ok(HighStateSort::Accept(StringOrComment::String)),
                (string::StringStateSort::Accept, comment::CommentStateSort::Unfound) => Ok(HighStateSort::Accept(StringOrComment::String)),
                (string::StringStateSort::Accept, comment::CommentStateSort::Found) => Ok(HighStateSort::Accept(StringOrComment::String)),
                (string::StringStateSort::Accept, comment::CommentStateSort::Accept) => Ok(HighStateSort::Accept(StringOrComment::String)),
            },
        }
    }

    fn transition_map(&self, state: &Self::State, character: &Self::Alphabet) -> Result<Self::State, Self::ErrorType> {
        todo!()
    }
}
