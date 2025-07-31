use crate::{automata::{NotNecessarilyFiniteStateDeterministicAutomaton, NotNecessarilyFiniteStateDeterministicAutomatonBlueprint, State, StateSort}, low_level::LowTokenVal, string_tokeniser::{StringStyle, StringToken, StringTokenIter, StringTokenVal, StringTokeniser}};

// Oh how I look forward to having Comments that are bordered by specific strings xdd
pub(crate) struct CommentStructure {
    start: Vec<StringTokenVal>,
    end: Vec<StringTokenVal>
}

pub(crate) struct CommentStyle(Vec<CommentStructure>);

impl CommentStyle {
    pub(crate) fn new() -> Self {
        CommentStyle(Vec::new())
    }

    pub(crate) fn automaton<G, F>(&self) -> NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<CommentState, StringTokenVal, G, F>
    where 
        G: Fn(&CommentState) -> Result<StateSort, String>,
        F: Fn(&CommentState, &StringTokenVal) -> CommentState,
    {
        todo!()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum CommentTokenSort {
    AlphaNum,
    Symbol,
    Newline,
    Whitespace,
    StringLiteral,
    Comment,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CommentTokenVal {
    val: String,
    sort: CommentTokenSort
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CommentToken<'a> {
    val: &'a str,
    initial_index: usize,
    sort: CommentTokenSort
}

impl CommentTokenVal {
    pub(crate) fn new(val: &str, sort: CommentTokenSort) -> Self {
        Self { val: val.into(), sort }
    }

    pub(crate) fn matches(&self, other: CommentToken) -> bool {
        self.val == other.val && self.sort == other.sort
    }
}

impl<'a> CommentToken<'a> {
    pub(crate) fn as_val(&self) -> CommentTokenVal {
        CommentTokenVal::new(self.val, self.sort)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct CommentState {
    val: String
}

impl State for CommentState {}

pub(crate) struct CommentTokeniser<S, G1, F1, G2, F2> 
where
    S: State,
    G1: Fn(&S) -> Result<StateSort, String>,
    F1: Fn(&S, &LowTokenVal) -> S,
    G2: Fn(&CommentState) -> Result<StateSort, String>,
    F2: Fn(&CommentState, &StringTokenVal) -> CommentState
{
    string_tokeniser: StringTokeniser<S, G1, F1>,
    comment_style: CommentStyle,
    comment_automaton_blueprint: Option<NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<CommentState, StringTokenVal, G2, F2>>
}

impl<S, G1, F1, G2, F2> CommentTokeniser<S, G1, F1, G2, F2>
where 
    S: State,
    G1: Fn(&S) -> Result<StateSort, String>,
    F1: Fn(&S, &LowTokenVal) -> S,
    G2: Fn(&CommentState) -> Result<StateSort, String>,
    F2: Fn(&CommentState, &StringTokenVal) -> CommentState
{
    fn new(currency_are_symbols: bool, other_symbols_are_symbols: bool, string_style: Option<StringStyle>, comment_style: CommentStyle) -> Self {
        Self {
            string_tokeniser: StringTokeniser::new(currency_are_symbols, other_symbols_are_symbols, string_style),
            comment_style,
            comment_automaton_blueprint: None,
        }
    }

    fn tokenise<'a, 'b>(&'b self, source: &'a str) -> CommentTokenIter<'a, 'b, S, G1, F1, G2, F2> {
        CommentTokenIter::new(source, self)
    }
}

pub(crate) struct CommentTokenIter<'a, 'b, S, G1, F1, G2, F2> 
where 
    S: State,
    G1: Fn(&S) -> Result<StateSort, String>,
    F1: Fn(&S, &LowTokenVal) -> S,
    G2: Fn(&CommentState) -> Result<StateSort, String>,
    F2: Fn(&CommentState, &StringTokenVal) -> CommentState
{
    source: &'a str,
    iter: StringTokenIter<'a, 'b, S, G1, F1>,
    comment_tokeniser: &'b CommentTokeniser<S, G1, F1, G2, F2>,
    backlog: Vec<StringToken<'a>>,
    automata: Vec<NotNecessarilyFiniteStateDeterministicAutomaton<'b, CommentState, StringTokenVal, G2, F2>>
}

impl<'a, 'b, S, G1, F1, G2, F2>  CommentTokenIter<'a, 'b, S, G1, F1, G2, F2> 
where 
    S: State,
    G1: Fn(&S) -> Result<StateSort, String>,
    F1: Fn(&S, &LowTokenVal) -> S,
    G2: Fn(&CommentState) -> Result<StateSort, String>,
    F2: Fn(&CommentState, &StringTokenVal) -> CommentState
{
    pub(crate) fn new(source: &'a str, comment_tokeniser: &'b CommentTokeniser<S, G1, F1, G2, F2>) -> Self {
        Self {
            source,
            iter: comment_tokeniser.string_tokeniser.tokenise(source),
            comment_tokeniser,
            backlog: Vec::new(),
            automata: Vec::new(),
        }
    }
}

impl<'a, 'b, S, G1, F1, G2, F2> Iterator for CommentTokenIter<'a, 'b, S, G1, F1, G2, F2>
where
    S: State,
    G1: Fn(&S) -> Result<StateSort, String>,
    F1: Fn(&S, &LowTokenVal) -> S,
    G2: Fn(&CommentState) -> Result<StateSort, String>,
    F2: Fn(&CommentState, &StringTokenVal) -> CommentState
{
    type Item = CommentToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

