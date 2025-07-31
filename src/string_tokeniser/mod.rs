use crate::{automata::{NotNecessarilyFiniteStateDeterministicAutomaton, NotNecessarilyFiniteStateDeterministicAutomatonBlueprint, State, StateSort, TokenVal}, low_level::{LowToken, LowTokenIter, LowTokenVal, LowTokeniser}};

#[derive(Clone, Debug)]
pub(crate) struct StringStructure {
    start: Vec<LowTokenVal>,
    end: Vec<LowTokenVal>,
    multiline: bool,
    escapable: bool
}

#[derive(Clone, Debug)]
pub(crate) enum StringStyle {
    Rust,
    Custom(Vec<StringStructure>)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum StringTokenSort {
    AlphaNum,
    Symbol,
    NewLine,
    WhiteSpace,
    StringLiteral
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct StringTokenVal {
    pub(crate) val: String,
    pub(crate) sort: StringTokenSort
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct StringToken<'a> {
    pub(crate) val: &'a str,
    pub(crate) initial_index: usize,
    pub(crate) sort: StringTokenSort
}

impl StringTokenVal {
    pub(crate) fn new(val: &str, sort: StringTokenSort) -> Self {
        Self { val: val.into(), sort }
    }

    pub(crate) fn matches(&self, other: StringToken) -> bool {
        self.val == other.val && self.sort == other.sort
    }
}

impl<'a> StringToken<'a> {
    pub(crate) fn as_val(&self) -> StringTokenVal {
        StringTokenVal::new(self.val, self.sort)
    }
}

// TODO: Add StringTokeniser implementation here

// TODO: implement this as an enum with appropriately handled G, F so that S can be handled case by case.
// I think G and F have to be Box(dyn)'ed in the custom case since we're producing them somewhat dynamically from 
// the custom data.
pub(crate) struct StringTokeniser<S, G, F> 
where
    S: State,
    G: Fn(&S) -> Result<StateSort, String>,
    F: Fn(&S, &LowTokenVal) -> S,
{
    low_tokeniser: LowTokeniser,
    string_style: Option<StringStyle>,
    string_automaton_blueprint: Option<NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, LowTokenVal, G, F>>
}

impl<S, G, F> StringTokeniser<S, G, F>
where 
    S: State,
    G: Fn(&S) -> Result<StateSort, String>,
    F: Fn(&S, &LowTokenVal) -> S,
{
    pub(crate) fn new(currency_are_symbols: bool, other_symbols_are_symbols: bool, string_style: Option<StringStyle>) -> Self {
        Self {
            low_tokeniser: LowTokeniser::new(currency_are_symbols, other_symbols_are_symbols),
            string_style,
            string_automaton_blueprint: None
        }
    }

    pub(crate) fn tokenise<'a, 'b>(&'b self, source: &'a str) -> StringTokenIter<'a, 'b, S, G, F> {
        StringTokenIter::new(source, self)
    }
}

pub(crate) struct StringTokenIter<'a, 'b, S, G, F> 
where 
    S: State,
    G: Fn(&S) -> Result<StateSort, String>,
    F: Fn(&S, &LowTokenVal) -> S
{
    source: &'a str,
    iter: LowTokenIter<'a, 'b>,
    string_tokeniser: &'b StringTokeniser<S, G, F>,
    backlog: Vec<LowToken<'a>>,
    automata: Vec<NotNecessarilyFiniteStateDeterministicAutomaton<'b, S, LowTokenVal, G, F>>
}

impl<'a, 'b, S, G, F> StringTokenIter<'a, 'b, S, G, F> 
where 
    S: State,
    G: Fn(&S) -> Result<StateSort, String>,
    F: Fn(&S, &LowTokenVal) -> S
{
    pub(crate) fn new(source: &'a str, string_tokeniser: &'b StringTokeniser<S, G, F>) -> Self {
        Self {
            source,
            iter: string_tokeniser.low_tokeniser.tokenise(source),
            string_tokeniser,
            backlog: Vec::new(),
            automata: Vec::new(),
        }
    }
}

impl<'a, 'b, S, G, F> Iterator for StringTokenIter<'a, 'b, S, G, F>
where
    S: State,
    G: Fn(&S) -> Result<StateSort, String>,
    F: Fn(&S, &LowTokenVal) -> S,
{
    type Item = StringToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

