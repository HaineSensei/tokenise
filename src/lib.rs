//! # Tokenise
//! 
//! A flexible lexical analyser (tokeniser) for parsing text into configurable token types.
//! 
//! `tokenise` allows you to split text into tokens based on customisable rules for special characters,
//! delimiters, and comments. It's designed to be flexible enough to handle various syntax styles
//! while remaining simple to configure.
//! 
//! This version uses a two-phase approach: primitive tokenization followed by contextual parsing
//! with proper precedence handling (strings > comments > normal parsing).
//! 
//! ## Basic Usage
//! 
//! The following example demonstrates how to configure a tokeniser with common syntax elements
//! and process a simple code snippet:
//! 
//! ```
//! use tokenise::Tokeniser;
//! 
//! fn main() {
//!     // Create a new tokeniser
//!     let mut tokeniser = Tokeniser::new();
//!     
//!     // Configure tokeniser with rules
//!     tokeniser.add_specials(".,;:!?");
//!     tokeniser.add_delimiter_pairs(&vec!["()", "[]", "{}"]).unwrap();
//!     tokeniser.add_balanced_delimiter("\"").unwrap();
//!     tokeniser.set_sl_comment("//").unwrap();
//!     tokeniser.set_ml_comment("/*", "*/").unwrap();
//!     
//!     // Tokenise some source text
//!     let source = "let x = 42; // The answer\nprint(\"Hello world!\");";
//!     let tokens = tokeniser.tokenise(source).unwrap();
//!     
//!     // Work with the resulting tokens
//!     for token in tokens {
//!         println!("{:?}: '{}'", token.get_state(), token.value());
//!     }
//! }
//! ```
//! 
//! ## Features
//! 
//! - Unicode support (using grapheme clusters)
//! - Configurable special characters and delimiters
//! - Support for paired delimiters (e.g., parentheses, brackets)
//! - Support for balanced delimiters (e.g., quotation marks)
//! - Single-line and multi-line comment handling
//! - Whitespace and newline preservation
//! - Proper precedence handling (strings override comments override normal parsing)

/*
TODO: Implementation Plan

DESIGN OVERVIEW:
- Two-phase tokenization: PrimitiveTokenizer -> StateMachine
- PrimitiveTokenizer: Iterator<Item=PrimitiveToken> that produces:
  * Word: alphanumeric sequences
  * Symbol: individual special characters
  * WhiteSpace: whitespace sequences (excluding newlines)
  * NewLine: newline characters
- StateMachine: processes primitives with precedence rules:
  1. String context (highest precedence) - once inside, ignore everything else until close
  2. Comment context (second precedence) - ignore delimiters but respect string boundaries  
  3. Normal parsing - handle delimiters, group symbols as needed

PRECEDENCE RULES:
- Multiple string delimiters: first one encountered takes precedence
- Comments cannot start inside strings
- Delimiters inside strings/comments are treated as literal text
- Individual symbols rather than symbol strings for flexibility

API COMPATIBILITY:
- Keep same configuration methods (add_specials, add_delimiter_pairs, etc.)
- Keep same tokenise() method signature
- Keep same Token structure with get_state() and value() methods
- But fix the fundamental precedence issues

IMPLEMENTATION STEPS:
1. Implement PrimitiveTokenizer iterator
2. Define TokenState enum and Token struct 
3. Implement Tokeniser configuration methods
4. Implement StateMachine iterator with proper precedence
5. Add tests for edge cases (comments in strings, etc.)
*/

use std::marker::PhantomData;

use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};
use unicode_general_category::{get_general_category, GeneralCategory};

// TODO: Implement the actual tokenizer structure and logic

#[derive(Clone)]
struct NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, T, G, F>
where
    S: State,
    T: TokenVal,
    G: Fn(&S) -> Result<StateSort,String>,
    F: Fn(&S, &T) -> S
{
    initial_state: S,
    inner_state_sort_map: G,
    inner_transition_map: F,
    _phantom: PhantomData<T>
}

trait TokenVal: Eq {}

impl TokenVal for LowTokenVal {}

impl<S, T, G, F> NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, T, G, F> 
where
    S: State,
    T: TokenVal,
    G: Fn(&S) -> Result<StateSort,String>,
    F: Fn(&S, &T) -> S
{
    fn transition_map(&self, state: &S, token: &T) -> S {
        (self.inner_transition_map)(state, token)
    }

    fn state_sort_map(&self, state: &S) -> Result<StateSort, String> {
        (self.inner_state_sort_map)(state)
    }

    fn new_automaton(&self) -> NotNecessarilyFiniteStateDeterministicAutomaton<S, T, G, F> {
        NotNecessarilyFiniteStateDeterministicAutomaton::new(self)
    }
}

struct NotNecessarilyFiniteStateDeterministicAutomaton<'a, S, T, G, F>
where
    S: State,
    T: TokenVal,
    G: Fn(&S) -> Result<StateSort,String>,
    F: Fn(&S, &T) -> S
{
    pub blueprint: &'a NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, T, G, F>,
    pub curr_state: S
}

impl<'a, S, T, G, F> NotNecessarilyFiniteStateDeterministicAutomaton<'a, S, T, G, F> 
where 
    S: State,
    T: TokenVal,
    G: Fn(&S) -> Result<StateSort,String>,
    F: Fn(&S, &T) -> S
{
    fn update_state(&mut self, token: &T) -> Result<StateSort, String> {
        self.curr_state = self.blueprint.transition_map(&self.curr_state, token);
        self.blueprint.state_sort_map(&self.curr_state)
    }

    fn new(blueprint: &'a NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, T, G, F>) -> Self {
        Self {blueprint, curr_state: blueprint.initial_state.clone() }
    }
}

trait State: Eq + Clone {}

impl State for RustState {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct RustState {
    name: String,
    index: usize
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum StateSort {
    Accepting,
    Rejecting,
    ImmediateRejecting,
}

#[derive(Clone, Debug)]
enum StringStyle {
    Rust,
    Custom(Vec<Vec<LowTokenVal>>)
}

pub struct Tokeniser {
    low_tokeniser: LowTokeniser,
    // high stuff
    string_style: Option<StringStyle>
}

impl Tokeniser {
    pub fn new(currency_are_symbols: bool, other_symbols_are_symbols:bool) -> Self {
        Tokeniser {
            low_tokeniser: LowTokeniser::new(currency_are_symbols, other_symbols_are_symbols),
            string_style: None
        }
    }

    pub fn change_to_rust_string_style(&mut self) -> Result<(), String> {
        match &self.string_style {
            Some(StringStyle::Rust) => Err("String style already rust cannot change to rust.".to_string()),
            _ => {
                self.string_style = Some(StringStyle::Rust);
                Ok(())
            },
        }
    }

    pub fn add_exception_symbol(&mut self, c: &str) -> Result<(), String> {
        self.low_tokeniser.add_exception_symbol(c)
    }

    pub fn add_exception_alphanum(&mut self, c: &str) -> Result<(), String> {
        self.low_tokeniser.add_exception_alphanum(c)
    }
}

#[derive(Clone, Debug)]
struct LowTokeniser {
    symbols_base: Vec<GeneralCategory>,
    symbols_exceptions: Vec<String>,
    alpha_exceptions: Vec<String>
}

fn is_newline(chr: char) -> bool {
    match chr {
        '\n' => true,
        '\r' => true,
        _ => false
    }
}

impl LowTokeniser {
    fn new(currency_are_symbols: bool, other_symbols_are_symbols: bool) -> Self {
        let mut symbols_base = Vec::new();
        symbols_base.push(GeneralCategory::ClosePunctuation);
        symbols_base.push(GeneralCategory::DashPunctuation);
        symbols_base.push(GeneralCategory::FinalPunctuation);
        symbols_base.push(GeneralCategory::InitialPunctuation);
        symbols_base.push(GeneralCategory::MathSymbol);
        symbols_base.push(GeneralCategory::ModifierSymbol);
        symbols_base.push(GeneralCategory::OpenPunctuation);
        symbols_base.push(GeneralCategory::OtherPunctuation);
        if other_symbols_are_symbols {
            symbols_base.push(GeneralCategory::OtherSymbol);
        }
        if currency_are_symbols {
            symbols_base.push(GeneralCategory::CurrencySymbol);
        }

        LowTokeniser {
            symbols_base,
            symbols_exceptions: Default::default(),
            alpha_exceptions: Default::default(),
        }
    }

    fn add_exception_symbol(&mut self, c: &str) -> Result<(), String> {
        if c.graphemes(true).count() != 1 {
            Err("Cannot add a string that isn't a single grapheme as a symbol.".to_string())
        } else {
            match self.token_sort(c).unwrap() {
                LowTokenSort::AlphaNum => {
                    self.symbols_exceptions.push(c.to_string());
                    Ok(())
                },
                LowTokenSort::WhiteSpace => Err("Cannot register whitespace as a symbol. I'm not tokenising a bad language.".to_string()),
                LowTokenSort::NewLine => Err("Cannot register a new line as a symbol. I'm not tokenising a bad language.".to_string()),
                LowTokenSort::Symbol => Err("Cannot register something that is already a symbol as a symbol exception.".to_string()),
            }
        }
    }

    fn add_exception_alphanum(&mut self, c: &str) -> Result<(), String> {
        if c.graphemes(true).count() != 1 {
            Err("Cannot add a string that isn't a single grapheme as a symbol.".to_string())
        } else {
            match self.token_sort(c).unwrap() {
                LowTokenSort::AlphaNum => Err("Cannot register something that is already alphanumeric as an alphanumeric exception.".to_string()),
                LowTokenSort::WhiteSpace => Err("Cannot register whitespace as alphanumeric. I'm not tokenising a bad language.".to_string()),
                LowTokenSort::NewLine => Err("Cannot register a new line as a alphanumeric. I'm not tokenising a bad language.".to_string()),
                LowTokenSort::Symbol => {
                    self.alpha_exceptions.push(c.to_string());
                    Ok(())
                },
            }
        }
    }

    fn tokenise<'a, 'b>(&'b self, source: &'a str) -> LowTokenIter<'a, 'b> {
        LowTokenIter::new(source, self)
    }

    fn token_sort(&self, c: &str) -> Option<LowTokenSort> {
        let first = c.chars().next()?;
        if self.symbols_exceptions.contains(&c.to_string()) {
            Some(LowTokenSort::Symbol)
        } else if self.alpha_exceptions.contains(&c.to_string()) {
            Some(LowTokenSort::AlphaNum)
        } else if is_newline(first) {
            Some(LowTokenSort::NewLine)
        } else if first.is_whitespace() {
            Some(LowTokenSort::WhiteSpace)
        } else if self.symbols_base.contains(&get_general_category(first)) {
            Some(LowTokenSort::Symbol)
        } else {
            Some(LowTokenSort::AlphaNum)
        }
    }
}

impl Default for LowTokeniser {
    fn default() -> Self {
        Self::new(true,false)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum LowTokenSort {
    AlphaNum,
    WhiteSpace,
    NewLine,
    Symbol
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct LowTokenVal {
    pub val: String,
    pub sort: LowTokenSort
}

#[derive(Clone, Copy, Debug)]
struct LowToken<'a> {
    pub val: &'a str,
    pub initial_index: usize,
    pub sort: LowTokenSort
}

impl<'a> LowToken<'a> {
    fn as_val(&self) -> LowTokenVal {
        LowTokenVal::new(self.val, self.sort)
    }
}

impl LowTokenVal {
    fn new(val: &str, sort: LowTokenSort) -> Self {
        Self { val: val.into(), sort }
    }

    fn matches(&self, other: LowToken) -> bool {
        self.val == other.val && self.sort == other.sort
    }
}

struct LowTokenIter<'a, 'b> {
    source: &'a str,
    source_iter: GraphemeIndices<'a>,
    curr: Option<(usize, &'a str)>,
    low_tokeniser: &'b LowTokeniser,
}

impl<'a, 'b> LowTokenIter<'a, 'b> {
    fn new(source: &'a str, low_tokeniser: &'b LowTokeniser) -> Self {
        let source_iter = source.grapheme_indices(true);
        Self {
            source,
            source_iter,
            low_tokeniser,
            curr: None,
        }
    }
}

impl<'a,'b> Iterator for LowTokenIter<'a, 'b> {
    type Item = LowToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut out = None;
        loop {
            let (index, c);
            match self.curr {
                Some(x) => {
                    (index, c) = x;
                    self.curr = None;
                },
                None => {
                    (index, c) = match self.source_iter.next(){
                        Some(x) => x,
                        None => {
                            return out;
                        },
                    };
                },
            }
            if let Some(x) = &mut out {
                let sort = self.low_tokeniser.token_sort(c).unwrap();
                if sort == x.sort && sort != LowTokenSort::Symbol{
                    x.val = &self.source[
                        x.initial_index
                        ..
                        x.initial_index + x.val.len() + c.len()
                    ];
                } else {
                    self.curr = Some((index, c));
                    return out
                };
            } else {
                out = Some(
                    LowToken {
                        val: c,
                        initial_index: index,
                        sort: self.low_tokeniser.token_sort(c)?
                    }
                )
            }
        }
    }
}


#[cfg(test)]
mod tests;
