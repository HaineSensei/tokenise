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

mod low_level;
mod high_tokeniser;

// Internal modules - not exported publicly
use low_level::LowTokeniser;

// TODO: Implement the actual tokenizer structure and logic

pub struct Tokeniser {
    low_tokeniser: LowTokeniser,
    // TODO: Add high_tokeniser field once implemented
}

impl Tokeniser {
    pub fn new(currency_are_symbols: bool, other_symbols_are_symbols:bool) -> Self {
        Tokeniser {
            low_tokeniser: LowTokeniser::new(currency_are_symbols, other_symbols_are_symbols),
        }
    }

    // TODO: Re-implement string and comment configuration methods once high_tokeniser is ready

    pub fn add_exception_symbol(&mut self, c: &str) -> Result<(), String> {
        self.low_tokeniser.add_exception_symbol(c)
    }

    pub fn add_exception_alphanum(&mut self, c: &str) -> Result<(), String> {
        self.low_tokeniser.add_exception_alphanum(c)
    }
}

#[cfg(test)]
mod tests;