# Tokenise

A flexible lexical analyser (tokeniser) for parsing text into configurable token types.

[![Crates.io](https://img.shields.io/crates/v/tokenise.svg)](https://crates.io/crates/tokenise)
[![Documentation](https://docs.rs/tokenise/badge.svg)](https://docs.rs/tokenise)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

`tokenise` allows you to split text into tokens based on customisable rules for special characters, delimiters, and comments. It's designed to be flexible enough to handle various syntax styles while remaining simple to configure.

## Features

- Unicode support (using grapheme clusters)
- Configurable special characters and delimiters
- Support for paired delimiters (e.g., parentheses, brackets)
- Support for balanced delimiters (e.g., quotation marks)
- Single-line and multi-line comment handling
- Whitespace and newline preservation

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
tokenise = "0.1.0"
```

### Basic Example

```rust
use tokenise::{Tokeniser, TokenState};

fn main() {
    // Create a new tokeniser
    let mut tokeniser = Tokeniser::new();
    
    // Configure tokeniser with rules
    tokeniser.add_specials(".,;:!?");
    tokeniser.add_delimiter_pairs(&vec!["()", "[]", "{}"]).unwrap();
    tokeniser.add_balanced_delimiter("\"").unwrap();
    tokeniser.set_sl_comment("//").unwrap();
    tokeniser.set_ml_comment("/*", "*/").unwrap();
    
    // Tokenise some source text
    let source = "let x = 42; // The answer\nprint(\"Hello world!\");";
    let tokens = tokeniser.tokenise(source).unwrap();
    
    // Work with the resulting tokens
    for token in tokens {
        println!("{:?}: '{}'", token.get_state(), token.value());
    }
}
```

## Token Types

The tokeniser recognises several token types represented by the `TokenState` enum:

- `Word`: Non-special character sequences
- `LDelimiter`/`RDelimiter`: Left/right delimiters of a pair (e.g., '(', ')')
- `BDelimiter`: Balanced delimiters (e.g., quotation marks)
- `SymbolString`: Special characters
- `NewLine`: Line breaks
- `WhiteSpace`: Spaces, tabs, etc.
- `SLComment`: Single-line comments
- `MLComment`: Multi-line comments

## License

This project is licensed under the MIT License - see the LICENSE file for details.
