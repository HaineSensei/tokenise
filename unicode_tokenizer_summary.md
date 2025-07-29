# Unicode Categories for Tokenizer Design

## Overview

Discussion about using Unicode General Categories to differentiate symbols from alphanumeric characters for tokenizer design, with focus on what should be treated as operators vs identifiers vs whitespace.

## Unicode General Categories Recap

Unicode assigns each code point to categories like:

**Alphanumeric:**
- **L*** (Letters): Lu (uppercase), Ll (lowercase), Lt (titlecase), Lm (modifier), Lo (other letters like Chinese)
- **N*** (Numbers): Nd (decimal digits), Nl (letter numbers), No (other numbers)

**Symbols:**
- **S*** (Symbols): Sm (math), Sc (currency), Sk (modifier), So (other)

**Other:**
- **P*** (Punctuation), **M*** (Marks), **Z*** (Separators), **C*** (Control/Other)

## Initial Tokenizer Requirements

Started with 3-category classification:
- **Alphanumeric** - allowed in identifiers
- **Whitespace** - separators  
- **Symbols** - potential operators (user must explicitly opt-in)

## Key Findings

### The `unicode_segmentation` Crate Limitation
- `unicode_segmentation` provides grapheme clustering but **not** category checking
- Need separate crate like `unicode-general-category` for category queries

### Scripts Without Case Distinction
- Languages like Chinese, Arabic, Hebrew use **OtherLetter (Lo)** category
- Unicode handles this perfectly - Lo gets classified as alphanumeric
- Much better than ASCII-only approaches

### Character Category Examples
Breakdown of common programming symbols:

- **MathSymbol**: `+ = | ~ < >` (plus mathematical symbols like `∀ ∃ ∪ ∩ ≡ ≅`)
- **OtherPunctuation**: `# % * \ • . , ? ! ' & @ "`
- **CurrencySymbol**: `€ $ ¥ £`  
- **ConnectorPunctuation**: `_` (typically for identifiers)
- **ModifierSymbol**: `^` (plus diacritical marks like `¨ ¯ ´ ¸`)

### Important Distinction: Letters vs Math Symbols
- **Σ** (U+03A3) = Greek letter sigma (`UppercaseLetter`)
- **∑** (U+2211) = N-ary summation symbol (`MathSymbol`)
- Same visual appearance, different semantic purposes

## Final Recommendation

### Default Symbol Categories
```rust
MathSymbol + OtherPunctuation + ModifierSymbol + CurrencySymbol
```

### Key Insight: Grapheme-Based Detection
Whether a character functions as operator vs identifier part depends on **position in grapheme**:
- **First character of grapheme** → available as operator
- **Combining character within grapheme** → part of identifier

Examples:
- `^e` → two graphemes: `["^", "e"]` (^ available as operator)
- `ê` → one grapheme: `["ê"]` (part of identifier)

### Configurable System
```toml
[symbols]
include_categories = ["MathSymbol", "OtherPunctuation", "ModifierSymbol", "CurrencySymbol"]
exclude_categories = ["CurrencySymbol"]  # For financial DSLs wanting $price variables
exclude_chars = [".", ",", "?", "!"]     # Remove sentence punctuation
include_chars = ["^"]                    # Add specific characters
```

## Rationale for Defaults

**Include CurrencySymbol by default:**
- Maximally permissive for general-purpose tokenizer
- Supports LaTeX-style (`$...$`), template (`${}`), shell (`$var`) syntaxes
- Easy to exclude for financial DSLs that want `$price` identifiers
- Aligns with most programming languages treating `$` as special

**Benefits of this approach:**
- Semantic categorization (math symbols designed for mathematical operations)
- International script support (Chinese, Arabic, etc.)
- Handles combining characters correctly via grapheme clustering
- Configurable for domain-specific needs
- Thousands of mathematical symbols available (`∀∃∈∉∅∞∫∑∏√∧∨¬→←↔`)

## Implementation Notes

- Use `unicode_general_category::get_general_category()` for category checking
- Use `unicode_segmentation::UnicodeSegmentation::graphemes()` for clustering
- Check category of first character in each grapheme to determine operator eligibility
- Allow configuration via category inclusion/exclusion plus individual character overrides