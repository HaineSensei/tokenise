//! # Tokenise
//! 
//! A flexible lexical analyser (tokeniser) for parsing text into configurable token types.
//! 
//! `tokenise` allows you to split text into tokens based on customisable rules for special characters,
//! delimiters, and comments. It's designed to be flexible enough to handle various syntax styles
//! while remaining simple to configure.
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
//! 
//! ## Token Types
//! 
//! The tokeniser recognises several token types represented by the `TokenState` enum:
//! 
//! - `Word`: Non-special character sequences (anything not identified as a special character or whitespace)
//! - `LDelimiter`/`RDelimiter`: Left/right delimiters of a pair (e.g., '(', ')')
//! - `BDelimiter`: Balanced delimiters (e.g., quotation marks)
//! - `SymbolString`: Special characters
//! - `NewLine`: Line breaks
//! - `WhiteSpace`: Spaces, tabs, etc.
//! - `SLComment`: Single-line comments
//! - `MLComment`: Multi-line comments
//! 
//! More precise definitions can be found in the documentation for each specific type.

use unicode_segmentation::UnicodeSegmentation;

// TODO: add multi-character Parenthesis
/// Represents the type of a token in the tokenisation process.
///
/// Each token in the parsed text is classified as one of these types,
/// which determines how it is interpreted and processed.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenState {
    /// A sequence of non-special characters (excluding whitespace).
    Word,
    
    /// A left delimiter of a pair (e.g., opening bracket).
    LDelimiter,
    
    /// A right delimiter of a pair (e.g., closing bracket).
    RDelimiter,
    
    /// A balanced delimiter that can open or close (e.g., quotation mark).
    BDelimiter,
    
    /// A sequence of special characters not recognized as delimiters or comments.
    SymbolString,
    
    /// A newline character sequence (\n, \r, or \r\n).
    NewLine,
    
    /// A sequence of whitespace characters (excluding newlines).
    WhiteSpace,
    
    /// A single-line comment.
    SLComment,
    
    /// A multi-line comment.
    MLComment
}
use TokenState::*;

/// Represents the categorisation of delimiters into left, right, or balanced types.
///
/// This is used to classify delimiters when tokenising text:
/// - Left delimiters open a section (e.g., opening brackets)
/// - Right delimiters close a section (e.g., closing brackets)
/// - Balanced delimiters can serve as both opening and closing (e.g., quotation marks)
#[derive(Debug, PartialEq, Eq)]
pub enum Side {
    /// A right (closing) delimiter such as ')', ']', or '}'.
    Right,
    
    /// A left (opening) delimiter such as '(', '[', or '{'.
    Left,
    
    /// A balanced delimiter that can both open and close, such as '"'.
    Bal
}

/// Checks if a string is exactly one grapheme cluster (user-perceived character).
///
/// # Examples
/// ```
/// assert!(tokenise::is_grapheme("a"));
/// assert!(tokenise::is_grapheme("👨‍💻"));
/// assert!(tokenise::is_grapheme("\r\n"));
/// assert!(!tokenise::is_grapheme("ab"));
/// ```
pub fn is_grapheme(s: &str) -> bool {
    s.graphemes(true).collect::<Vec<_>>().len() == 1
}

/// Checks if a string consists entirely of whitespace.
///
/// # Examples
/// ```
/// assert!(tokenise::is_whitespace(" \t"));
/// assert!(!tokenise::is_whitespace("a "));
/// ```
pub fn is_whitespace(c: &str) -> bool {
    c.trim().is_empty()
}

/// Represents a token extracted from the source text during tokenisation.
///
/// Each token has a state (type), a string value, and a position in the original text.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token<'a> {
    /// The type of this token.
    state: TokenState,
    
    /// The string content of this token.
    val: &'a str,
    
    /// The starting position (in characters) of this token in the original text.
    start_pos: usize
}

impl<'a> Token<'a> {
    /// Returns the starting position of this token in the original text.
    pub fn start(&self) -> usize {
        self.start_pos
    }

    /// Returns the string content of this token.
    pub fn value(&self) -> &'a str {
        self.val
    }

    /// Returns the state (type) of this token.
    pub fn get_state(&self) -> TokenState {
        self.state
    }
}

/// A configurable tokeniser for parsing text into meaningful tokens.
///
/// The `Tokeniser` can be customised with special characters, delimiter pairs,
/// balanced delimiters, and comment markers to suit different syntax requirements.
/// Once configured, it can parse text into tokens according to those rules.
///
/// Note that delimiters and the characters in comment markers are automatically
/// treated as special characters, but with additional distinctions in how they're
/// processed during tokenisation.
///
/// # Examples
///
/// ```
/// use tokenise::Tokeniser;
///
/// // Create and configure a tokeniser for a C-like language
/// let mut tokeniser = Tokeniser::new();
/// tokeniser.add_specials("+-*/=<>!&|^~%");
/// tokeniser.add_delimiter_pairs(&vec!["()", "[]", "{}"]).unwrap();
/// tokeniser.set_sl_comment("//").unwrap();
/// tokeniser.set_ml_comment("/*", "*/").unwrap();
///
/// // Tokenise some code
/// let code = "int main() { // Entry point\n    return 0;\n}";
/// let tokens = tokeniser.tokenise(code).unwrap();
/// ```
pub struct Tokeniser {
    special_characters: Vec<String>,
    delimiter_pairs: Vec<(String, String)>,
    balanced_delimiters: Vec<String>,
    single_line_comment: Option<String>,
    multi_line_comment: Option<(String, String)>
}

impl Tokeniser {
    /// Creates a new, unconfigured `Tokeniser`.
    ///
    /// This constructor creates a tokeniser with no special characters, delimiters, or comment markers.
    /// You'll need to configure it with the appropriate methods before it's ready for use.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// // Configure the tokeniser...
    /// ```
    pub fn new() -> Self {
        Self {
            special_characters: Vec::new(),
            delimiter_pairs: Vec::new(),
            balanced_delimiters: Vec::new(),
            single_line_comment: None,
            multi_line_comment: None,
        }
    }

    /// Adds a single special character to the tokeniser.
    ///
    /// Special characters are treated differently from regular text during tokenisation.
    /// They form `SymbolString` tokens unless they're also configured as delimiters or
    /// used in comment markers.
    ///
    /// # Arguments
    ///
    /// * `special` - The special character to add, which must be a single grapheme
    ///
    /// # Returns
    ///
    /// * `Ok(())` if the character was added successfully
    /// * `Err(String)` if the input is not a single grapheme
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_special("@").unwrap();
    /// tokeniser.add_special("+").unwrap();
    ///
    /// // Unicode graphemes are supported
    /// tokeniser.add_special("👨‍💻").unwrap();
    ///
    /// // This would fail as it's not a single grapheme
    /// assert!(tokeniser.add_special("abc").is_err());
    /// ```
    pub fn add_special(&mut self, special: &str) -> Result<(),String> {
        if !is_grapheme(special) {
            Err(format!("string {:?} is not a single grapheme",special))
        } else {
            if !self.special(special) {
                self.special_characters.push(special.to_string());
            }
            Ok(())
        }
    }

    /// Adds multiple special characters to the tokeniser.
    ///
    /// This is a convenience method that adds each grapheme in the input string
    /// as a special character.
    ///
    /// # Arguments
    ///
    /// * `specials` - A string containing the special characters to add
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_specials("+-*/=<>!&|^~%");
    /// ```
    pub fn add_specials(&mut self, specials: &str) {
        for c in specials.graphemes(true) {
            self.add_special(c).unwrap();
        }
    }

    /// Adds a pair of left and right delimiters to the tokeniser.
    ///
    /// Delimiter pairs are used to mark the beginning and end of sections in text,
    /// such as parentheses, brackets, and braces. During tokenisation, they are
    /// classified as `LDelimiter` and `RDelimiter` respectively.
    ///
    /// Both characters are automatically added as special characters if they aren't already.
    ///
    /// # Arguments
    ///
    /// * `left` - The left (opening) delimiter, which must be a single grapheme
    /// * `right` - The right (closing) delimiter, which must be a single grapheme
    ///
    /// # Returns
    ///
    /// * `Ok(())` if the delimiter pair was added successfully
    /// * `Err(String)` if either character is not a single grapheme, or if either
    ///   character is already used as a different type of delimiter
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_delimiter_pair("(", ")").unwrap();
    /// tokeniser.add_delimiter_pair("[", "]").unwrap();
    /// tokeniser.add_delimiter_pair("{", "}").unwrap();
    ///
    /// // Unicode delimiters are supported
    /// tokeniser.add_delimiter_pair("「", "」").unwrap();
    /// ```
    pub fn add_delimiter_pair(&mut self, left: &str, right: &str) -> Result<(),String> {
        if !is_grapheme(left) {
            Err(format!("string {:?} is not a single grapheme",left))
        } else if !is_grapheme(right) {
            Err(format!("string {:?} is not a single grapheme",right))
        } else {
            match (self.delimiter(left),self.delimiter(right)) {
                (None, None) => {
                    self.add_special(left).unwrap();
                    self.add_special(right).unwrap();
                    self.delimiter_pairs.push((left.to_string(),right.to_string()));
                },
                (None, Some(_)) => {
                    return Err(format!("right delimiter {right:?} is already a delimiter of type {:?} with other pair", self.delimiter(right).unwrap()));
                },
                (Some(_), None) => {
                    return Err(format!("left delimiter {left:?} is already a delimiter of type {:?} with other pair", self.delimiter(left).unwrap()));
                },
                (Some(l), Some(r)) => {
                    match l {
                        Side::Right => {
                            return Err(format!("left delimiter {left:?} is already a delimiter of type {:?} with other pair", Side::Right));
                        },
                        Side::Left => {},
                        Side::Bal => {
                            return Err(format!("left delimiter {left:?} is already a delimiter of type {:?} with other pair", Side::Bal));
                        },
                    }
                    match r {
                        Side::Right => {},
                        Side::Left => {
                            return Err(format!("right delimiter {right:?} is already a delimiter of type {:?} with other pair", Side::Left));
                        },
                        Side::Bal => {
                            return Err(format!("right delimiter {right:?} is already a delimiter of type {:?} with other pair", Side::Bal));
                        },
                    }
                },
            }
            Ok(())
        }
    }

    /// Adds multiple delimiter pairs to the tokeniser.
    ///
    /// Each pair should be represented as a string containing exactly two graphemes,
    /// where the first is the left delimiter and the second is the right delimiter.
    /// 
    /// Each character is automatically added as a special character if it isn't already.
    ///
    /// # Arguments
    ///
    /// * `delimiter_pairs` - A vector of strings, each containing exactly two graphemes
    ///
    /// # Returns
    ///
    /// * `Ok(())` if all delimiter pairs were added successfully
    /// * `Err(String)` if any string doesn't contain exactly two graphemes, or if any
    ///   character is already used as a different type of delimiter
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_delimiter_pairs(&vec!["()", "[]", "{}"]).unwrap();
    /// ```
    pub fn add_delimiter_pairs(&mut self, delimiter_pairs: &Vec<&str>) -> Result<(),String> {
        for &s in delimiter_pairs {
            let v = s.graphemes(true).collect::<Vec<_>>();
            if v.len() != 2 {
                return Err(format!("delimiter pair {s:?} must be made up of 2 graphemes"));
            }
            let [left,right] = v.try_into().unwrap();
            match self.add_delimiter_pair(left, right) {
                Ok(_) => {},
                Err(x) => {
                    return Err(x);
                },
            }
        }
        Ok(())
    }

    /// Adds a balanced delimiter to the tokeniser.
    ///
    /// Balanced delimiters are characters that serve as both opening and closing markers,
    /// such as quotation marks. During tokenisation, they are classified as `BDelimiter`.
    ///
    /// The character is automatically added as a special character if it isn't already.
    ///
    /// # Arguments
    ///
    /// * `delim` - The balanced delimiter, which must be a single grapheme
    ///
    /// # Returns
    ///
    /// * `Ok(())` if the delimiter was added successfully
    /// * `Err(String)` if the character is not a single grapheme, or if it is already used
    ///   as a different type of delimiter
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_balanced_delimiter("\"").unwrap(); // Double quote
    /// tokeniser.add_balanced_delimiter("'").unwrap();  // Single quote
    /// tokeniser.add_balanced_delimiter("`").unwrap();  // Backtick
    /// ```
    pub fn add_balanced_delimiter(&mut self, delim: &str) -> Result<(),String> {
        if !is_grapheme(delim) {
            Err(format!("string {:?} is not a single grapheme",delim))
        } else {
            match self.delimiter(delim) {
                Some(side) => {
                    match side {
                        Side::Right => {
                        return Err(format!("balanced delimiter {delim:?} is already a delimiter of type {:?} with other pair", Side::Right));
                    },
                        Side::Left => {
                            return Err(format!("balanced delimiter {delim:?} is already a delimiter of type {:?} with other pair", Side::Left));
                        },
                        Side::Bal => {},
                    }
                },
                None => {
                    self.add_special(delim).unwrap();
                    self.balanced_delimiters.push(delim.to_string());
                },
            }
            Ok(())
        }
    }

    /// Adds multiple balanced delimiters to the tokeniser.
    ///
    /// Each character in the input string is added as a balanced delimiter.
    /// The characters are automatically added as special characters if they aren't already.
    ///
    /// # Arguments
    ///
    /// * `delims` - A string containing the balanced delimiters to add
    ///
    /// # Returns
    ///
    /// * `Ok(())` if all delimiters were added successfully
    /// * `Err(String)` if any character is already used as a different type of delimiter
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_balanced_delimiters("\"'`").unwrap(); // Adds ", ', and ` as balanced delimiters
    /// ```
    pub fn add_balanced_delimiters(&mut self, delims: &str) -> Result<(),String> {
        for delim in delims.graphemes(true) {
            match self.add_balanced_delimiter(delim) {
                Ok(_) => {},
                Err(x) => {
                    return Err(x);
                }
            }
        }
        Ok(())
    }

    /// Sets the marker for single-line comments.
    ///
    /// Single-line comments run from the marker to the end of the line.
    /// During tokenisation, they are classified as `SLComment`.
    ///
    /// All characters in the comment marker are automatically added as special characters.
    ///
    /// # Arguments
    ///
    /// * `comm` - The single-line comment marker (e.g., "//")
    ///
    /// # Returns
    ///
    /// * `Ok(())` if the marker was set successfully
    /// * `Err(String)` if the marker is an empty string
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.set_sl_comment("//").unwrap();  // C/C++/Rust style
    ///
    /// // Could also use other styles
    /// // tokeniser.set_sl_comment("#").unwrap();   // Python/Ruby style
    /// // tokeniser.set_sl_comment("--").unwrap();  // SQL/Lua style
    /// ```
    pub fn set_sl_comment(&mut self, comm: &str) -> Result<(),String> {
        if comm.len() == 0 {
            Err(format!("Empty string cannot be the start of a single line comment"))
        } else {
            self.add_specials(comm);
            self.single_line_comment = Some(comm.to_string());
            Ok(())
        }
    }

    /// Sets the markers for multi-line comments.
    ///
    /// Multi-line comments run from the start marker to the end marker,
    /// potentially spanning multiple lines. During tokenisation, they
    /// are classified as `MLComment`.
    ///
    /// All characters in both comment markers are automatically added as special characters.
    ///
    /// # Arguments
    ///
    /// * `left` - The start marker for multi-line comments (e.g., "/*")
    /// * `right` - The end marker for multi-line comments (e.g., "*/")
    ///
    /// # Returns
    ///
    /// * `Ok(())` if the markers were set successfully
    /// * `Err(String)` if either marker is an empty string
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.set_ml_comment("/*", "*/").unwrap();  // C/C++/Rust style
    ///
    /// // Could also use other styles
    /// // tokeniser.set_ml_comment("<!--", "-->").unwrap();  // HTML/XML style
    /// // tokeniser.set_ml_comment("{-", "-}").unwrap();    // Haskell style
    /// ```
    /// 
    /// # Warning
    /// 
    /// Be cautious with comment markers that contain alphanumeric characters (like words). 
    /// Since all characters in comment markers are added as special characters, using 
    /// word-based markers may cause unexpected tokenisation of normal text:
    ///
    /// ```
    /// use tokenise::Tokeniser;
    /// 
    /// // Not recommended - would treat the letters in "begin" and "end" as special characters
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.set_ml_comment("=begin", "=end").unwrap(); // Ruby style
    /// ```
    pub fn set_ml_comment(&mut self, left: &str, right: &str) -> Result<(),String> {
        if left.len() == 0 {
            Err(format!("Empty string cannot be the start of a multi-line comment"))
        } else if right.len() == 0 {
            Err(format!("Empty string cannot be the end of a multi-line comment"))
        } else {
            self.add_specials(left);
            self.add_specials(right);
            self.multi_line_comment = Some((left.to_string(),right.to_string()));
            Ok(())
        }
    }

    /// Returns a vector of all registered special characters.
    ///
    /// # Returns
    ///
    /// A vector of string slices, each containing one special character.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_specials("+-*/");
    /// 
    /// let specials = tokeniser.specials();
    /// assert!(specials.contains(&"+"));
    /// assert!(specials.contains(&"-"));
    /// ```
    pub fn specials<'a>(&'a self) -> Vec<&'a str> {
        self.special_characters
            .iter()
            .map(|x|x.as_str())
            .collect()
    }

    /// Returns a vector of all registered left-right delimiter pairs.
    ///
    /// # Returns
    ///
    /// A vector of tuples, each containing a left delimiter and its corresponding right delimiter.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_delimiter_pairs(&vec!["()", "[]"]).unwrap();
    /// 
    /// let delimiters = tokeniser.lr_delimiters();
    /// assert!(delimiters.contains(&("(", ")")));
    /// ```
    pub fn lr_delimiters<'a>(&'a self) -> Vec<(&'a str, &'a str)> {
        self.delimiter_pairs
            .iter()
            .map(|(x,y)|(x.as_str(),y.as_str()))
            .collect()
    }

    /// Returns a vector of all registered balanced delimiters.
    ///
    /// # Returns
    ///
    /// A vector of string slices, each containing one balanced delimiter.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_balanced_delimiters("\"'").unwrap();
    /// 
    /// let delimiters = tokeniser.bal_delimiters();
    /// assert!(delimiters.contains(&"\""));
    /// assert!(delimiters.contains(&"'"));
    /// ```
    pub fn bal_delimiters<'a>(&'a self) -> Vec<&'a str> {
        self.balanced_delimiters
            .iter()
            .map(|x|x.as_str())
            .collect()
    }

    /// Returns the configured single-line comment marker, if any.
    ///
    /// # Returns
    ///
    /// An `Option` containing the single-line comment marker, or `None` if not configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// assert_eq!(tokeniser.sl_comment(), None);
    ///
    /// tokeniser.set_sl_comment("//").unwrap();
    /// assert_eq!(tokeniser.sl_comment(), Some("//"));
    /// ```
    pub fn sl_comment<'a>(&'a self) -> Option<&'a str> {
        self.single_line_comment
            .iter()
            .map(|x| x.as_str())
            .next()
    }

    /// Returns the configured multi-line comment markers, if any.
    ///
    /// # Returns
    ///
    /// An `Option` containing a tuple of the start and end markers for multi-line comments,
    /// or `None` if not configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// assert_eq!(tokeniser.ml_comment(), None);
    ///
    /// tokeniser.set_ml_comment("/*", "*/").unwrap();
    /// assert_eq!(tokeniser.ml_comment(), Some(("/*", "*/")));
    /// ```
    pub fn ml_comment<'a>(&'a self) -> Option<(&'a str, &'a str)> {
        self.multi_line_comment
            .iter()
            .map(|(x,y)|(x.as_str(),y.as_str()))
            .next()
    }

    /// Checks if a character is registered as a special character.
    ///
    /// Special characters include those explicitly added via `add_special`/`add_specials`,
    /// as well as any characters used in delimiters or comment markers.
    ///
    /// # Arguments
    ///
    /// * `c` - The character to check
    ///
    /// # Returns
    ///
    /// `true` if the character is registered as a special character, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_special("+").unwrap();
    /// tokeniser.add_delimiter_pair("(", ")").unwrap();
    ///
    /// assert!(tokeniser.special("+")); // Explicitly added special
    /// assert!(tokeniser.special("(")); // Special because it's a delimiter
    /// assert!(!tokeniser.special("-")); // Not registered as special
    /// ```
    pub fn special(&self, c: &str) -> bool {
        for x in self.specials() {
            if x == c {
                return true;
            }
        }
        false
    }
    
    /// Checks if a character is registered as a delimiter and returns its type.
    ///
    /// # Arguments
    ///
    /// * `c` - The character to check
    ///
    /// # Returns
    ///
    /// * `Some(Side::Left)` if the character is a left delimiter
    /// * `Some(Side::Right)` if the character is a right delimiter
    /// * `Some(Side::Bal)` if the character is a balanced delimiter
    /// * `None` if the character is not a delimiter
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::{Tokeniser, Side};
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_delimiter_pair("(", ")").unwrap();
    /// tokeniser.add_balanced_delimiter("\"").unwrap();
    ///
    /// assert_eq!(tokeniser.delimiter("("), Some(Side::Left));
    /// assert_eq!(tokeniser.delimiter(")"), Some(Side::Right));
    /// assert_eq!(tokeniser.delimiter("\""), Some(Side::Bal));
    /// assert_eq!(tokeniser.delimiter("a"), None);
    /// ```
    pub fn delimiter<'g>(&self, c: &'g str) -> Option<Side> {
        for (x,y) in self.lr_delimiters() {
            if x == c {
                return Some(Side::Left);
            } 
            if y == c {
                return Some(Side::Right);
            }
        }
        for x in self.bal_delimiters() {
            if x == c {
                return Some(Side::Bal);
            }
        }
        None
    }

    /// Checks if a string is the configured single-line comment marker.
    ///
    /// # Arguments
    ///
    /// * `s` - The string to check
    ///
    /// # Returns
    ///
    /// `true` if the string exactly matches the configured single-line comment marker,
    /// `false` otherwise or if no single-line comment marker is configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.set_sl_comment("//").unwrap();
    ///
    /// assert!(tokeniser.is_sl_comment_start("//"));
    /// assert!(!tokeniser.is_sl_comment_start("/"));
    /// ```
    pub fn is_sl_comment_start(&self, s: &str) -> bool {
        match self.sl_comment() {
            None => false,
            Some(sl_comment) => s == sl_comment
        }
    }

    /// Checks if a string ends with the configured single-line comment marker.
    ///
    /// This is used during tokenisation to detect when a series of special characters
    /// transitions into a comment.
    ///
    /// # Arguments
    ///
    /// * `s` - The string to check
    ///
    /// # Returns
    ///
    /// `true` if the string ends with the configured single-line comment marker,
    /// `false` otherwise or if no single-line comment marker is configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.set_sl_comment("//").unwrap();
    ///
    /// assert!(tokeniser.ends_with_sl_comment_start("abc//"));
    /// assert!(!tokeniser.ends_with_sl_comment_start("abc/"));
    /// ```
    pub fn ends_with_sl_comment_start(&self, s: &str) -> bool {
        match self.sl_comment() {
            None => false,
            Some(sl_comment) => s.ends_with(sl_comment)
        }
    }

    /// Checks if a string is the configured multi-line comment start marker.
    ///
    /// # Arguments
    ///
    /// * `s` - The string to check
    ///
    /// # Returns
    ///
    /// `true` if the string exactly matches the configured multi-line comment start marker,
    /// `false` otherwise or if no multi-line comment marker is configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.set_ml_comment("/*", "*/").unwrap();
    ///
    /// assert!(tokeniser.is_ml_comment_start("/*"));
    /// assert!(!tokeniser.is_ml_comment_start("*/"));
    /// ```
    pub fn is_ml_comment_start(&self, s: &str) -> bool {
        match self.ml_comment() {
            None => false,
            Some((start,_)) => s == start
        }
    }

    /// Checks if a string ends with the configured multi-line comment start marker.
    ///
    /// This is used during tokenisation to detect when a series of special characters
    /// transitions into a comment.
    ///
    /// # Arguments
    ///
    /// * `s` - The string to check
    ///
    /// # Returns
    ///
    /// `true` if the string ends with the configured multi-line comment start marker,
    /// `false` otherwise or if no multi-line comment marker is configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.set_ml_comment("/*", "*/").unwrap();
    ///
    /// assert!(tokeniser.ends_with_ml_comment_start("abc/*"));
    /// assert!(!tokeniser.ends_with_ml_comment_start("abc/"));
    /// ```
    pub fn ends_with_ml_comment_start(&self, s: &str) -> bool {
        match self.ml_comment() {
            None => false,
            Some((start,_)) => s.ends_with(start)
        }
    }

    /// Checks if a string is the configured multi-line comment end marker.
    ///
    /// # Arguments
    ///
    /// * `s` - The string to check
    ///
    /// # Returns
    ///
    /// `true` if the string exactly matches the configured multi-line comment end marker,
    /// `false` otherwise or if no multi-line comment marker is configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::Tokeniser;
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.set_ml_comment("/*", "*/").unwrap();
    ///
    /// assert!(tokeniser.is_ml_comment_end("*/"));
    /// assert!(!tokeniser.is_ml_comment_end("/*"));
    /// ```
    pub fn is_ml_comment_end(&self, s: &str) -> bool {
        match self.ml_comment() {
            None => false,
            Some((_, end)) => s == end
        }
    }
    
    /// Tokenises a string according to the configured rules.
    ///
    /// This is the main method of the library, converting a string into a sequence of tokens
    /// based on the special characters, delimiters, and comment markers that have been configured.
    ///
    /// # Arguments
    ///
    /// * `text` - The string to tokenise
    ///
    /// # Returns
    ///
    /// * `Ok(Vec<Token>)` - A vector of tokens if tokenisation was successful
    /// * `Err(String)` - An error message if tokenisation failed
    ///
    /// # Examples
    ///
    /// ```
    /// use tokenise::{Tokeniser, TokenState};
    ///
    /// let mut tokeniser = Tokeniser::new();
    /// tokeniser.add_specials("+-*/=");
    /// tokeniser.add_delimiter_pairs(&vec!["()", "[]"]).unwrap();
    /// tokeniser.set_sl_comment("//").unwrap();
    ///
    /// let source = "x = 42; // The answer";
    /// let tokens = tokeniser.tokenise(source).unwrap();
    ///
    /// // We can now work with the tokens
    /// for token in &tokens {
    ///     match token.get_state() {
    ///         TokenState::Word => println!("Word: {}", token.value()),
    ///         TokenState::SymbolString => println!("Symbol: {}", token.value()),
    ///         TokenState::SLComment => println!("Comment: {}", token.value()),
    ///         _ => println!("Other token: {}", token.value()),
    ///     }
    /// }
    /// ```
    pub fn tokenise<'g>(&self, text: &'g str) -> Result<Vec<Token<'g>>,String> {
        let mut out: Vec<Token<'g>> = Vec::new();
        let mut curr_start: usize = 0;
        let mut curr_state : Option<TokenState> = None;
        for (curr_pos,c) in text.grapheme_indices(true) {
            match curr_state {
                None => {
                    if self.special(c) {
                        if self.is_sl_comment_start(c) {
                            curr_state = Some(SLComment);
                            curr_start = curr_pos;
                        } else if self.is_ml_comment_start(c) {
                            curr_state = Some(MLComment);
                            curr_start = curr_pos;
                        } else {
                            curr_state = Some(SymbolString);
                            curr_start = curr_pos;
                            match self.delimiter(c) {
                                Some(Side::Left) => {
                                    out.push(Token { state: LDelimiter, val: c, start_pos: curr_pos });
                                    curr_state = None;
                                },
                                Some(Side::Right) => {
                                    out.push(Token { state: RDelimiter, val: c, start_pos: curr_pos });
                                    curr_state = None;
                                },
                                Some(Side::Bal) => {
                                    out.push(Token { state: BDelimiter, val: c, start_pos: curr_pos });
                                    curr_state = None;
                                }
                                None => {}
                            }
                        }
                    } else {
                        if c == "\n" || c == "\r" || c == "\r\n" {
                            out.push(Token {
                                state: NewLine,
                                val: c,
                                start_pos: curr_pos
                            });
                        } else if is_whitespace(c) {
                            curr_state = Some(WhiteSpace);
                        } else {
                            curr_state = Some(Word);
                        }
                        curr_start = curr_pos;
                    }
                },
                Some(Word) => {
                    if self.special(c) {
                        out.push(Token{
                            state: Word,
                            val: &text[curr_start..curr_pos],
                            start_pos: curr_start
                        });
                        
                        match self.delimiter(c) {
                            Some(Side::Left) => {
                                out.push(Token { state: LDelimiter, val: c, start_pos: curr_pos });
                                curr_state = None;
                            },
                            Some(Side::Right) => {
                                out.push(Token { state: RDelimiter, val: c, start_pos: curr_pos });
                                curr_state = None;
                            },
                            Some(Side::Bal) => {
                                out.push(Token { state: BDelimiter, val: c, start_pos: curr_pos });
                                curr_state = None;
                            },
                            None => {
                                curr_start = curr_pos;
                                curr_state = Some(SymbolString);
                            }
                        }
                    } else {
                        if is_whitespace(c) {
                            out.push(Token{
                                state: Word,
                                val: &text[curr_start..curr_pos],
                                start_pos: curr_start
                            });
                            if c == "\n" || c == "\r" || c == "\r\n" {
                                out.push(Token {
                                    state: NewLine,
                                    val: c,
                                    start_pos: curr_pos
                                });
                                curr_state = None;
                            } else {
                                curr_state = Some(WhiteSpace);
                                curr_start = curr_pos;
                            }
                        } else {
                        }
                    }
                },
                Some(SymbolString) => {
                    if !self.special(c) {
                        out.push(Token { state: SymbolString, val: &text[curr_start..curr_pos], start_pos: curr_start });
                        curr_start = curr_pos;
                        if is_whitespace(c) {
                            if c == "\n" || c == "\r" || c == "\r\n" {
                                out.push(Token {
                                    state: NewLine,
                                    val: c,
                                    start_pos: curr_pos
                                });
                                curr_state = None;
                            } else {
                                curr_state = Some(WhiteSpace);
                            }
                        } else {
                            curr_state = Some(Word);
                        }
                    } else {
                        let curr_str = &text[curr_start..curr_pos+c.len()];
                        if self.ends_with_sl_comment_start(curr_str) {
                            if self.is_sl_comment_start(curr_str) {
                                curr_state = Some(SLComment);
                            } else {
                                let new_start = curr_pos + c.len() - self.sl_comment().unwrap().len();
                                out.push(Token {
                                    state: SymbolString,
                                    val: &text[curr_start..new_start],
                                    start_pos: curr_start
                                });
                                curr_state = Some(SLComment);
                                curr_start = new_start;
                            }
                        } else if self.ends_with_ml_comment_start(curr_str) {
                            if self.is_ml_comment_start(curr_str) {
                                curr_state = Some(MLComment);
                            } else {
                                let new_start = curr_pos + c.len() - self.ml_comment().unwrap().0.len();
                                out.push(Token {
                                    state: SymbolString,
                                    val: &text[curr_start..new_start],
                                    start_pos: curr_start
                                });
                                curr_state = Some(MLComment);
                                curr_start = new_start;
                            }
                        }
                    }
                },
                Some(WhiteSpace) => {
                    if self.special(c) {
                        out.push(Token {
                            state: WhiteSpace,
                            val: &text[curr_start..curr_pos],
                            start_pos: curr_start
                        });
                        match self.delimiter(c) {
                            None => {
                                if self.is_sl_comment_start(c) {
                                    curr_state = Some(SLComment);
                                } else if self.is_ml_comment_start(c) {
                                    curr_state = Some(MLComment);
                                } else {
                                    curr_state = Some(SymbolString);
                                }
                                curr_start = curr_pos;
                            },
                            Some(Side::Left) => {
                                out.push(Token { state: LDelimiter, val: c, start_pos: curr_pos });
                                curr_state = None;
                            },
                            Some(Side::Right) => {
                                out.push(Token { state: RDelimiter, val: c, start_pos: curr_pos });
                                curr_state = None;
                            },
                            Some(Side::Bal) => {
                                out.push(Token { state: BDelimiter, val: c, start_pos: curr_pos });
                                curr_state = None;
                            }
                        }
                    } else {
                        if c == "\n" || c == "\r" || c == "\r\n" {
                            out.push(Token {
                                state: WhiteSpace,
                                val: &text[curr_start..curr_pos],
                                start_pos: curr_start
                            });
                            out.push(Token {
                                state: NewLine,
                                val: c,
                                start_pos: curr_pos
                            });
                            curr_state = None;
                        } else if !is_whitespace(c) {
                            out.push(Token {
                                state: WhiteSpace,
                                val: &text[curr_start..curr_pos],
                                start_pos: curr_start
                            });
                            curr_start = curr_pos;
                            curr_state = Some(Word);
                        }
                    }
                },
                Some(SLComment) => {
                    if c == "\n" || c == "\r" || c == "\r\n" {
                        out.push(Token {
                            state: SLComment,
                            val: &text[curr_start..curr_pos],
                            start_pos: curr_start
                        });
                        out.push(Token {
                            state: NewLine,
                            val: c,
                            start_pos: curr_pos
                        });
                        curr_state = None;
                    }
                },
                Some(MLComment) => {
                    let curr_str = &text[curr_start..curr_pos+(c.len())];
                    let end = match self.ml_comment() {
                        Some((_, e)) => Ok(e),
                        _ => Err("This should never happen".to_string())
                    }.unwrap();
                    if curr_str.ends_with(end) {
                        out.push(Token {
                            state: MLComment,
                            val: &text[curr_start..curr_pos+(c.len())],
                            start_pos: curr_start
                        });
                        curr_state = None;
                    }
                },
                other => {return Err(format!("curr_state should never reach {:?}",other))}
            }
        }
        if let Some(token) = out.last() {
            if token.value().len() + token.start() != text.len() {
                out.push(Token {
                    state: curr_state.unwrap(),
                    val: &text[curr_start..],
                    start_pos: curr_start
                });
            }
        }
        Ok(out)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_gets_work() {
        let token = Token{ state: Word, val:"hi", start_pos:4 };
        assert_eq!(token.start(), 4);
        assert_eq!(token.value(), "hi");
    }

    #[test]
    fn tokeniser_building_works() {
        let mut tokeniser = Tokeniser::new();
        tokeniser.add_specials("!@%👨‍💻*");
        tokeniser.add_delimiter_pairs(&vec!["<>", "()", "{}", "🇺🇸👋🏽"]).unwrap();
        tokeniser.add_balanced_delimiters("\"").unwrap();
        tokeniser.set_sl_comment("//").unwrap();
        tokeniser.set_ml_comment("/*","*/").unwrap();
        assert_eq!(tokeniser.specials(),vec!["!", "@", "%", "👨‍💻", "*", "<", ">", "(", ")", "{", "}", "🇺🇸", "👋🏽", "\"", "/"]);
        assert_eq!(tokeniser.lr_delimiters(),vec![("<",">"),("(",")"),("{","}"),("🇺🇸","👋🏽")]);
        assert_eq!(tokeniser.bal_delimiters(),vec!["\""]);
        assert_eq!(tokeniser.sl_comment(),Some("//"));
        assert_eq!(tokeniser.ml_comment(),Some(("/*","*/")));
    }

    #[test]
    fn tokeniser_tokenise_works() {
        let source = " hi, skdjfs;;    842\t 39fsl == 3\n \
        what's going on? idk... \n\
        fire___sldfksfl // what's going on? \n\
        idk what I'm 🇺🇸doing \n\
        \n\
         now👋🏽 hi £*$*@ \n\
        help!\n\
        \"hello\"hi";
        let mut tokeniser = Tokeniser::new();
        tokeniser.add_delimiter_pairs(&vec!["()","[]"]).unwrap();
        tokeniser.add_balanced_delimiter("\"").unwrap();
        tokeniser.set_sl_comment("//").unwrap();
        tokeniser.set_ml_comment("🇺🇸","👋🏽").unwrap();
        tokeniser.add_specials(",;=?.'£<>@*");
        assert_eq!(tokeniser.tokenise(source).unwrap(),vec![
            Token { state: WhiteSpace, val: " ", start_pos: 0 },
            Token { state: Word, val: "hi", start_pos: 1 },
            Token { state: SymbolString, val: ",", start_pos: 3 },
            Token { state: WhiteSpace, val: " ", start_pos: 4 },
            Token { state: Word, val: "skdjfs", start_pos: 5 },
            Token { state: SymbolString, val: ";;", start_pos: 11 },
            Token { state: WhiteSpace, val: "    ", start_pos: 13 },
            Token { state: Word, val: "842", start_pos: 17 },
            Token { state: WhiteSpace, val: "\t ", start_pos: 20 },
            Token { state: Word, val: "39fsl", start_pos: 22 },
            Token { state: WhiteSpace, val: " ", start_pos: 27 },
            Token { state: SymbolString, val: "==", start_pos: 28 },
            Token { state: WhiteSpace, val: " ", start_pos: 30 },
            Token { state: Word, val: "3", start_pos: 31 },
            Token { state: NewLine, val: "\n", start_pos: 32 },
            Token { state: WhiteSpace, val: " ", start_pos: 33 },
            Token { state: Word, val: "what", start_pos: 34 },
            Token { state: SymbolString, val: "'", start_pos: 38 },
            Token { state: Word, val: "s", start_pos: 39 },
            Token { state: WhiteSpace, val: " ", start_pos: 40 },
            Token { state: Word, val: "going", start_pos: 41 },
            Token { state: WhiteSpace, val: " ", start_pos: 46 },
            Token { state: Word, val: "on", start_pos: 47 },
            Token { state: SymbolString, val: "?", start_pos: 49 },
            Token { state: WhiteSpace, val: " ", start_pos: 50 },
            Token { state: Word, val: "idk", start_pos: 51 },
            Token { state: SymbolString, val: "...", start_pos: 54 },
            Token { state: WhiteSpace, val: " ", start_pos: 57 },
            Token { state: NewLine, val: "\n", start_pos: 58 },
            Token { state: Word, val: "fire___sldfksfl", start_pos: 59 },
            Token { state: WhiteSpace, val: " ", start_pos: 74 },
            Token { state: SLComment, val: "// what's going on? ", start_pos: 75 },
            Token { state: NewLine, val: "\n", start_pos: 95 },
            Token { state: Word, val: "idk", start_pos: 96 },
            Token { state: WhiteSpace, val: " ", start_pos: 99 },
            Token { state: Word, val: "what", start_pos: 100 },
            Token { state: WhiteSpace, val: " ", start_pos: 104 },
            Token { state: Word, val: "I", start_pos: 105 },
            Token { state: SymbolString, val: "'", start_pos: 106 },
            Token { state: Word, val: "m", start_pos: 107 },
            Token { state: WhiteSpace, val: " ", start_pos: 108 },
            Token { state: MLComment, val: "🇺🇸doing \n\nnow👋🏽", start_pos: 109 },
            Token { state: WhiteSpace, val: " ", start_pos: 136 },
            Token { state: Word, val: "hi", start_pos: 137 },
            Token { state: WhiteSpace, val: " ", start_pos: 139 },
            Token { state: SymbolString, val: "£*", start_pos: 140 },
            Token { state: Word, val: "$", start_pos: 143 },
            Token { state: SymbolString, val: "*@", start_pos: 144 },
            Token { state: WhiteSpace, val: " ", start_pos: 146 },
            Token { state: NewLine, val: "\n", start_pos: 147 },
            Token { state: Word, val: "help!", start_pos: 148 },
            Token { state: NewLine, val: "\n", start_pos: 153 },
            Token { state: BDelimiter, val: "\"", start_pos: 154 },
            Token { state: Word, val: "hello", start_pos: 155 },
            Token { state: BDelimiter, val: "\"", start_pos: 160 },
            Token { state: Word, val: "hi", start_pos: 161 }
        ]);
    }

    #[test]
    fn test_tokeniser_tokenise_newline_handling() {
        let source = "hi   \r\n\n\rhiagain";
        let tokens = vec![
            Token { state: Word, val: "hi", start_pos: 0 },
            Token { state: WhiteSpace, val: "   ", start_pos: 2 },
            Token { state: NewLine, val: "\r\n", start_pos: 5 },
            Token { state: NewLine, val: "\n", start_pos: 7 },
            Token { state: NewLine, val: "\r", start_pos: 8},
            Token { state: Word, val: "hiagain", start_pos: 9}
        ];
        let tokeniser = Tokeniser::new();
        assert_eq!(tokeniser.tokenise(source).unwrap(), tokens);
    }
}
