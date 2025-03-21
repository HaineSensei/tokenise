use unicode_segmentation::UnicodeSegmentation;

// TODO: add multi-character Parenthesis
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenState {
    Word,
    LDelimiter,
    RDelimiter,
    BDelimiter,
    SymbolString,
    NewLine,
    WhiteSpace,
    SLComment,
    MLComment
}
use TokenState::*;

#[derive(Debug)]
pub enum Side {
    Right,
    Left,
    Bal
}

pub fn is_grapheme(s: &str) -> bool {
    s.graphemes(true).collect::<Vec<_>>().len() == 1
}

pub fn is_whitespace(c: &str) -> bool {
    c.trim().is_empty()
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token<'a> {
    state: TokenState,
    val: &'a str,
    start_pos: usize
}

impl<'a> Token<'a> {
    pub fn start(&self) -> usize {
        self.start_pos
    }

    pub fn value(&self) -> &'a str {
        self.val
    }

    pub fn get_state(&self) -> TokenState {
        self.state
    }
}

pub struct Tokeniser {
    special_characters: Vec<String>,
    delimiter_pairs: Vec<(String, String)>,
    balanced_delimiters: Vec<String>,
    single_line_comment: Option<String>,
    multi_line_comment: Option<(String, String)>
}

impl Tokeniser {

    pub fn new() -> Self {
        Self {
            special_characters: Vec::new(),
            delimiter_pairs: Vec::new(),
            balanced_delimiters: Vec::new(),
            single_line_comment: None,
            multi_line_comment: None,
        }
    }

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

    pub fn add_specials(&mut self, specials: &str) {
        for c in specials.graphemes(true) {
            self.add_special(c).unwrap();
        }
    }

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

    pub fn set_sl_comment(&mut self, comm: &str) -> Result<(),String> {
        if comm.len() == 0 {
            Err(format!("Empty string cannot be the start of a single line comment"))
        } else {
            self.add_specials(comm);
            self.single_line_comment = Some(comm.to_string());
            Ok(())
        }
    }

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

    pub fn specials<'a>(&'a self) -> Vec<&'a str> {
        self.special_characters
            .iter()
            .map(|x|x.as_str())
            .collect()
    }

    pub fn lr_delimiters<'a>(&'a self) -> Vec<(&'a str, &'a str)> {
        self.delimiter_pairs
            .iter()
            .map(|(x,y)|(x.as_str(),y.as_str()))
            .collect()
    }

    pub fn bal_delimiters<'a>(&'a self) -> Vec<&'a str> {
        self.balanced_delimiters
            .iter()
            .map(|x|x.as_str())
            .collect()
    }

    pub fn sl_comment<'a>(&'a self) -> Option<&'a str> {
        self.single_line_comment
            .iter()
            .map(|x| x.as_str())
            .next()
    }

    pub fn ml_comment<'a>(&'a self) -> Option<(&'a str, &'a str)> {
        self.multi_line_comment
            .iter()
            .map(|(x,y)|(x.as_str(),y.as_str()))
            .next()
    }

    pub fn special(&self, c: &str) -> bool {
        for x in self.specials() {
            if x == c {
                return true;
            }
        }
        false
    }
    
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

    pub fn is_sl_comment_start(&self, s: &str) -> bool {
        match self.sl_comment() {
            None => false,
            Some(sl_comment) => s == sl_comment
        }
    }

    pub fn ends_with_sl_comment_start(&self, s: &str) -> bool {
        match self.sl_comment() {
            None => false,
            Some(sl_comment) => s.ends_with(sl_comment)
        }
    }

    pub fn is_ml_comment_start(&self, s: &str) -> bool {
        match self.ml_comment() {
            None => false,
            Some((start,_)) => s == start
        }
    }

    pub fn ends_with_ml_comment_start(&self, s: &str) -> bool {
        match self.ml_comment() {
            None => false,
            Some((start,_)) => s.ends_with(start)
        }
    }

    pub fn is_ml_comment_end(&self, s: &str) -> bool {
        match self.ml_comment() {
            None => false,
            Some((_, end)) => s == end
        }
    }

    pub fn well_formed(&self) -> bool {
        for (x,y) in self.lr_delimiters() {
            if (!self.special(x)) || !self.special(y) {
                return false;
            }
        }
        if let Some(sl_comment) = self.sl_comment() {
            for c in sl_comment.graphemes(true) {
                if !self.special(c) {
                    return false;
                }
            }
        }
        if let Some((start, end)) = self.ml_comment() {
            for c in start.graphemes(true) {
                if !self.special(c) {
                    return false;
                }
            }
            for c in end.graphemes(true) {
                if !self.special(c) {
                    return false
                }
            }
        }
        true
    }

    
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
