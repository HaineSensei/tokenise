use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone,Copy,Debug)]
pub enum TokenState {
    Word,
    LParenthesis,
    RParenthesis,
    SymbolString,
    NewLine,
    WhiteSpace,
}
use TokenState::*;

pub enum Side {
    Right,
    Left
}

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

pub struct Tokeniser<'a, 'b, 'c, 'd, 'e> {
    special_characters: Vec<&'a str>,
    parenthesis_pairs: Vec<(&'b str, &'b str)>,
    single_line_comment: Option<&'c str>,
    multi_line_comment: Option<(&'d str, &'e str)>
}

impl<'a, 'b, 'c, 'd, 'e> Tokeniser<'a, 'b, 'c, 'd, 'e> {
    pub fn new(special_characters: &'a str, parenthesis_pairs: &Vec<&'b str>, single_line_comment: Option<&'c str>, multi_line_comment: Option<(&'d str, &'e str)>) -> Result<Self,String> {
        let parenthesis_pairs_result: Result<Vec<(&str,&str)>,String> = parenthesis_pairs.into_iter().map(|&s| {
            let vector: Vec<&str> = s.graphemes(true).collect();
            if vector.len() == 2 {
                Ok((vector[0],vector[1]))
            } else {
                return Err(format!("Parenthesis pair \"{}\" not length 2",s))
            }
        })
        .collect();
        match parenthesis_pairs_result {
            Ok(parenthesis_pairs) => Ok(Self {
                special_characters: special_characters.graphemes(true).collect(),
                parenthesis_pairs,
                single_line_comment,
                multi_line_comment
            }),
            Err(x) => Err(x)
        }
    }

    pub fn specials(&self) -> Vec<&'a str> {
        self.special_characters.clone()
    }

    pub fn parentheses(&self) -> Vec<(&'b str, &'b str)> {
        self.parenthesis_pairs.clone()
    }

    pub fn sl_comment(&self) -> Option<&'c str> {
        self.single_line_comment
    }

    pub fn ml_comment(&self) -> Option<(&'d str, &'e str)> {
        self.multi_line_comment
    }

    pub fn special<'f>(&self, c:&'f str) -> bool {
        for x in self.specials() {
            if x == c {
                return true;
            }
        }
        false
    }
    
    pub fn parenthesis<'f>(&self, c: &'f str) -> Option<Side> {
        for (x,y) in self.parentheses() {
            if x == c {
                return Some(Side::Left);
            } 
            if y == c {
                return Some(Side::Right)
            }
        }
        None
    }

    pub fn is_sl_comment_start(&self, s: &String) -> bool {
        match self.sl_comment() {
            None => false,
            Some(sl_comment) => s.as_str() == sl_comment
        }
    }

    pub fn is_ml_comment_start(&self, s: &String) -> bool {
        match self.ml_comment() {
            None => false,
            Some((start,_)) => s.as_str() == start
        }
    }

    pub fn is_ml_comment_end(&self, s: &String) -> bool {
        match self.ml_comment() {
            None => false,
            Some((_, end)) => s.as_str() == end
        }
    }

    pub fn well_formed(&self) -> bool {
        for (x,y) in self.parentheses() {
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
    /// TODO: Add whitespace and comment handling
    pub fn tokenise<'f>(&self, text: &'f str) -> Result<Vec<Token<'f>>,String> {
        let mut out: Vec<Token<'f>> = Vec::new();
        let mut curr_string: String = String::new();
        let mut curr_start: usize = 0;
        let mut curr_state : Option<TokenState> = None;
        for (curr_pos,c) in text.grapheme_indices(true) {
            match curr_state {
                None => {
                    curr_string.push_str(c);
                    if self.special(c) {
                        curr_state = Some(SymbolString);
                        curr_start = curr_pos;
                        match self.parenthesis(c) {
                            Some(Side::Left) => {
                                out.push(Token { state: LParenthesis, val: c, start_pos: curr_pos });
                                curr_string = String::new();
                                curr_state = None;
                            },
                            Some(Side::Right) => {
                                out.push(Token { state: RParenthesis, val: c, start_pos: curr_pos });
                                curr_string = String::new();
                                curr_state = None;
                            },
                            None => {}
                        }
                    } else {
                        curr_state = Some(Word);
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
                        
                        match self.parenthesis(c) {
                            Some(Side::Left) => {
                                out.push(Token { state: LParenthesis, val: c, start_pos: curr_pos });
                                curr_string = String::new();
                                curr_state = None;
                            },
                            Some(Side::Right) => {
                                out.push(Token { state: RParenthesis, val: c, start_pos: curr_pos });
                                curr_string = String::new();
                                curr_state = None;
                            },
                            None => {
                                curr_string = String::from(c);
                                curr_start = curr_pos;
                                curr_state = Some(SymbolString);
                            }
                        }
                    } else {
                        curr_string.push_str(c);
                    }
                },
                Some(SymbolString) => {
                    if !self.special(c) {
                        out.push(Token { state: SymbolString, val: &text[curr_start..curr_pos], start_pos: curr_start });
                        curr_string = String::from(c);
                        curr_start = curr_pos;
                        if is_whitespace(c) {

                        }
                    }
                },
                other => {return Err(format!("curr_state should never reach {:?}",other))}
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
        let token = Token{ state: Word, val:"hi", start_pos:4};
        assert_eq!(token.start(), 4);
        assert_eq!(token.value(), "hi");
    }

    #[test]
    fn tokeniser_new_works() {
        let tokeniser = Tokeniser::new("!@%👨‍💻*", &vec!["<>", "()", "{}", "🇺🇸👋🏽"], Some("//"), Some(("/*","*/"))).unwrap();
        assert_eq!(tokeniser.specials(),vec!["!", "@", "%", "👨‍💻", "*"]);
        assert_eq!(tokeniser.parentheses(),vec![("<",">"),("(",")"),("{","}"),("🇺🇸","👋🏽")]);
        assert_eq!(tokeniser.sl_comment(),Some("//"));
        assert_eq!(tokeniser.ml_comment(),Some(("/*","*/")));
    }
}
