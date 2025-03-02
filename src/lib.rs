use unicode_segmentation::UnicodeSegmentation;

pub enum Token<'a> {
    Word {
        val: &'a str, 
        start_pos: usize
    },
    LParenthesis {
        val: &'a str,
        start_pos: usize
    },
    RParenthesis {
        val: &'a str,
        start_pos: usize
    },
    SymbolString {
        val: &'a str,
        start_pos: usize
    },
    NewLine {
        val: &'a str,
        start_pos: usize
    },
    WhiteSpace {
        val: &'a str,
        start_pos: usize
    }
}

impl<'a> Token<'a> {
    pub fn start(&self) -> usize {
        match self {
            &Self::Word { val: _, start_pos } => start_pos,
            &Self::LParenthesis { val: _, start_pos } => start_pos,
            &Self::RParenthesis { val: _, start_pos } => start_pos,
            &Self::SymbolString { val: _, start_pos } => start_pos,
            &Self::NewLine { val: _, start_pos } => start_pos,
            &Self::WhiteSpace { val: _, start_pos } => start_pos
        }
    }

    pub fn value(&self) -> &'a str {
        match self {
            &Self::Word { val, start_pos: _ } => val,
            &Self::LParenthesis { val, start_pos: _ } => val,
            &Self::RParenthesis { val, start_pos: _ } => val,
            &Self::SymbolString { val, start_pos: _ } => val,
            &Self::NewLine { val, start_pos: _ } => val,
            &Self::WhiteSpace { val, start_pos: _ } => val
        }
    }
}

pub struct Tokeniser<'a, 'b, 'c, 'd, 'e> {
    special_characters: Vec<&'a str>,
    parenthesis_pairs: Vec<(&'b str,&'b str)>,
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_gets_work() {
        let token = Token::Word {val:"hi", start_pos:4};
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
