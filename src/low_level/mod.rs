use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};
use unicode_general_category::{get_general_category, GeneralCategory};

#[derive(Clone, Debug)]
pub(crate) struct LowTokeniser {
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
    pub(crate) fn new(currency_are_symbols: bool, other_symbols_are_symbols: bool) -> Self {
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

    pub(crate) fn add_exception_symbol(&mut self, c: &str) -> Result<(), String> {
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

    pub(crate) fn add_exception_alphanum(&mut self, c: &str) -> Result<(), String> {
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

    pub(crate) fn tokenise<'a, 'b>(&'b self, source: &'a str) -> LowTokenIter<'a, 'b> {
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
pub(crate) enum LowTokenSort {
    AlphaNum,
    WhiteSpace,
    NewLine,
    Symbol
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct LowTokenVal {
    pub(crate) val: String,
    pub(crate) sort: LowTokenSort
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct LowToken<'a> {
    pub(crate) val: &'a str,
    pub(crate) initial_index: usize,
    pub(crate) sort: LowTokenSort
}

impl<'a> LowToken<'a> {
    pub(crate) fn as_val(&self) -> LowTokenVal {
        LowTokenVal::new(self.val, self.sort)
    }
}

impl LowTokenVal {
    pub(crate) fn new(val: &str, sort: LowTokenSort) -> Self {
        Self { val: val.into(), sort }
    }

    pub(crate) fn matches(&self, other: LowToken) -> bool {
        self.val == other.val && self.sort == other.sort
    }
}

pub(crate) struct LowTokenIter<'a, 'b> {
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