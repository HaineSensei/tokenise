use crate::{LowTokeniser, LowTokenSort};

#[test]
fn test_basic_alphanumeric_token() {
    let low_tokeniser = LowTokeniser::default();
    let source = "hello";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].val, "hello");
    assert_eq!(tokens[0].sort, LowTokenSort::AlphaNum);
    assert_eq!(tokens[0].initial_index, 0);
}

#[test]
fn test_symbols_separate() {
    let low_tokeniser = LowTokeniser::default();
    let source = "(){}";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[0].val, "(");
    assert_eq!(tokens[0].sort, LowTokenSort::Symbol);
    assert_eq!(tokens[1].val, ")");
    assert_eq!(tokens[1].sort, LowTokenSort::Symbol);
    assert_eq!(tokens[2].val, "{");
    assert_eq!(tokens[2].sort, LowTokenSort::Symbol);
    assert_eq!(tokens[3].val, "}");
    assert_eq!(tokens[3].sort, LowTokenSort::Symbol);
}

#[test]
fn test_whitespace_grouping() {
    let low_tokeniser = LowTokeniser::default();
    let source = "   ";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].val, "   ");
    assert_eq!(tokens[0].sort, LowTokenSort::WhiteSpace);
}

#[test]
fn test_mixed_content() {
    let low_tokeniser = LowTokeniser::default();
    let source = "hello world";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0].val, "hello");
    assert_eq!(tokens[0].sort, LowTokenSort::AlphaNum);
    assert_eq!(tokens[1].val, " ");
    assert_eq!(tokens[1].sort, LowTokenSort::WhiteSpace);
    assert_eq!(tokens[2].val, "world");
    assert_eq!(tokens[2].sort, LowTokenSort::AlphaNum);
}

#[test]
fn test_numbers() {
    let low_tokeniser = LowTokeniser::default();
    let source = "123";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].val, "123");
    assert_eq!(tokens[0].sort, LowTokenSort::AlphaNum);
}

#[test]
fn test_mixed_alphanumeric() {
    let low_tokeniser = LowTokeniser::default();
    let source = "var123";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].val, "var123");
    assert_eq!(tokens[0].sort, LowTokenSort::AlphaNum);
}

#[test]
fn test_punctuation_operators_separate() {
    let low_tokeniser = LowTokeniser::default();
    let source = "+=*-";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 4);
    for token in &tokens {
        assert_eq!(token.sort, LowTokenSort::Symbol);
    }
}

#[test]
fn test_realistic_code_snippet() {
    let low_tokeniser = LowTokeniser::default();
    let source = "let x = 42;";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 8);
    assert_eq!(tokens[0].val, "let");
    assert_eq!(tokens[0].sort, LowTokenSort::AlphaNum);
    assert_eq!(tokens[1].val, " ");
    assert_eq!(tokens[1].sort, LowTokenSort::WhiteSpace);
    assert_eq!(tokens[2].val, "x");
    assert_eq!(tokens[2].sort, LowTokenSort::AlphaNum);
    assert_eq!(tokens[3].val, " ");
    assert_eq!(tokens[3].sort, LowTokenSort::WhiteSpace);
    assert_eq!(tokens[4].val, "=");
    assert_eq!(tokens[4].sort, LowTokenSort::Symbol);
    assert_eq!(tokens[5].val, " ");
    assert_eq!(tokens[5].sort, LowTokenSort::WhiteSpace);
    assert_eq!(tokens[6].val, "42");
    assert_eq!(tokens[6].sort, LowTokenSort::AlphaNum);
    assert_eq!(tokens[7].val, ";");
    assert_eq!(tokens[7].sort, LowTokenSort::Symbol);
}

#[test]
fn test_newlines() {
    let low_tokeniser = LowTokeniser::default();
    let source = "\n";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].val, "\n");
    assert_eq!(tokens[0].sort, LowTokenSort::NewLine);
}

#[test]
fn test_carriage_return() {
    let low_tokeniser = LowTokeniser::default();
    let source = "\r";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].val, "\r");
    assert_eq!(tokens[0].sort, LowTokenSort::NewLine);
}

#[test]
fn test_multiple_newlines_grouping() {
    let low_tokeniser = LowTokeniser::default();
    let source = "\n\n\n";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].val, "\n\n\n");
    assert_eq!(tokens[0].sort, LowTokenSort::NewLine);
}

#[test]
fn test_mixed_newlines_and_text() {
    let low_tokeniser = LowTokeniser::default();
    let source = "hello\nworld";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0].val, "hello");
    assert_eq!(tokens[0].sort, LowTokenSort::AlphaNum);
    assert_eq!(tokens[1].val, "\n");
    assert_eq!(tokens[1].sort, LowTokenSort::NewLine);
    assert_eq!(tokens[2].val, "world");
    assert_eq!(tokens[2].sort, LowTokenSort::AlphaNum);
}

#[test]
fn test_simple_multiline() {
    let low_tokeniser = LowTokeniser::default();
    let source = "a\nb";
    let tokens: Vec<_> = low_tokeniser.tokenise(source).collect();
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[0].val, "a");
    assert_eq!(tokens[0].sort, LowTokenSort::AlphaNum);
    assert_eq!(tokens[1].val, "\n");
    assert_eq!(tokens[1].sort, LowTokenSort::NewLine);
    assert_eq!(tokens[2].val, "b");
    assert_eq!(tokens[2].sort, LowTokenSort::AlphaNum);
}