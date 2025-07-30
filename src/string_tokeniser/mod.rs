use crate::low_level::LowTokenVal;

#[derive(Clone, Debug)]
pub(crate) enum StringStyle {
    Rust,
    Custom(Vec<Vec<LowTokenVal>>)
}

// TODO: Add StringTokeniser implementation here