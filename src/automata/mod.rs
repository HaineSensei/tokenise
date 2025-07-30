use std::marker::PhantomData;

#[derive(Clone)]
pub(crate) struct NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, T, G, F>
where
    S: State,
    T: TokenVal,
    G: Fn(&S) -> Result<StateSort,String>,
    F: Fn(&S, &T) -> S
{
    initial_state: S,
    inner_state_sort_map: G,
    inner_transition_map: F,
    _phantom: PhantomData<T>
}

pub(crate) trait TokenVal: Eq {}

impl TokenVal for crate::low_level::LowTokenVal {}

impl<S, T, G, F> NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, T, G, F> 
where
    S: State,
    T: TokenVal,
    G: Fn(&S) -> Result<StateSort,String>,
    F: Fn(&S, &T) -> S
{
    pub(crate) fn transition_map(&self, state: &S, token: &T) -> S {
        (self.inner_transition_map)(state, token)
    }

    pub(crate) fn state_sort_map(&self, state: &S) -> Result<StateSort, String> {
        (self.inner_state_sort_map)(state)
    }

    pub(crate) fn new_automaton(&self) -> NotNecessarilyFiniteStateDeterministicAutomaton<S, T, G, F> {
        NotNecessarilyFiniteStateDeterministicAutomaton::new(self)
    }
}

pub(crate) struct NotNecessarilyFiniteStateDeterministicAutomaton<'a, S, T, G, F>
where
    S: State,
    T: TokenVal,
    G: Fn(&S) -> Result<StateSort,String>,
    F: Fn(&S, &T) -> S
{
    pub(crate) blueprint: &'a NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, T, G, F>,
    pub(crate) curr_state: S
}

impl<'a, S, T, G, F> NotNecessarilyFiniteStateDeterministicAutomaton<'a, S, T, G, F> 
where 
    S: State,
    T: TokenVal,
    G: Fn(&S) -> Result<StateSort,String>,
    F: Fn(&S, &T) -> S
{
    pub(crate) fn update_state(&mut self, token: &T) -> Result<StateSort, String> {
        self.curr_state = self.blueprint.transition_map(&self.curr_state, token);
        self.blueprint.state_sort_map(&self.curr_state)
    }

    fn new(blueprint: &'a NotNecessarilyFiniteStateDeterministicAutomatonBlueprint<S, T, G, F>) -> Self {
        Self {blueprint, curr_state: blueprint.initial_state.clone() }
    }
}

pub(crate) trait State: Eq + Clone {}

impl State for RustState {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct RustState {
    pub(crate) name: String,
    pub(crate) index: usize
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum StateSort {
    Accepting,
    Rejecting,
    ImmediateRejecting,
}