use std::fmt::Debug;
use std::marker::PhantomData;
use std::slice::Iter;

/// An Indexer is any type that can be converted to and from a usize and used to index a vector type
pub trait Indexer: Debug + Copy + Eq + From<usize> {
    fn index(&self) -> usize;
}

/// An vector that can only be indexed by a specific type. The indexing type must implement the Indexer
/// trait
pub struct IndexVec<I: Indexer, V> {
    data: Vec<V>,
    _marker: PhantomData<I>,
}

impl<I: Indexer, V> IndexVec<I, V> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Push a value and return its typed index.
    pub fn push(&mut self, value: V) -> I {
        let idx = I::from(self.data.len());
        self.data.push(value);
        idx
    }

    /// Number of elements in the IndexVec
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if the vec contains no elements.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Get a reference by typed index, returning `None` if out of bounds.
    pub fn get(&self, index: I) -> Option<&V> {
        self.data.get(index.index())
    }

    /// Get a mutable reference by typed index, returning `None` if out of bounds.
    pub fn get_mut(&mut self, index: I) -> Option<&mut V> {
        self.data.get_mut(index.index())
    }

    /// Iterate over all values.
    pub fn iter(&self) -> Iter<'_, V> {
        self.data.iter()
    }

    /// Enumerate over all values.
    pub fn iter_enumerated(&self) -> impl Iterator<Item = (I, &V)> {
        self.data.iter().enumerate().map(|(i, v)| (I::from(i), v))
    }
}
