use std::collections::HashMap;

/// A type alias for string identifiers. Used to efficiently reference interned strings
/// without storing duplicate string data.
/// StrID types are safe to compare like strings since the same string will always map to the
/// same StrID
pub type StrID = usize;

/// A string interning store that maps strings to unique identifiers.
///
/// This structure allows efficient deduplication of strings during lexing and parsing.
/// Instead of storing the same string multiple times in memory, we store it once and
/// reference it by a small numeric ID.
///
/// # Example
/// ```
/// let mut store = StrStore::new();
/// let id1 = store.get_id("hello");
/// let id2 = store.get_id("hello"); // Returns the same ID without storing "hello" twice
/// assert_eq!(id1, id2);
/// ```
pub struct StrStore<'a> {
    /// Map from string slices to their unique identifiers
    strings: HashMap<&'a str, StrID>,
    /// Counter for generating the next unique ID
    next_id: usize,
}

impl<'a> StrStore<'a> {
    /// Creates a new empty string store.
    pub fn new() -> Self {
        StrStore {
            strings: HashMap::new(),
            next_id: 0,
        }
    }

    /// Returns or creates an interned ID for the given string.
    ///
    /// If the string has been seen before, returns its existing ID.
    /// Otherwise, assigns a new unique ID to this string and stores it.
    pub fn get_id(&mut self, s: &'a str) -> StrID {
        match self.strings.get(s) {
            // String already interned, return its existing ID
            Some(id) => *id,
            // New string encountered, assign it a new ID
            None => {
                let id = self.next_id;
                self.next_id += 1;
                self.strings.insert(s, self.next_id);
                id
            }
        }
    }
}
