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
pub struct StrStore {
    // TODO: I'd like to not copy every string twice like this but ownership is such a headach
    // right now that i'm going to leave this as is.
    // I probably need to rething this type but I do think that the StrStore should own the source
    // code bytes
    //
    /// Map from string slices to their unique identifiers
    strings: HashMap<String, StrID>,
    /// Sequential list of strings for reverse lookup (index = StrID)
    reverse_strings: Vec<String>,
    /// Counter for generating the next unique ID
    next_id: usize,
}

impl StrStore {
    /// Creates a new empty string store.
    pub fn new() -> Self {
        StrStore {
            strings: HashMap::new(),
            reverse_strings: Vec::new(),
            next_id: 0,
        }
    }

    /// Returns or creates an interned ID for the given string.
    ///
    /// If the string has been seen before, returns its existing ID.
    /// Otherwise, assigns a new unique ID to this string and stores it.
    pub fn get_id(&mut self, s: &str) -> StrID {
        match self.strings.get(s) {
            // String already interned, return its existing ID
            Some(id) => *id,
            // New string encountered, assign it a new ID
            None => {
                let id = self.next_id;
                self.next_id += 1;
                self.strings.insert(s.to_string(), id);
                self.reverse_strings.push(s.to_string());
                id
            }
        }
    }

    /// Look up the string for a given ID. Returns None if ID not found.
    pub fn get_string(&self, id: StrID) -> Option<&String> {
        self.reverse_strings.get(id)
    }

    /// get the StrID for the string if it has been added previously.
    pub fn find_id(&self, s: &str) -> Option<StrID> {
        match self.strings.get(s) {
            Some(id) => Some(*id),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_first_string_gets_id_zero() {
        let mut store = StrStore::new();
        let id = store.get_id("hello");
        assert_eq!(id, 0);
    }

    fn test_find_string() {
        let mut store = StrStore::new();
        store.get_id("hello");
        store.get_id("goodbye");
        if store.find_id("hello").is_none() {
            panic!("hello should be in the store");
        }
        if store.find_id("world").is_some() {
            panic!("world should not be in the store");
        }
    }

    #[test]
    fn test_duplicate_string_returns_same_id() {
        let mut store = StrStore::new();
        let id1 = store.get_id("hello");
        store.get_id("other");
        let id2 = store.get_id("hello");
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_different_strings_get_different_ids() {
        let mut store = StrStore::new();
        let id1 = store.get_id("hello");
        let id2 = store.get_id("world");
        store.get_id("hello");
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_empty_string_interning() {
        let mut store = StrStore::new();
        let id1 = store.get_id("");
        let id2 = store.get_id("");
        assert_eq!(id1, id2);
        assert_eq!(id1, 0);
    }

    #[test]
    fn test_string_with_special_characters() {
        let mut store = StrStore::new();
        let id1 = store.get_id("hello\nworld\t!");
        let id2 = store.get_id("hello\nworld\t!");
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_unicode_strings() {
        let mut store = StrStore::new();
        let id1 = store.get_id("café");
        let id2 = store.get_id("café");
        let id3 = store.get_id("🦀");
        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_long_string_interning() {
        let mut store = StrStore::new();
        let long_string = "a".repeat(1000);
        let id1 = store.get_id(&long_string);
        let id2 = store.get_id(&long_string);
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_multiple_different_strings() {
        let mut store = StrStore::new();
        let mut ids = Vec::new();
        let strings = vec!["apple", "banana", "cherry", "date", "fig"];

        for s in &strings {
            ids.push(store.get_id(s));
        }

        // All IDs should be unique
        ids.sort();
        for i in 0..ids.len() {
            for j in i + 1..ids.len() {
                assert_ne!(ids[i], ids[j]);
            }
        }

        // Retrieving same strings should return same IDs
        for (i, s) in strings.iter().enumerate() {
            assert_eq!(store.get_id(s), ids[i]);
        }
    }

    #[test]
    fn test_store_size_grows_correctly() {
        let mut store = StrStore::new();
        assert_eq!(store.strings.len(), 0);

        store.get_id("first");
        assert_eq!(store.strings.len(), 1);

        store.get_id("second");
        assert_eq!(store.strings.len(), 2);

        store.get_id("first");
        assert_eq!(store.strings.len(), 2);
    }

    #[test]
    fn test_whitespace_strings_are_different() {
        let mut store = StrStore::new();
        let id1 = store.get_id("hello");

        // TODO: do we actually want the prefix/suffix whitespace to matter?
        // will this even come up?
        let id2 = store.get_id(" hello");
        let id3 = store.get_id("hello ");
        assert_ne!(id1, id2);
        assert_ne!(id1, id3);
        assert_ne!(id2, id3);
    }
}
