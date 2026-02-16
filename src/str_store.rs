use std::collections::HashMap;

/// A type alias for string identifiers. Used to efficiently reference interned strings
/// without storing duplicate string data.
/// StrID types are safe to compare like strings since the same string will always map to the
/// same StrID
pub type StrID = usize;

// Some key strings are hard coded as package constants these include type names
// and internal string values
// nil is is used to represent an empty StrID
//
// TODO: using usize here makes the tests platform dependent since on a 32-bit platform the
// constant values will actually be different. Do we care that you won't be able to build/test the
// complier on 32-bit systems? For now no, but what systems still in play use 32-bit architectures?
// Is it just embedded systems?
pub const NIL: StrID = usize::MAX;

pub const U8: StrID = usize::MAX - 1;
pub const U16: StrID = usize::MAX - 2;
pub const U32: StrID = usize::MAX - 3;
pub const U64: StrID = usize::MAX - 4;

pub const I8: StrID = usize::MAX - 5;
pub const I16: StrID = usize::MAX - 6;
pub const I32: StrID = usize::MAX - 7;
pub const I64: StrID = usize::MAX - 8;

pub const F32: StrID = usize::MAX - 9;
pub const F64: StrID = usize::MAX - 10;

pub const STR: StrID = usize::MAX - 11;
pub const BOOL: StrID = usize::MAX - 12;

pub const WRAP: StrID = usize::MAX - 13;
pub const PANIC: StrID = usize::MAX - 14;

// these are used for the internal meta struct for type information
pub const SIZEOF: StrID = usize::MAX - 15;
pub const ALIGNOF: StrID = usize::MAX - 16;
pub const METAFLAGS: StrID = usize::MAX - 17;

fn constant_str_id(s: &str) -> Option<StrID> {
    match s {
        "u8" => Some(U8),
        "u16" => Some(U16),
        "u32" => Some(U32),
        "u64" => Some(U64),
        "i8" => Some(I8),
        "i16" => Some(I16),
        "i32" => Some(I32),
        "i64" => Some(I64),
        "f32" => Some(F32),
        "f64" => Some(F64),
        "str" => Some(STR),
        "bool" => Some(BOOL),

        // keywords used throughout the complier
        "panic" => Some(PANIC),
        "size_of" => Some(SIZEOF),
        "align_of" => Some(ALIGNOF),
        "flags" => Some(METAFLAGS),

        // this is not a valid identifier so we can use it in the compiler
        // without worrying about conflicting with user identifiers
        "<wrap>" => Some(WRAP),
        _ => None,
    }
}

fn constant_id_str(id: StrID) -> Option<&'static str> {
    match id {
        U8 => Some("u8"),
        U16 => Some("u16"),
        U32 => Some("u32"),
        U64 => Some("u64"),
        I8 => Some("i8"),
        I16 => Some("i16"),
        I32 => Some("i32"),
        I64 => Some("i64"),
        F32 => Some("f32"),
        F64 => Some("f64"),
        STR => Some("str"),
        BOOL => Some("bool"),
        // this is not a valid identifier so we can use it in the compiler
        // without worrying about conflicting with user identifiers
        WRAP => Some("<wrap>"),
        // Nil is also not a valid identifier
        NIL => Some("<nil>"),
        PANIC => Some("panic"),
        _ => None,
    }
}

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
        if let Some(id) = constant_str_id(s) {
            return id;
        }

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
    pub fn get_string(&self, id: StrID) -> Option<String> {
        match constant_id_str(id) {
            Some(id) => Some(id.to_string()),
            None => self.reverse_strings.get(id).cloned(),
        }
    }

    /// get the StrID for the string if it has been added previously.
    pub fn _find_id(&self, s: &str) -> Option<StrID> {
        match constant_str_id(s) {
            Some(id) => Some(id),
            None => self.strings.get(s).copied(),
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

    #[test]
    fn test_find_string() {
        let mut store = StrStore::new();
        store.get_id("hello");
        store.get_id("goodbye");
        if store._find_id("hello").is_none() {
            panic!("hello should be in the store");
        }
        if store._find_id("world").is_some() {
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

    #[test]
    fn test_constant_strings() {
        let mut store = StrStore::new();
        let u8_id = store.get_id("u8");
        assert_eq!(u8_id, U8);

        let u16_id = store.get_id("u16");
        assert_eq!(u16_id, U16);

        store.get_id("other");
        let bool_id = store.get_id("bool");
        assert_eq!(bool_id, BOOL);
    }
}
