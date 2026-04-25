use serde::{Deserialize, Serialize};
use std::{collections::HashMap, u32};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct MStr {
    id: u32,
}

pub const STR: MStr = MStr { id: u32::MAX };
pub const TRUE: MStr = MStr { id: u32::MAX - 1 };
pub const FALSE: MStr = MStr { id: u32::MAX - 2 };
pub const FN: MStr = MStr { id: u32::MAX - 3 };
pub const IF: MStr = MStr { id: u32::MAX - 4 };
pub const IN: MStr = MStr { id: u32::MAX - 5 };
pub const RETURN: MStr = MStr { id: u32::MAX - 6 };
pub const ELSE: MStr = MStr { id: u32::MAX - 7 };
pub const WHILE: MStr = MStr { id: u32::MAX - 8 };
pub const FOR: MStr = MStr { id: u32::MAX - 9 };
pub const LOOP: MStr = MStr { id: u32::MAX - 10 };
pub const BREAK: MStr = MStr { id: u32::MAX - 11 };
pub const CONTINUE: MStr = MStr { id: u32::MAX - 12 };
pub const DEFER: MStr = MStr { id: u32::MAX - 13 };
pub const STRUCT: MStr = MStr { id: u32::MAX - 14 };
pub const ENUM: MStr = MStr { id: u32::MAX - 15 };
pub const SWITCH: MStr = MStr { id: u32::MAX - 16 };
pub const MATCH: MStr = MStr { id: u32::MAX - 17 };
pub const LET: MStr = MStr { id: u32::MAX - 18 };
pub const CONST: MStr = MStr { id: u32::MAX - 19 };
pub const TYPE: MStr = MStr { id: u32::MAX - 20 };
pub const PUB: MStr = MStr { id: u32::MAX - 21 };
pub const MOD: MStr = MStr { id: u32::MAX - 22 };
pub const USE: MStr = MStr { id: u32::MAX - 23 };
pub const MUT: MStr = MStr { id: u32::MAX - 24 };
pub const VAR: MStr = MStr { id: u32::MAX - 25 };
pub const OR: MStr = MStr { id: u32::MAX - 26 };
pub const WRAP: MStr = MStr { id: u32::MAX - 27 };
pub const U8: MStr = MStr { id: u32::MAX - 28 };
pub const U16: MStr = MStr { id: u32::MAX - 29 };
pub const U32: MStr = MStr { id: u32::MAX - 30 };
pub const U64: MStr = MStr { id: u32::MAX - 31 };
pub const I8: MStr = MStr { id: u32::MAX - 32 };
pub const I16: MStr = MStr { id: u32::MAX - 33 };
pub const I32: MStr = MStr { id: u32::MAX - 34 };
pub const I64: MStr = MStr { id: u32::MAX - 35 };
pub const F32: MStr = MStr { id: u32::MAX - 36 };
pub const F64: MStr = MStr { id: u32::MAX - 37 };
pub const BOOL: MStr = MStr { id: u32::MAX - 38 };
pub const PANIC: MStr = MStr { id: u32::MAX - 39 };
pub const SIZEOF: MStr = MStr { id: u32::MAX - 40 };
pub const ALIGNOF: MStr = MStr { id: u32::MAX - 41 };
pub const INNER_LET: MStr = MStr { id: u32::MAX - 42 };
pub const INIT: MStr = MStr { id: u32::MAX - 43 };
pub const UNDERSCORE: MStr = MStr { id: u32::MAX - 44 };
pub const FREE: MStr = MStr { id: u32::MAX - 45 };
pub const ALLOC: MStr = MStr { id: u32::MAX - 46 };
pub const MATCH_TARGET: MStr = MStr { id: u32::MAX - 47 };
pub const PRINT: MStr = MStr { id: u32::MAX - 48 };
pub const EPRINT: MStr = MStr { id: u32::MAX - 49 };

pub struct MStrBuilder<'s> {
    str_map: HashMap<&'s str, u32>,
    next_id: u32,
}

impl<'s> MStrBuilder<'s> {
    pub fn new() -> Self {
        MStrBuilder {
            str_map: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn get_mstr(&mut self, s: &'s str) -> MStr {
        match self.str_map.get(s) {
            Some(id) => MStr { id: *id },
            None => {
                self.str_map.insert(s, self.next_id);
                self.next_id += 1;

                MStr {
                    id: self.next_id - 1,
                }
            }
        }
    }
}

pub struct StrStore {
    offsets: Vec<u32>,
    str_data: String,
}

impl<const N: usize> From<[(&'static str, u32); N]> for StrStore {
    fn from(pairs: [(&'static str, u32); N]) -> Self {
        for (s, _) in pairs {
            store.get_id(s);
        }
        store
    }
}

impl StrStore {
    pub fn new(builder: &MStrBuilder) -> Self {
        let mut len_vec = vec![0; builder.next_id as usize];
        let mut str_vec = vec![None; builder.next_id as usize];

        for (ptr, id) in &builder.str_map {
            len_vec[*id as usize] = ptr.len();
            str_vec[*id as usize] = Some(*ptr)
        }

        let mut offsets = vec![];
        let mut current_offset = 0u32;
        let mut str_data = String::new();
        for (len, ptr) in len_vec.iter().zip(str_vec.iter()) {
            offsets.push(current_offset);
            current_offset += *len as u32;

            let s = ptr.expect("failed to get string from builder");
            str_data += s;
        }

        StrStore { offsets, str_data }
    }

    pub fn get_str<'s>(&'s self, mstr: MStr) -> &'s str {
        let id = mstr.id as usize;
        let start = self.offsets[id] as usize;
        let end = match self.offsets.get(id + 1) {
            Some(end) => *end as usize,
            None => self.str_data.len(),
        };

        return &self.str_data[start..end];
    }
}

/*
/// A type alias for string identifiers. Used to efficiently reference interned strings
/// without storing duplicate string data.
/// StrID types are safe to compare like strings since the same string will always map to the
/// same StrID
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct StrID(usize);

impl StrID {
    pub fn from_usize(id: usize) -> Self {
        StrID(id)
    }

    pub fn to_usize(self) -> usize {
        self.0
    }
}

// Some key strings are hard coded as package constants these include type names
// and internal string values
// nil is is used to represent an empty StrID
//
// TODO: using usize here makes the tests platform dependent since on a 32-bit platform the
// constant values will actually be different. Do we care that you won't be able to build/test the
// complier on 32-bit systems? For now no, but what systems still in play use 32-bit architectures?
// Is it just embedded systems?
pub const NIL: StrID = StrID(usize::MAX);

pub const U8: StrID = StrID(usize::MAX - 1);
pub const U16: StrID = StrID(usize::MAX - 2);
pub const U32: StrID = StrID(usize::MAX - 3);
pub const U64: StrID = StrID(usize::MAX - 4);

pub const I8: StrID = StrID(usize::MAX - 5);
pub const I16: StrID = StrID(usize::MAX - 6);
pub const I32: StrID = StrID(usize::MAX - 7);
pub const I64: StrID = StrID(usize::MAX - 8);

pub const F32: StrID = StrID(usize::MAX - 9);
pub const F64: StrID = StrID(usize::MAX - 10);

pub const STR: StrID = StrID(usize::MAX - 11);
pub const BOOL: StrID = StrID(usize::MAX - 12);

pub const WRAP: StrID = StrID(usize::MAX - 13);
pub const PANIC: StrID = StrID(usize::MAX - 14);

// these are used for the internal meta struct for type information
pub const SIZEOF: StrID = StrID(usize::MAX - 15);
pub const ALIGNOF: StrID = StrID(usize::MAX - 16);
pub const METAFLAGS: StrID = StrID(usize::MAX - 17);
pub const INNERLET: StrID = StrID(usize::MAX - 18);

pub const INIT: StrID = StrID(usize::MAX - 19);
pub const UNDERSCORE: StrID = StrID(usize::MAX - 20);

pub const DEFER: StrID = StrID(usize::MAX - 21);
pub const FREE: StrID = StrID(usize::MAX - 22);
pub const ALLOC: StrID = StrID(usize::MAX - 23);
pub const MATCH_TARGET: StrID = StrID(usize::MAX - 24);
pub const PRINT: StrID = StrID(usize::MAX - 25);
pub const EPRINT: StrID = StrID(usize::MAX - 26);

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
        "free" => Some(FREE),
        "alloc" => Some(ALLOC),
        "print" => Some(PRINT),
        "eprint" => Some(EPRINT),

        "_" => Some(UNDERSCORE),

        // these are not a valid identifier so we can use it in the compiler
        // without worrying about conflicting with user identifiers
        "<wrap>" => Some(WRAP),
        "<inner let>" => Some(INNERLET),
        "<init>" => Some(INIT),
        "<defer>" => Some(DEFER),
        "<match_target>" => Some(MATCH_TARGET),
        _ => None,
    }
}

pub fn constant_id_str(id: StrID) -> Option<&'static str> {
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

        // keywords used throughout the complier
        PANIC => Some("panic"),
        SIZEOF => Some("size_of"),
        ALIGNOF => Some("align_of"),
        METAFLAGS => Some("flags"),
        FREE => Some("free"),
        ALLOC => Some("alloc"),
        PRINT => Some("print"),
        EPRINT => Some("eprint"),
        UNDERSCORE => Some("_"),

        // this is not a valid identifier so we can use it in the compiler
        // without worrying about conflicting with user identifiers
        WRAP => Some("<wrap>"),
        INNERLET => Some("<inner let>"),
        INIT => Some("<init>"),
        DEFER => Some("<defer>"),
        MATCH_TARGET => Some("<match target>"),

        // Nil is also not a valid identifier
        NIL => Some("<nil>"),
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
#[derive(Debug, Clone, Serialize, Deserialize)]
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
                let id = StrID(self.next_id);
                self.next_id += 1;
                self.strings.insert(s.to_string(), id);
                self.reverse_strings.push(s.to_string());
                id
            }
        }
    }

    /// Returns the interned ID for a given string if it exists in the store.
    pub fn find_id(&self, s: &str) -> Option<StrID> {
        match constant_str_id(s) {
            Some(id) => Some(id),
            None => self.strings.get(s).cloned(),
        }
    }

    /// Look up the string for a given ID. Returns None if ID not found.
    pub fn get_string(&self, id: StrID) -> Option<String> {
        match constant_id_str(id) {
            Some(id) => Some(id.to_string()),
            None => self.reverse_strings.get(id.to_usize()).cloned(),
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
        assert_eq!(id, StrID(0));
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
        assert_eq!(id1, StrID(0));
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
*/
