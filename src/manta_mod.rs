use std::error::Error;
use std::fmt::{self, Display};
use std::io::{self, BufRead, Write};
use std::str::FromStr;

// Semver is specifically a semver representation used to track the manta version in the manta.mod
pub struct Semver {
    major: u32,
    minor: u32,
    patch: u32,
}

impl Semver {
    pub fn new_zero() -> Self {
        Semver {
            major: 0,
            minor: 0,
            patch: 0,
        }
    }
}

impl FromStr for Semver {
    type Err = String; // or a custom error type

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // parse logic here
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 3 {
            return Err(format!(
                "invalid semver, expected 3 components, got {}",
                parts.len()
            ));
        }

        Ok(Semver {
            major: parts[0].trim().parse().map_err(|e| format!("major: {e}"))?,
            minor: parts[1].trim().parse().map_err(|e| format!("minor: {e}"))?,
            patch: parts[2].trim().parse().map_err(|e| format!("patch: {e}"))?,
        })
    }
}

impl Display for Semver {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

// MantaMod is a struct that represents a manta.mod file
pub struct MantaMod {
    pub manta_version: Semver,
    pub project_name: String,
}

impl MantaMod {
    pub fn new(manta_version: Semver, name: String) -> Self {
        MantaMod {
            manta_version,
            project_name: name,
        }
    }

    pub fn from_reader(mut r: impl BufRead) -> Result<Self, Box<dyn Error>> {
        let mut mod_file = String::new();
        r.read_to_string(&mut mod_file)?;

        let line: Vec<_> = mod_file.split("\n").collect();
        if line.len() < 2 {
            return Err("invalid manta module file".into());
        }

        let semver_line = match line[0].strip_prefix("manta ") {
            Some(s) => s,
            None => return Err("invalid manta version line".into()),
        };

        let manta_version: Semver = semver_line.parse()?;
        let project_name = line[1].trim().to_string();
        if project_name.is_empty() {
            return Err("manta.mod missing project name".into());
        }

        Ok(MantaMod {
            manta_version,
            project_name,
        })
    }
}

impl MantaMod {
    pub fn write_to(&self, mut w: impl Write) -> Result<(), io::Error> {
        write!(w, "manta {}\n{}\n", self.manta_version, self.project_name)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    // --- Semver tests ---

    #[test]
    fn semver_parse_valid() {
        let v: Semver = "1.2.3".parse().unwrap();
        assert_eq!(v.to_string(), "1.2.3");
    }

    #[test]
    fn semver_parse_zero() {
        let v: Semver = "0.0.0".parse().unwrap();
        assert_eq!(v.to_string(), "0.0.0");
    }

    #[test]
    fn semver_new_zero_display() {
        assert_eq!(Semver::new_zero().to_string(), "0.0.0");
    }

    #[test]
    fn semver_parse_too_few_parts() {
        assert!("1.2".parse::<Semver>().is_err());
    }

    #[test]
    fn semver_parse_too_many_parts() {
        assert!("1.2.3.4".parse::<Semver>().is_err());
    }

    #[test]
    fn semver_parse_non_numeric() {
        assert!("1.x.3".parse::<Semver>().is_err());
    }

    #[test]
    fn semver_parse_negative_rejected() {
        assert!("-1.0.0".parse::<Semver>().is_err());
    }

    // --- MantaMod round-trip ---

    #[test]
    fn manta_mod_write_then_read() {
        let original = MantaMod::new(Semver::new_zero(), "my_project".to_string());

        let mut buf = Vec::new();
        original.write_to(&mut buf).unwrap();

        let parsed = MantaMod::from_reader(Cursor::new(buf)).unwrap();
        assert_eq!(parsed.project_name, "my_project");
        assert_eq!(parsed.manta_version.to_string(), "0.0.0");
    }

    #[test]
    fn manta_mod_write_format() {
        let m = MantaMod::new("1.2.3".parse().unwrap(), "myproj".to_string());
        let mut buf = Vec::new();
        m.write_to(&mut buf).unwrap();
        assert_eq!(String::from_utf8(buf).unwrap(), "manta 1.2.3\nmyproj\n");
    }

    // --- MantaMod::from_reader error cases ---

    #[test]
    fn from_reader_missing_version_prefix() {
        let input = Cursor::new("1.0.0\nmy_project\n");
        assert!(MantaMod::from_reader(input).is_err());
    }

    #[test]
    fn from_reader_invalid_semver() {
        let input = Cursor::new("manta 1.2\nmy_project\n");
        assert!(MantaMod::from_reader(input).is_err());
    }

    #[test]
    fn from_reader_empty_project_name() {
        let input = Cursor::new("manta 0.0.0\n\n");
        assert!(MantaMod::from_reader(input).is_err());
    }

    #[test]
    fn from_reader_only_version_line() {
        let input = Cursor::new("manta 0.0.0");
        assert!(MantaMod::from_reader(input).is_err());
    }

    #[test]
    fn from_reader_trims_project_name_whitespace() {
        let input = Cursor::new("manta 0.0.0\n  spaced_name  \n");
        let m = MantaMod::from_reader(input).unwrap();
        assert_eq!(m.project_name, "spaced_name");
    }
}
