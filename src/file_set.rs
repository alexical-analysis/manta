use std::error::Error;
use std::io;
use std::{fs, path::PathBuf};

pub struct File {
    pub name: String,
    pub source: String,
    base: u32,
}

impl File {
    pub fn new(name: String, source: String) -> Self {
        File {
            name,
            source,
            base: 0,
        }
    }
}

pub struct FileSet {
    root_dir: PathBuf,
    files: Vec<File>,
}

impl FileSet {
    pub fn new_from_dir(dir: PathBuf) -> io::Result<Self> {
        let mut files = vec![];
        let mut base = 0u32;

        for entry in fs::read_dir(&dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                continue;
            }

            let ext = path.extension().and_then(|s| s.to_str());
            if !matches!(ext, Some("manta")) {
                continue;
            }

            let source = fs::read_to_string(&path)?;
            let size = source.len() as u32;
            let name = path
                .strip_prefix(&dir)
                .expect("failed to strip dir prefix from path name")
                .to_string_lossy()
                .into_owned();

            files.push(File { name, source, base });

            // +1 here represents the files EOF even though that's not technically
            // a byte from the file. It makes it possible to track the correct file
            // in the FileSet down using only a SourceId
            base += size + 1;
        }

        Ok(FileSet {
            root_dir: dir,
            files,
        })
    }

    pub fn new_from_files(root_dir: PathBuf, files: Vec<File>) -> Self {
        let mut files = files;
        let mut base = 0u32;

        // update all the file bases so things are aligned and the caller dosen't need to think about
        // sourc lengths for stuff to be set up correctly
        for file in &mut files {
            file.base = base;

            let size = file.source.len() as u32;

            // +1 here represents the files EOF even though that's not technically
            // a byte from the file. It makes it possible to track the correct file
            // in the FileSet down using only a SourceId
            base += size + 1;
        }

        FileSet { root_dir, files }
    }

    pub fn files(&self) -> &Vec<File> {
        &self.files
    }

    pub fn line_count(&self) -> usize {
        self.files
            .iter()
            .map(|f| {
                f.source
                    .lines()
                    .filter(|l| !l.trim().is_empty())
                    .filter(|l| !l.trim_start().starts_with("//"))
                    .count()
            })
            .sum()
    }
}

pub fn gather_file_sets(root_dir: PathBuf) -> Result<Vec<FileSet>, Box<dyn Error>> {
    let mut file_sets = vec![];

    for entry in fs::read_dir(&root_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            let mut nested = gather_file_sets(path)?;
            file_sets.append(&mut nested);
        }
    }

    let root = FileSet::new_from_dir(root_dir)?;
    file_sets.push(root);

    // filter out any empty file sets
    let file_sets = file_sets
        .into_iter()
        .filter(|fs| !fs.files.is_empty())
        .collect();

    Ok(file_sets)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    /// Creates a temporary directory populated with the given files.
    /// Each entry is `(relative_path, content)`, e.g. `("src/main.manta", "fn main() {}")`.
    /// Intermediate directories are created automatically. The directory is deleted when the
    /// returned `TempDir` is dropped.
    fn make_project(files: &[(&str, &str)]) -> tempfile::TempDir {
        let dir = tempdir().unwrap();
        for (rel_path, content) in files {
            let path = dir.path().join(rel_path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, content).unwrap();
        }
        dir
    }

    #[test]
    fn test_base_of_first_file_is_zero() {
        let dir = make_project(&[("main.manta", "fn main() {}")]);
        let fs = FileSet::new_from_dir(dir.path().to_path_buf()).unwrap();
        assert_eq!(fs.files[0].base, 0);
    }

    #[test]
    fn test_bases_are_non_overlapping() {
        let dir = make_project(&[("a.manta", "hello"), ("b.manta", "world"), ("c.manta", "!")]);
        let files = FileSet::new_from_dir(dir.path().to_path_buf())
            .unwrap()
            .files;
        // Each file's base must equal the previous file's base + source len + 1
        for window in files.windows(2) {
            let expected = window[0].base + window[0].source.len() as u32 + 1;
            assert_eq!(window[1].base, expected);
        }
    }

    #[test]
    fn test_empty_file_advances_base_by_one() {
        let dir = make_project(&[("a.manta", "hello"), ("b.manta", ""), ("c.manta", "")]);
        let files = FileSet::new_from_dir(dir.path().to_path_buf())
            .unwrap()
            .files;
        // Each file's base must equal the previous file's base + source len + 1
        // even if the file is empty
        for window in files.windows(2) {
            let expected = window[0].base + window[0].source.len() as u32 + 1;
            assert_eq!(window[1].base, expected);
        }
    }

    #[test]
    fn test_non_manta_files_are_ignored() {
        let dir = make_project(&[
            ("main.manta", "fn main() {}"),
            ("readme.txt", "ignored"),
            ("notes.md", "also ignored"),
        ]);
        let fs = FileSet::new_from_dir(dir.path().to_path_buf()).unwrap();
        assert_eq!(fs.files.len(), 1);
        assert_eq!(fs.files[0].name, "main.manta");
    }

    #[test]
    fn test_nested_directories_are_skipped() {
        let dir = make_project(&[
            ("top.manta", "fn top() {}"),
            ("sub/nested.manta", "fn nested() {}"),
            ("sub2/nested.manta", "fn nested2() {}"),
        ]);
        let fs = FileSet::new_from_dir(dir.path().to_path_buf()).unwrap();
        assert_eq!(fs.files.len(), 1);
        assert_eq!(fs.files[0].name, "top.manta");
    }

    #[test]
    fn test_empty_directory_produces_empty_fileset() {
        let dir = make_project(&[]);
        let fs = FileSet::new_from_dir(dir.path().to_path_buf()).unwrap();
        assert!(fs.files.is_empty());
    }

    #[test]
    fn test_gather_file_sets_flat() {
        let dir = make_project(&[("main.manta", "fn main() {}")]);
        let sets = gather_file_sets(dir.path().to_path_buf()).unwrap();
        assert_eq!(sets.len(), 1);
        assert_eq!(sets[0].files.len(), 1);
        assert_eq!(sets[0].files[0].name, "main.manta");
    }

    #[test]
    fn test_gather_file_sets_nested() {
        let dir = make_project(&[
            ("main.manta", "fn main() {}"),
            ("sub/util.manta", "fn util() {}"),
            ("sub/helper.manta", "fn helper() {}"),
        ]);
        let sets = gather_file_sets(dir.path().to_path_buf()).unwrap();
        assert_eq!(sets.len(), 2);

        let total_files: usize = sets.iter().map(|s| s.files.len()).sum();
        assert_eq!(total_files, 3);

        let has_main = sets
            .iter()
            .any(|s| s.files.iter().any(|f| f.name == "main.manta"));
        let has_util = sets
            .iter()
            .any(|s| s.files.iter().any(|f| f.name == "util.manta"));
        assert!(has_main);
        assert!(has_util);
    }

    #[test]
    fn test_gather_file_sets_deeply_nested() {
        let dir = make_project(&[
            ("a.manta", "fn a() {}"),
            ("sub/b.manta", "fn b() {}"),
            ("sub/deep/c.manta", "fn c() {}"),
        ]);
        let sets = gather_file_sets(dir.path().to_path_buf()).unwrap();
        assert_eq!(sets.len(), 3);

        let total_files: usize = sets.iter().map(|s| s.files.len()).sum();
        assert_eq!(total_files, 3);
    }

    #[test]
    fn test_gather_file_sets_excludes_empty_dirs() {
        // Only the root and "full" subdirs have .manta files; "empty" has none.
        let dir = make_project(&[
            ("root.manta", "fn root() {}"),
            ("full/mod.manta", "fn mod_fn() {}"),
            ("empty/README.md", "not manta"),
        ]);
        let sets = gather_file_sets(dir.path().to_path_buf()).unwrap();
        // "empty" dir should be excluded; only root and "full" remain
        assert_eq!(sets.len(), 2);
        for s in &sets {
            assert!(!s.files.is_empty());
        }
    }

    #[test]
    fn test_gather_file_sets_all_empty_returns_empty() {
        let dir = make_project(&[
            ("sub1/notes.txt", "nothing"),
            ("sub2/readme.md", "also nothing"),
        ]);
        let sets = gather_file_sets(dir.path().to_path_buf()).unwrap();
        assert!(sets.is_empty());
    }
}
