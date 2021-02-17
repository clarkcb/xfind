use std::collections::HashSet;
use std::collections::VecDeque;
use std::error::Error;
use std::fs;
use std::io;
use std::io::BufReader;
use std::io::Read;
use std::path::Path;
use std::str::Lines;

use encoding::codec::singlebyte::SingleByteEncoding;
use encoding::{label, DecoderTrap, Encoding};
use regex::{Match, Regex};
use walkdir::WalkDir;
use zip::read::ZipFile;

use crate::common::log;
use crate::filetypes::{FileType, FileTypes};
use crate::fileutil::FileUtil;
use crate::finderror::FindError;
use crate::findfile::FindFile;
use crate::findresult::FindResult;
use crate::findsettings::FindSettings;

const BINARY_ENCODING: &SingleByteEncoding = encoding::all::ISO_8859_1;

pub struct Finder {
    pub filetypes: FileTypes,
    pub settings: FindSettings,
}

impl Finder {
    /// Create a new Finder instance for the given settings, if valid
    pub fn new(settings: FindSettings) -> Result<Finder, FindError> {
        let filetypes = match FileTypes::new() {
            Ok(filetypes) => filetypes,
            Err(error) => return Err(error),
        };

        if let Err(error) = Finder::validate_settings(&settings) {
            return Err(error);
        }

        Ok(Finder {
            filetypes,
            settings,
        })
    }

    fn validate_settings(settings: &FindSettings) -> Result<(), FindError> {
        if settings.startpath == "" {
            return Err(FindError::new("Startpath not defined"));
        }
        let metadata = fs::metadata(&settings.startpath);
        if metadata.is_err() {
            match metadata.err().unwrap().kind() {
                io::ErrorKind::NotFound => return Err(FindError::new("Startpath not found")),
                io::ErrorKind::PermissionDenied => {
                    return Err(FindError::new("Startpath not readable"))
                },
                _ => {
                    return Err(FindError::new(
                        "An unknown error occurred trying to read startpath",
                    ))
                }
            }
        }
        if settings.find_patterns.is_empty() {
            return Err(FindError::new("No find patterns defined"));
        }
        if let None = label::encoding_from_whatwg_label(&settings.text_file_encoding) {
            return Err(FindError::new(
                "Invalid or unsupported text file encoding",
            ));
        }
        Ok(())
    }

    fn get_text_file_encoding(&self) -> &'static dyn Encoding {
        label::encoding_from_whatwg_label(&self.settings.text_file_encoding).unwrap()
    }

    fn matches_any_string(&self, string: &str, strings: &[String]) -> bool {
        strings.iter().any(|s| s == string)
    }

    fn matches_any_pattern(&self, s: &String, patterns: &[Regex]) -> bool {
        for p in patterns.iter() {
            if p.is_match(s) {
                return true;
            }
        }
        false
    }

    fn any_matches_any_pattern(&self, ss: &[String], patterns: &[Regex]) -> bool {
        for s in ss.iter() {
            if self.matches_any_pattern(s, patterns) {
                return true;
            }
        }
        false
    }

    fn is_find_dir(&self, dir: &String) -> bool {
        if FileUtil::is_hidden(&dir) && self.settings.exclude_hidden {
            return false;
        }
        (self.settings.in_dir_patterns.is_empty()
            || self.matches_any_pattern(&dir, &self.settings.in_dir_patterns))
            && (self.settings.out_dir_patterns.is_empty()
                || !self.matches_any_pattern(&dir, &self.settings.out_dir_patterns))
    }

    fn is_archive_find_file(&self, findfile: &FindFile) -> bool {
        if !self.is_find_dir(&findfile.path) {
            return false;
        }
        if FileUtil::is_hidden(&findfile.name) && self.settings.exclude_hidden {
            return false;
        }
        (self.settings.in_archive_file_patterns.is_empty()
            || self.matches_any_pattern(&findfile.name, &self.settings.in_archive_file_patterns))
            && (self.settings.out_archive_file_patterns.is_empty()
                || !self.matches_any_pattern(
                    &findfile.name,
                    &self.settings.out_archive_file_patterns,
                ))
    }

    fn is_find_file(&self, findfile: &FindFile) -> bool {
        if !self.is_find_dir(&findfile.path) {
            return false;
        }
        if FileUtil::is_hidden(&findfile.name) && self.settings.exclude_hidden {
            return false;
        }
        if let Some(ext) = FileUtil::get_extension(&findfile.name) {
            if (!self.settings.in_extensions.is_empty()
                && !self.matches_any_string(ext, &self.settings.in_extensions))
                || (!self.settings.out_extensions.is_empty()
                    && self.matches_any_string(ext, &self.settings.out_extensions))
            {
                return false;
            }
        }
        (self.settings.in_file_patterns.is_empty()
            || self.matches_any_pattern(&findfile.name, &self.settings.in_file_patterns))
            && (self.settings.in_file_types.is_empty()
                || self.settings.in_file_types.contains(&findfile.filetype))
            && (self.settings.out_file_patterns.is_empty()
                || !self.matches_any_pattern(&findfile.name, &self.settings.out_file_patterns))
            && (self.settings.out_file_types.is_empty()
                || !self.settings.out_file_types.contains(&findfile.filetype))
    }

    fn filter_file(&self, findfile: &FindFile) -> bool {
        if findfile.filetype == FileType::Archive {
            return self.settings.find_archives && self.is_archive_find_file(findfile);
        }
        !self.settings.archives_only && self.is_find_file(findfile)
    }

    /// Initiate a finder find for the given settings and get the results
    pub fn find(&self) -> Result<Vec<FindResult>, FindError> {
        let mut findfiles: Vec<FindFile> = Vec::new();
        for entry in WalkDir::new(&self.settings.startpath)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
        {
            let path = match entry.path().parent() {
                Some(parent) => parent.to_str().unwrap().to_string(),
                None => ".".to_string(),
            };
            let filename = entry.file_name().to_str().unwrap().to_string();
            let filetype = self.filetypes.get_file_type(&filename);
            if filetype == FileType::Unknown {
                continue;
            }
            let findfile = FindFile::new(path, filename, filetype);
            if self.filter_file(&findfile) {
                findfiles.push(findfile)
            }
        }

        if self.settings.verbose && !findfiles.is_empty() {
            let mut dirs: Vec<String> = findfiles.iter().map(|f| f.path.clone()).collect();
            dirs.sort_unstable();
            dirs.dedup();
            log(format!("\nDirectories to be found ({}):", dirs.len()).as_str());
            for dir in dirs.iter() {
                log(format!("{}", dir).as_str());
            }

            let mut files: Vec<String> = findfiles.iter().map(|f| f.filepath()).collect();
            files.sort_unstable();
            log(format!("\nFiles to be found ({}):", files.len()).as_str());
            for file in files.iter() {
                log(format!("{}", file).as_str());
            }
        }

        self.find_files(&findfiles)
    }

    fn find_files(&self, findfiles: &[FindFile]) -> Result<Vec<FindResult>, FindError> {
        let mut results: Vec<FindResult> = Vec::new();
        for sf in findfiles.iter() {
            match self.find_file(&sf) {
                Ok(mut file_results) => {
                    results.append(&mut file_results);
                },
                Err(error) => {
                    log(format!("{}", error).as_str());
                    return Err(error);
                }
            }
        }
        results.sort_unstable();
        Ok(results)
    }

    /// Find an individual file and get the results
    pub fn find_file(&self, findfile: &FindFile) -> Result<Vec<FindResult>, FindError> {
        match findfile.filetype {
            FileType::Text | FileType::Code | FileType::Xml => self.find_text_file(findfile),
            FileType::Binary => self.find_binary_file(findfile),
            FileType::Archive => {
                if self.settings.find_archives {
                    self.find_archive_file(findfile)
                } else {
                    log(format!("Skipping archive file: {}", findfile.filepath()).as_str());
                    Ok(vec![])
                }
            },
            _ => {
                log(format!("Skipping unknown file type: {:?}", findfile.filetype).as_str());
                Ok(vec![])
            }
        }
    }

    fn find_archive_file(
        &self,
        findfile: &FindFile,
    ) -> Result<Vec<FindResult>, FindError> {
        if self.settings.verbose {
            log(format!("Finding archive file {}", findfile.filepath()).as_str());
        }

        match FileUtil::get_extension(&findfile.name) {
            // TODO: what other extensions are zip format?
            Some(ext) if ["zip", "zipx", "jar", "war", "ear", "whl"].contains(&ext) => {
                self.find_archive_zip_file(findfile)
            },
            Some(ext) => {
                log(format!("Finding not currently supported for {} files", ext).as_str());
                Ok(vec![])
            },
            None => {
                log(format!(
                    "Skipping unknown archive file of unknown type: {}",
                    findfile.filepath()
                )
                .as_str());
                Ok(vec![])
            }
        }
    }

    fn find_archive_zip_file(
        &self,
        findfile: &FindFile,
    ) -> Result<Vec<FindResult>, FindError> {
        if self.settings.verbose {
            log(format!("Finding zip file {}", findfile.filepath()).as_str());
        }
        let mut results: Vec<FindResult> = Vec::new();
        match fs::File::open(findfile.filepath()) {
            Ok(f) => {
                let reader = BufReader::new(f);
                let mut archive = match zip::ZipArchive::new(reader) {
                    Ok(archive) => archive,
                    Err(error) => return Err(FindError::new(error.description())),
                };
                for i in 0..archive.len() {
                    let mut zipfile = match archive.by_index(i) {
                        Ok(zipfile) => zipfile,
                        Err(error) => return Err(FindError::new(error.description())),
                    };
                    if zipfile.is_file() {
                        match self.find_zip_file(findfile, &mut zipfile) {
                            Ok(mut zip_results) => results.append(&mut zip_results),
                            Err(error) => return Err(error),
                        }
                    }
                }
            },
            Err(error) => {
                return Err(FindError::new(error.description()));
            }
        }
        Ok(results)
    }

    fn find_zip_file(
        &self,
        findfile: &FindFile,
        zipfile: &mut ZipFile,
    ) -> Result<Vec<FindResult>, FindError> {
        let path = if let Some(path) = Path::new(zipfile.name()).parent() {
            path.to_str().unwrap()
        } else {
            ""
        };
        let filename = Path::new(zipfile.name())
            .file_name()
            .unwrap()
            .to_str()
            .unwrap();
        let mut containers = vec![];
        for c in findfile.containers.iter() {
            containers.push(c.clone());
        }
        containers.push(findfile.filepath());
        let zip_findfile = FindFile::with_containers(
            containers,
            path.to_string(),
            filename.to_string(),
            self.filetypes.get_file_type(filename),
        );
        if self.is_find_file(&zip_findfile) {
            match zip_findfile.filetype {
                FileType::Text | FileType::Code | FileType::Xml => {
                    let encoding = self.get_text_file_encoding();
                    let zip_contents = match self.get_text_reader_contents(zipfile, encoding) {
                        Ok(bytestring) => bytestring,
                        Err(error) => return Err(error),
                    };

                    let results = if self.settings.multiline_find {
                        self.find_multiline_string(&zip_contents)
                    } else {
                        let mut lines = zip_contents.lines();
                        self.find_text_lines(&mut lines)
                    };
                    return Ok(results
                        .iter()
                        .map(|r| {
                            FindResult::new(
                                r.pattern.clone(),
                                Some(zip_findfile.clone()),
                                r.line_num,
                                r.match_start_index,
                                r.match_end_index,
                                r.line.clone(),
                                r.lines_before.clone(),
                                r.lines_after.clone(),
                            )
                        })
                        .collect());
                },
                FileType::Binary => match self.get_byte_string_for_reader(zipfile) {
                    Ok(bytestring) => match self.find_binary_byte_string(&bytestring) {
                        Ok(results) => {
                            return Ok(results
                                .iter()
                                .map(|r| {
                                    FindResult::new(
                                        r.pattern.clone(),
                                        Some(zip_findfile.clone()),
                                        0,
                                        r.match_start_index,
                                        r.match_end_index,
                                        "".to_string(),
                                        Vec::new(),
                                        Vec::new(),
                                    )
                                })
                                .collect())
                        }
                        Err(error) => return Err(error),
                    },
                    Err(error) => return Err(FindError::new(&error.to_string())),
                },
                _ => {}
            }
        }
        Ok(vec![])
    }

    fn get_encoded_byte_string_for_reader(
        &self,
        reader: &mut dyn Read,
        enc: &dyn Encoding,
    ) -> Result<String, FindError> {
        let mut buffer: Vec<u8> = Vec::new();
        match reader.read_to_end(&mut buffer) {
            Ok(_) => match enc.decode(&buffer, DecoderTrap::Strict) {
                Ok(bytestring) => Ok(bytestring),
                Err(cow) => return Err(FindError::new(&cow.to_string())),
            },
            Err(error) => {
                return Err(FindError::new(error.description()));
            }
        }
    }

    fn get_encoded_byte_string(
        &self,
        findfile: &FindFile,
        enc: &dyn Encoding,
    ) -> Result<String, FindError> {
        match fs::File::open(findfile.filepath()) {
            Ok(mut f) => self.get_encoded_byte_string_for_reader(&mut f, enc),
            Err(error) => Err(FindError::new(error.description())),
        }
    }

    fn get_byte_string_for_reader(&self, reader: &mut dyn Read) -> Result<String, FindError> {
        self.get_encoded_byte_string_for_reader(reader, BINARY_ENCODING)
    }

    fn get_byte_string(&self, findfile: &FindFile) -> Result<String, FindError> {
        self.get_encoded_byte_string(findfile, BINARY_ENCODING)
    }

    fn find_binary_file(
        &self,
        findfile: &FindFile,
    ) -> Result<Vec<FindResult>, FindError> {
        if self.settings.verbose {
            log(format!("Finding binary file {}", findfile.filepath()).as_str());
        }
        let mut results: Vec<FindResult> = Vec::new();
        match self.get_byte_string(findfile) {
            Ok(bytestring) => match self.find_binary_byte_string(&bytestring) {
                Ok(rs) => {
                    for r in rs.iter() {
                        results.push(FindResult::new(
                            r.pattern.clone(),
                            Some(findfile.clone()),
                            0,
                            r.match_start_index,
                            r.match_end_index,
                            "".to_string(),
                            Vec::new(),
                            Vec::new(),
                        ));
                    }
                },
                Err(error) => return Err(error),
            },
            Err(error) => return Err(error),
        }
        Ok(results)
    }

    fn find_binary_byte_string(
        &self,
        bytestring: &String,
    ) -> Result<Vec<FindResult>, FindError> {
        let mut results: Vec<FindResult> = Vec::new();
        for p in &self.settings.find_patterns {
            let matches: Vec<Match> = if self.settings.first_match {
                match p.find(&bytestring) {
                    Some(m) => vec![m],
                    None => vec![],
                }
            } else {
                p.find_iter(&bytestring).collect()
            };
            for m in matches {
                let r = FindResult::new(
                    p.as_str().to_string(),
                    None,
                    0,
                    m.start() + 1,
                    m.end() + 1,
                    "".to_string(),
                    Vec::new(),
                    Vec::new(),
                );
                results.push(r);
            }
        }
        Ok(results)
    }

    /// Try to get the reader contents for the given encoding (UTF-8 by default)
    fn get_text_reader_contents(
        &self,
        reader: &mut dyn Read,
        encoding: &'static dyn Encoding,
    ) -> Result<String, FindError> {
        let mut into_string = String::from("");
        let contents = match encoding.name() {
            "utf-8" => match reader.read_to_string(&mut into_string) {
                Ok(_) => into_string,
                Err(error) if error.kind() == io::ErrorKind::InvalidData => {
                    match self.get_byte_string_for_reader(reader) {
                        Ok(bytestring) => bytestring,
                        Err(error) => return Err(error),
                    }
                },
                Err(error) => {
                    //let msg = format!("{} (file: {}", error.description(), findfile.filepath());
                    return Err(FindError::new(error.description()));
                }
            },
            _ => match self.get_encoded_byte_string_for_reader(reader, encoding) {
                Ok(bytestring) => bytestring,
                Err(error) => return Err(error),
            },
        };
        Ok(contents)
    }

    /// Try to get the file contents for the given encoding (UTF-8 by default)
    fn get_text_file_contents(
        &self,
        findfile: &FindFile,
        encoding: &'static dyn Encoding,
    ) -> Result<String, FindError> {
        match fs::File::open(findfile.filepath()) {
            Ok(mut f) => self.get_text_reader_contents(&mut f, encoding),
            Err(error) => Err(FindError::new(error.description())),
        }
    }

    fn find_text_file(&self, findfile: &FindFile) -> Result<Vec<FindResult>, FindError> {
        let encoding = self.get_text_file_encoding();
        if self.settings.verbose {
            log(format!("Finding text file {}", findfile.filepath()).as_str());
        }
        if self.settings.multiline_find {
            return self.find_text_file_contents(findfile, encoding);
        }
        return self.find_text_file_lines(findfile, encoding);
    }

    fn find_text_file_lines<'a>(
        &self,
        findfile: &'a FindFile,
        encoding: &'static dyn Encoding,
    ) -> Result<Vec<FindResult>, FindError> {
        let contents = match self.get_text_file_contents(findfile, encoding) {
            Ok(contents) => contents,
            Err(error) => return Err(error),
        };
        let mut lines = contents.lines();
        Ok(self
            .find_text_lines(&mut lines)
            .iter()
            .map(|r| {
                FindResult::new(
                    r.pattern.clone(),
                    Some(findfile.clone()),
                    r.line_num,
                    r.match_start_index,
                    r.match_end_index,
                    r.line.clone(),
                    r.lines_before.clone(),
                    r.lines_after.clone(),
                )
            })
            .collect())
    }

    fn lines_match(&self, lines: &[String], in_patterns: &[Regex], out_patterns: &[Regex]) -> bool {
        (in_patterns.is_empty() || self.any_matches_any_pattern(lines, in_patterns))
            && (out_patterns.is_empty() || !self.any_matches_any_pattern(lines, out_patterns))
    }

    /// Find a Lines iterator
    pub fn find_text_lines<'a>(&self, lines: &mut Lines) -> Vec<FindResult> {
        let mut results: Vec<FindResult> = Vec::new();
        let mut current_linenum = 1usize;
        let mut lines_before: VecDeque<&str> = VecDeque::new();
        let mut lines_after: VecDeque<&str> = VecDeque::new();
        let mut matched_patterns: HashSet<&str> = HashSet::new();
        loop {
            let line = if lines_after.len() > 0 {
                lines_after.pop_front().unwrap()
            } else if let Some(line) = lines.next() {
                line
            } else {
                break;
            };
            while lines_after.len() < self.settings.lines_after {
                match lines.next() {
                    Some(l) => lines_after.push_back(l),
                    None => break,
                }
            }
            for p in &self.settings.find_patterns {
                for m in p.find_iter(line) {
                    let mut v_lines_before: Vec<String> = Vec::new();
                    if !lines_before.is_empty() {
                        for line_before in lines_before.iter() {
                            v_lines_before.push(line_before.to_string());
                        }
                        if !self.lines_match(
                            &v_lines_before,
                            &self.settings.in_lines_before_patterns,
                            &self.settings.out_lines_before_patterns,
                        ) {
                            continue;
                        }
                    }
                    let mut v_lines_after: Vec<String> = Vec::new();
                    if !lines_after.is_empty() {
                        for line_after in lines_after.iter() {
                            v_lines_after.push(line_after.to_string());
                        }
                        if !self.lines_match(
                            &v_lines_after,
                            &self.settings.in_lines_after_patterns,
                            &self.settings.out_lines_after_patterns,
                        ) {
                            continue;
                        }
                    }

                    let has_lines_after_to_patterns =
                        !self.settings.lines_after_to_patterns.is_empty();
                    let has_lines_after_until_patterns =
                        !self.settings.lines_after_until_patterns.is_empty();
                    if has_lines_after_to_patterns || has_lines_after_until_patterns {
                        let lines_after_patterns = if has_lines_after_to_patterns {
                            &self.settings.lines_after_to_patterns
                        } else {
                            &self.settings.lines_after_until_patterns
                        };

                        let mut lines_after_match =
                            self.any_matches_any_pattern(&v_lines_after, &lines_after_patterns);
                        while !lines_after_match {
                            match lines.next() {
                                Some(line) => {
                                    if self.matches_any_pattern(
                                        &line.to_string(),
                                        &lines_after_patterns,
                                    ) {
                                        if has_lines_after_to_patterns {
                                            lines_after.push_back(line);
                                            v_lines_after.push(line.to_string());
                                        }
                                        lines_after_match = true;
                                    } else {
                                        lines_after.push_back(line);
                                        v_lines_after.push(line.to_string());
                                    }
                                },
                                None => break,
                            }
                        }
                        if !lines_after_match {
                            continue;
                        }
                    }

                    let r = FindResult::new(
                        p.as_str().to_string(),
                        None,
                        current_linenum,
                        m.start() + 1,
                        m.end() + 1,
                        line.to_string().clone(),
                        v_lines_before,
                        v_lines_after,
                    );
                    results.push(r);
                    matched_patterns.insert(p.as_str());
                    if self.settings.first_match
                        && matched_patterns.len() == self.settings.find_patterns.len()
                    {
                        return results;
                    }
                }
            }
            if self.settings.lines_before > 0 {
                if lines_before.len() == self.settings.lines_before {
                    lines_before.pop_front();
                }
                if lines_before.len() < self.settings.lines_before {
                    lines_before.push_back(line);
                }
            }
            current_linenum += 1;
        }
        results
    }

    fn find_text_file_contents(
        &self,
        findfile: &FindFile,
        encoding: &'static dyn Encoding,
    ) -> Result<Vec<FindResult>, FindError> {
        let contents = match self.get_text_file_contents(findfile, encoding) {
            Ok(contents) => contents,
            Err(error) => return Err(error),
        };
        Ok(self
            .find_multiline_string(&contents)
            .iter()
            .map(|r| {
                FindResult::new(
                    r.pattern.clone(),
                    Some(findfile.clone()),
                    r.line_num,
                    r.match_start_index,
                    r.match_end_index,
                    r.line.clone(),
                    r.lines_before.clone(),
                    r.lines_after.clone(),
                )
            })
            .collect())
    }

    fn get_lines_from_contents(&self, contents: &str, newline_indices: Vec<usize>) -> Vec<String> {
        if newline_indices.is_empty() {
            vec![]
        } else {
            let mut lines: Vec<String> = vec![];
            let mut i = 0usize;
            while i < newline_indices.len() - 1 {
                lines.push(contents[newline_indices[i] + 1..newline_indices[i + 1]].to_string());
                i += 1;
            }
            lines
        }
    }

    /// Find a file's contents as a multiline string
    pub fn find_multiline_string<'a>(&self, contents: &str) -> Vec<FindResult> {
        let mut results: Vec<FindResult> = Vec::new();

        let newline_indices: Vec<usize> = contents.match_indices("\n").map(|i| i.0).collect();
        let mut startline_indices: Vec<usize> = vec![0];
        startline_indices.append(&mut newline_indices.iter().map(|n| n + 1).collect());

        for p in &self.settings.find_patterns {
            let matches: Vec<Match> = if self.settings.first_match {
                match p.find(contents) {
                    Some(m) => vec![m],
                    None => vec![],
                }
            } else {
                p.find_iter(contents).collect()
            };
            for m in matches {
                let lines_before_count = startline_indices
                    .iter()
                    .take_while(|i| i <= &&m.start())
                    .count();
                let line_start_index: usize = match startline_indices.get(lines_before_count - 1) {
                    Some(i) => *i,
                    None => 0usize,
                };
                let line_ends_before: Vec<_> = newline_indices
                    .iter()
                    .skip_while(|i| i <= &&m.start())
                    .take(1)
                    .collect();
                let line_end_index: usize = match line_ends_before.get(0) {
                    Some(i) => **i,
                    None => contents.len(),
                };
                let line = &contents[line_start_index..line_end_index];
                let lines_before: Vec<String> = if self.settings.lines_before > 0 {
                    let nlis: &[usize] = &newline_indices[..lines_before_count - 1];
                    let nlis: Vec<usize> = nlis
                        .iter()
                        .rev()
                        .take(self.settings.lines_before + 1)
                        .rev()
                        .cloned()
                        .collect();
                    self.get_lines_from_contents(contents, nlis)
                } else {
                    vec![]
                };
                if !self.lines_match(
                    &lines_before,
                    &self.settings.in_lines_before_patterns,
                    &self.settings.out_lines_before_patterns,
                ) {
                    continue;
                }
                let lines_after: Vec<String> = if (self.settings.lines_after > 0
                    || !self.settings.lines_after_to_patterns.is_empty()
                    || !self.settings.lines_after_until_patterns.is_empty())
                    && newline_indices.len() > lines_before_count
                {
                    let nlis: &[usize] = &newline_indices[lines_before_count - 1..];
                    let nlis: Vec<usize> = if self.settings.lines_after > 0 {
                        nlis.iter()
                            .take(self.settings.lines_after + 1)
                            .cloned()
                            .collect()
                    } else {
                        match p.find(&contents[line_end_index..]) {
                            Some(m) => {
                                let match_start_index = m.start() + line_end_index;
                                let _nlis: Vec<usize> = nlis
                                    .iter()
                                    .take_while(|i| i < &&match_start_index)
                                    .cloned()
                                    .collect();

                                if !self.settings.lines_after_to_patterns.is_empty() {
                                    nlis.iter().take(_nlis.len() + 1).cloned().collect()
                                } else {
                                    _nlis
                                }
                            },
                            None => vec![],
                        }
                    };
                    self.get_lines_from_contents(contents, nlis)
                } else {
                    vec![]
                };
                if !lines_after.is_empty() {
                    if !self.lines_match(
                        &lines_after,
                        &self.settings.in_lines_after_patterns,
                        &self.settings.out_lines_after_patterns,
                    ) {
                        continue;
                    }
                } else if !self.settings.lines_after_to_patterns.is_empty()
                    || !self.settings.lines_after_until_patterns.is_empty()
                {
                    continue;
                }
                let r = FindResult::new(
                    p.as_str().to_string(),
                    None,
                    lines_before_count,
                    m.start() - line_start_index + 1,
                    m.end() - line_start_index + 1,
                    line.to_string().clone(),
                    lines_before,
                    lines_after,
                );
                results.push(r);
            }
        }

        results
    }
}

/// Get the unique list of directories for which find results were found
pub fn get_result_dirs(results: &[FindResult]) -> Vec<&String> {
    let mut dirs: Vec<&String> = Vec::new();
    for r in results.iter() {
        if let Some(f) = &r.file {
            dirs.push(&f.path);
        }
    }
    dirs.sort_unstable();
    dirs.dedup();
    dirs
}

/// Get the unique list of files for which find results were found
pub fn get_result_files(results: &[FindResult]) -> Vec<String> {
    let mut files: Vec<String> = Vec::new();
    for r in results.iter() {
        if let Some(f) = &r.file {
            let filepath = f.filepath();
            files.push(filepath);
        }
    }
    files.sort_unstable();
    files.dedup();
    files
}

/// Get the [unique] list of lines containing matches for a set of find results
pub fn get_result_lines(results: &[FindResult], unique: bool) -> Vec<&str> {
    let mut lines: Vec<&str> = Vec::new();
    for r in results.iter() {
        lines.push(&r.line.trim());
    }
    lines.sort_unstable();
    if unique {
        lines.dedup();
    }
    lines
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::config::{Config, CONFIG_FILE_PATH};
    use crate::filetypes::FileType;

    use super::*;

    fn get_default_test_settings() -> FindSettings {
        let mut settings = FindSettings::default();
        settings.startpath = String::from(".");
        settings.add_find_pattern(String::from("Finder"));
        settings
    }

    #[test]
    fn test_filter_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("codefile.js");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(finder.filter_file(&file));

        let path = String::from("./temp/");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from("./.hidden/");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from(".codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.filter_file(&file));

        let mut settings = get_default_test_settings();
        settings.find_archives = true;
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(finder.filter_file(&file));
    }

    #[test]
    fn test_is_find_dir() {
        let mut settings = get_default_test_settings();
        settings.add_out_dir_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        assert!(finder.is_find_dir(&path));

        let path = String::from(".git");
        assert!(!finder.is_find_dir(&path));

        let path = String::from("./temp/");
        assert!(!finder.is_find_dir(&path));
    }

    #[test]
    fn test_is_find_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("codefile.js");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(finder.is_find_file(&file));

        let path = String::from(".");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(finder.is_find_file(&file));

        let path = String::from("./temp/");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.is_find_file(&file));

        let path = String::from("./.hidden/");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.is_find_file(&file));

        let path = String::from("./");
        let filename = String::from(".codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.is_find_file(&file));

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.is_find_file(&file));
    }

    #[test]
    fn test_is_archive_find_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_archive_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(finder.is_archive_find_file(&file));

        let path = String::from(".");
        let filename = String::from(".archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.is_archive_find_file(&file));

        let path = String::from("./temp");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.is_archive_find_file(&file));

        let path = String::from(".");
        let filename = String::from("temp_archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.is_archive_find_file(&file));
    }

    #[test]
    fn test_find_text_lines() {
        let mut settings = FindSettings::default();
        settings.startpath = String::from(".");
        settings.add_find_pattern(String::from("Finder"));
        let finder = Finder::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");
        let mut lines: std::str::Lines = contents.lines();

        let results = finder.find_text_lines(&mut lines);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].line_num, 29);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_before.len(), 0);
        assert_eq!(results[0].lines_after.len(), 0);
        assert_eq!(results[1].line_num, 35);
        assert_eq!(results[1].match_start_index, 24);
        assert_eq!(results[1].match_end_index, 32);
        assert_eq!(results[1].lines_before.len(), 0);
        assert_eq!(results[1].lines_after.len(), 0);
    }

    #[test]
    fn test_find_text_lines_lines_after_to_until() {
        let mut settings = FindSettings::default();
        settings.startpath = String::from(".");
        settings.add_find_pattern(String::from("Finder"));
        settings.add_lines_after_to_pattern("after".to_string());
        let finder = Finder::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");
        let mut lines: std::str::Lines = contents.lines();

        let results = finder.find_text_lines(&mut lines);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].line_num, 29);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_after.len(), 6);
    }

    #[test]
    fn test_find_text_contents() {
        let mut settings = FindSettings::default();
        settings.startpath = String::from(".");
        settings.add_find_pattern(String::from("Finder"));

        settings.lines_before = 2;
        settings.lines_after = 2;

        let finder = Finder::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");

        let results = finder.find_multiline_string(&contents);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].line_num, 29);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_before.len(), 2);
        assert_eq!(results[0].lines_after.len(), 2);
        assert_eq!(results[1].line_num, 35);
        assert_eq!(results[1].match_start_index, 24);
        assert_eq!(results[1].match_end_index, 32);
        assert_eq!(results[1].lines_before.len(), 2);
        assert_eq!(results[1].lines_after.len(), 2);
    }

    #[test]
    fn test_find_text_contents_lines_after_to_until() {
        let mut settings = FindSettings::default();
        settings.startpath = String::from(".");
        settings.add_find_pattern(String::from("Finder"));

        settings.add_lines_after_to_pattern("after".to_string());

        let finder = Finder::new(settings).ok().unwrap();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/testFile2.txt");
        let contents =
            fs::read_to_string(testfile_path).expect("Something went wrong reading test file");

        let results = finder.find_multiline_string(&contents);

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].line_num, 29);
        assert_eq!(results[0].match_start_index, 3);
        assert_eq!(results[0].match_end_index, 11);
        assert_eq!(results[0].lines_after.len(), 6);
    }

    #[test]
    fn test_find_code_files() {
        let mut settings = FindSettings::default();
        settings.startpath = String::from("/Users/cary/src/xfind/rust");
        settings.add_in_extension(String::from("go,rs"));
        settings.add_find_pattern(String::from("Finder"));
        let finder = Finder::new(settings).ok().unwrap();

        let results = finder.find();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
    }

    #[test]
    fn test_find_helloworld_file() {
        let mut settings = FindSettings::default();

        let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let testfile_path = Path::new(config.shared_path.as_str()).join("testFiles/helloworld.txt");

        settings.startpath = String::from(testfile_path.to_str().unwrap());
        settings.add_find_pattern(String::from("Hello"));
        settings.add_find_pattern(String::from("你好"));
        //settings.text_file_encoding = String::from("windows-1252");
        let finder = Finder::new(settings).ok().unwrap();

        let results = finder.find();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
    }

    #[test]
    fn test_find_binary_files() {
        let mut settings = FindSettings::default();
        settings.startpath = String::from("/Users/cary/src/xfind/java");
        settings.add_in_extension(String::from("class"));
        settings.add_find_pattern(String::from("Finder"));
        let finder = Finder::new(settings).ok().unwrap();

        let results = finder.find();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
        if !results.is_empty() {
            assert_eq!(results[0].pattern, "Finder");
            assert!(results[0].file.is_some());
            assert_eq!(results[0].line, "");
            assert_eq!(results[0].line_num, 0);
        }
    }

    #[test]
    fn test_find_jar_files() {
        let mut settings = FindSettings::default();
        settings.startpath = String::from("../../java/javafind");
        settings.set_archives_only(true);
        settings.add_in_archive_extension(String::from("jar"));
        settings.add_find_pattern(String::from("Finder"));
        let finder = Finder::new(settings).ok().unwrap();

        let results = finder.find();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
        if !results.is_empty() {
            assert_eq!(results[0].pattern, "Finder");
            assert!(results[0].file.is_some());
            //assert_eq!(results[0].line, "");
            assert_eq!(results[0].line_num, 0);
        }
    }

    #[test]
    fn test_find_zip_file() {
        let mut settings = FindSettings::default();
        let path = Path::new("../../shared/testFiles.zip");
        settings.startpath = if path.exists() {
            String::from("../../shared/testFiles.zip")
        } else {
            String::from("../../shared")
        };
        settings.find_archives = true;
        settings.add_find_pattern(String::from("Finder"));
        let finder = Finder::new(settings).ok().unwrap();

        let results = finder.find();
        assert!(results.is_ok());
        let results = results.ok().unwrap();
        println!("results: {}", results.len());
        if !results.is_empty() {
            assert_eq!(results[0].pattern, "Finder");
            assert!(results[0].file.is_some());
            //assert_eq!(results[0].line, "");
            assert_eq!(results[0].line_num, 3);
        }
    }
}
