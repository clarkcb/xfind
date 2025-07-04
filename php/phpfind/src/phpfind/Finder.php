<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Class Finder
 *
 * @property FindSettings $settings
 * @property FileTypes $file_types
 */
class Finder
{
    const string STARTPATH_NOT_DEFINED = 'Startpath not defined';
    const string STARTPATH_NOT_READABLE = 'Startpath not readable';
    const string STARTPATH_NOT_FOUND = 'Startpath not found';
    const string INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH = 'Invalid range for mindepth and maxdepth';
    const string INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD = 'Invalid range for minlastmod and maxlastmod';
    const string INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE = 'Invalid range for minsize and maxsize';
    const string STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS = 'Startpath does not match find settings';
    private readonly FindSettings $settings;
    private readonly FileTypes $file_types;

    /**
     * @param FindSettings $settings
     * @throws FindException
     */
    public function __construct(FindSettings $settings)
    {
        $this->settings = $settings;
        $this->file_types = new FileTypes();
        $this->validate_settings();
    }

    /**
     * @throws FindException
     */
    private function validate_settings(): void
    {
        if (!$this->settings->paths) {
            throw new FindException(self::STARTPATH_NOT_DEFINED);
        }
        foreach ($this->settings->paths as $p) {
            if (!file_exists($p)) {
                $p = FileUtil::expand_path($p);
            }
            if (file_exists($p)) {
                if (!is_readable($p)) {
                    throw new FindException(self::STARTPATH_NOT_READABLE);
                }
            } else {
                throw new FindException(self::STARTPATH_NOT_FOUND);
            }
        }
        if ($this->settings->max_depth > -1 && $this->settings->min_depth > -1
            && $this->settings->max_depth < $this->settings->min_depth) {
            throw new FindException(self::INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH);
        }
        if ($this->settings->max_last_mod != null && $this->settings->min_last_mod != null
            && $this->settings->max_last_mod->getTimestamp() < $this->settings->min_last_mod->getTimestamp()) {
            throw new FindException(self::INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD);
        }
        if ($this->settings->max_size > 0 && $this->settings->max_size < $this->settings->min_size) {
            throw new FindException(self::INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE);
        }
    }

    /**
     * @param string $s
     * @param string[] $patterns
     * @return bool
     */
    private function matches_any_pattern(string $s, array $patterns): bool
    {
        foreach ($patterns as $pattern) {
            $pattern = '/' . $pattern . '/';
            if (preg_match_all($pattern, $s)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param string[] $slist
     * @param string[] $patterns
     * @return bool
     */
    private function any_matches_any_pattern(array $slist, array $patterns): bool
    {
        foreach ($slist as $s) {
            if ($this->matches_any_pattern($s, $patterns)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param string $dir
     * @return bool
     */
    public function is_matching_dir(string $dir): bool
    {
        // empty or dot dir is a match
        if ($dir == '' || FileUtil::is_dot_dir($dir)) {
            return true;
        }
        if (!$this->settings->include_hidden && FileUtil::is_hidden($dir)) {
            return false;
        }
        $path_elems = FileUtil::split_path($dir);
        if ($this->settings->in_dir_patterns &&
            !$this->any_matches_any_pattern($path_elems, $this->settings->in_dir_patterns)) {
            return false;
        }
        if ($this->settings->out_dir_patterns &&
            $this->any_matches_any_pattern($path_elems, $this->settings->out_dir_patterns)) {
            return false;
        }
        return true;
    }

    /**
     * @param string $file_path
     * @return FileResult
     */
    private function file_path_to_file_result(string $file_path): FileResult {
        $path_and_file_name = FileUtil::split_to_path_and_file_name($file_path);
        $path = $path_and_file_name[0];
        $file_name = $path_and_file_name[1];
        $file_type = $this->file_types->get_file_type($file_name);
        $file_size = 0;
        $last_mod = 0;
        if ($this->settings->need_last_mod() || $this->settings->need_size()) {
            $stat = stat($file_path);
            if ($stat !== false) {
                if ($this->settings->need_last_mod()) {
                    $last_mod = $stat['mtime'];
                }
                if ($this->settings->need_size()) {
                    $file_size = $stat['size'];
                }
            }
        }
        return new FileResult($path, $file_name, $file_type, $file_size, $last_mod);
    }

    /**
     * @param string $f
     * @return bool
     */
    public function is_matching_archive_file(string $f): bool
    {
        return $this->is_matching_archive_file_result($this->file_path_to_file_result($f));
    }

    /**
     * @param string $f
     * @return bool
     */
    public function is_matching_file(string $f): bool
    {
        return $this->is_matching_file_result($this->file_path_to_file_result($f));
    }

    /**
     * @param string $ext
     * @return bool
     */
    public function is_matching_archive_extension(string $ext): bool
    {
        return ((count($this->settings->in_archive_extensions) == 0
                || in_array($ext, $this->settings->in_archive_extensions))
            && (count($this->settings->out_archive_extensions) == 0
                || !in_array($ext, $this->settings->out_archive_extensions)));
    }

    /**
     * @param string $ext
     * @return bool
     */
    public function is_matching_extension(string $ext): bool
    {
        return ((count($this->settings->in_extensions) == 0
                || in_array($ext, $this->settings->in_extensions))
            && (count($this->settings->out_extensions) == 0
                || !in_array($ext, $this->settings->out_extensions)));
    }

    /**
     * @param FileResult $fr
     * @return bool
     */
    public function has_matching_archive_extension(FileResult $fr): bool
    {
        if ($this->settings->in_archive_extensions || $this->settings->out_archive_extensions) {
            $ext = FileUtil::get_extension($fr->file_name);
            return $this->is_matching_archive_extension($ext);
        }
        return true;
    }

    /**
     * @param FileResult $fr
     * @return bool
     */
    public function has_matching_extension(FileResult $fr): bool
    {
        if ($this->settings->in_extensions || $this->settings->out_extensions) {
            $ext = FileUtil::get_extension($fr->file_name);
            return $this->is_matching_extension($ext);
        }
        return true;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_matching_archive_file_name(string $file_name): bool
    {
        return ((count($this->settings->in_archive_file_patterns) == 0
                || $this->matches_any_pattern($file_name, $this->settings->in_archive_file_patterns))
            && (count($this->settings->out_archive_file_patterns) == 0
                || !$this->matches_any_pattern($file_name, $this->settings->out_archive_file_patterns)));
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_matching_file_name(string $file_name): bool
    {
        return ((count($this->settings->in_file_patterns) == 0
                || $this->matches_any_pattern($file_name, $this->settings->in_file_patterns))
            && (count($this->settings->out_file_patterns) == 0
                || !$this->matches_any_pattern($file_name, $this->settings->out_file_patterns)));
    }

    /**
     * @param FileType $file_type
     * @return bool
     */
    public function is_matching_file_type(FileType $file_type): bool
    {
        return ((count($this->settings->in_file_types) == 0
                || in_array($file_type, $this->settings->in_file_types))
            && (count($this->settings->out_file_types) == 0
                || !in_array($file_type, $this->settings->out_file_types)));
    }

    /**
     * @param int $file_size
     * @return bool
     */
    public function is_matching_file_size(int $file_size): bool
    {
        return (($this->settings->max_size == 0 || $file_size <= $this->settings->max_size)
            && ($this->settings->min_size == 0 || $file_size >= $this->settings->min_size));
    }

    /**
     * @param int $last_mod
     * @return bool
     */
    public function is_matching_last_mod(int $last_mod): bool
    {
        return (($this->settings->max_last_mod == null
                || $last_mod <= $this->settings->max_last_mod->getTimestamp())
            && ($this->settings->min_last_mod == null
                || $last_mod >= $this->settings->min_last_mod->getTimestamp()));
    }

    /**
     * @param FileResult $fr
     * @return bool
     */
    public function is_matching_archive_file_result(FileResult $fr): bool
    {
        return $this->has_matching_archive_extension($fr)
            && $this->is_matching_archive_file_name($fr->file_name);
    }

    /**
     * @param FileResult $fr
     * @return bool
     */
    public function is_matching_file_result(FileResult $fr): bool
    {
        return $this->has_matching_extension($fr)
            && $this->is_matching_file_name($fr->file_name)
            && $this->is_matching_file_type($fr->file_type)
            && $this->is_matching_file_size($fr->file_size)
            && $this->is_matching_last_mod($fr->last_mod);
    }

    /**
     * @param string $dir
     * @param string $file_name
     * @return FileResult|null
     */
    public function filter_to_file_result(string $dir, string $file_name): ?FileResult
    {
        if (!$this->settings->include_hidden && FileUtil::is_hidden($file_name)) {
            return null;
        }
        $file_type = $this->file_types->get_file_type($file_name);
        if ($file_type == FileType::Archive
            && !$this->settings->include_archives
            && !$this->settings->archives_only) {
            return null;
        }
        $file_size = 0;
        $last_mod = 0;
        if ($this->settings->need_last_mod() || $this->settings->need_size()) {
            $file_path = FileUtil::join_paths($dir, $file_name);
            $stat = stat($file_path);
            if ($stat !== false) {
                if ($this->settings->need_last_mod()) {
                    $last_mod = $stat['mtime'];
                }
                if ($this->settings->need_size()) {
                    $file_size = $stat['size'];
                }
            }
        }

        $file_result = new FileResult($dir, $file_name, $file_type, $file_size, $last_mod);
        if ($file_result->file_type == FileType::Archive) {
            if ($this->is_matching_archive_file_result($file_result)) {
                return $file_result;
            }
            return null;
        }
        if (!$this->settings->archives_only && $this->is_matching_file_result($file_result)) {
            return $file_result;
        }
        return null;
    }

    /**
     * @param string $dir
     * @param int $min_depth
     * @param int $max_depth
     * @param int $current_depth
     * @return FileResult[]
     */
    private function rec_get_file_results(string $dir, int $min_depth, int $max_depth, int $current_depth): array
    {
        $recurse = true;
        if ($current_depth == $max_depth) {
            $recurse = false;
        } else if ($max_depth > -1 && $current_depth > $max_depth) {
            return [];
        }
        $dir_results = array();
        $file_results = array();
        foreach (scandir($dir) as $entry) {
            if (FileUtil::is_dot_dir($entry)) {
                continue;
            }
            $entry_path = FileUtil::join_paths($dir, $entry);
            if (is_link($entry_path) && !$this->settings->follow_symlinks) {
                continue;
            }
            if (is_dir($entry_path) && $recurse && $this->is_matching_dir($entry)) {
                $dir_results[] = $entry_path;
            } else if (is_file($entry_path) && ($min_depth < 0 || $current_depth >= $min_depth)) {
                $file_result = $this->filter_to_file_result($dir, $entry);
                if ($file_result != null) {
                    $file_results[] = $file_result;
                }
            }
        }
        foreach ($dir_results as $dir_result) {
            $file_results = array_merge($file_results, $this->rec_get_file_results($dir_result,
                $min_depth, $max_depth, $current_depth + 1));
        }
        return $file_results;
    }

    /**
     * @return FileResult[]
     * @throws FindException
     */
    public function get_file_results(string $file_path): array
    {
        $file_results = [];
        if (!file_exists($file_path)) {
            $file_path = FileUtil::expand_path($file_path);
        }
        if (is_dir($file_path)) {
            # if max_depth is zero, we can skip since a directory cannot be a result
            if ($this->settings->max_depth == 0) {
                return [];
            }
            if ($this->is_matching_dir($file_path)) {
                $max_depth = $this->settings->max_depth;
                if (!$this->settings->recursive) {
                    $max_depth = 1;
                }
                $file_results = array_merge($file_results, $this->rec_get_file_results($file_path,
                    $this->settings->min_depth, $max_depth, 1));
            } else {
                throw new FindException(self::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS);
            }
        } else {
            # if min_depth > zero, we can skip since the file is at depth zero
            if ($this->settings->min_depth > 0) {
                return [];
            }
            $d = dirname($file_path);
            $f = basename($file_path);
            $file_result = $this->filter_to_file_result($d, $f);
            if ($file_result != null) {
                $file_results[] = $file_result;
            } else {
                throw new FindException(self::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS);
            }
        }
        return $file_results;
    }

    /**
     * @return FileResult[]
     * @throws FindException
     */
    public function find(): array
    {
        $file_results = [];
        foreach ($this->settings->paths as $p) {
            $file_results = array_merge($file_results, $this->get_file_results($p));
        }
        $file_result_sorter = new FileResultSorter($this->settings);
        return $file_result_sorter->sort($file_results);
    }

    /**
     * @param FileResult[] $file_results
     * @return string[]
     */
    public function get_matching_dirs(array $file_results): array
    {
        $dirs = [];
        foreach ($file_results as $f) {
            if (!in_array($f->path, $dirs)) {
                $dirs[] = $f->path;
            }
        }
        sort($dirs);
        return $dirs;
    }

    /**
     * @param FileResult[] $file_results
     * @return void
     */
    public function print_matching_dirs(array $file_results, FileResultFormatter $formatter): void
    {
        $dirs = $this->get_matching_dirs($file_results);
        if (count($dirs) > 0) {
            Logger::log_msg(sprintf("\nMatching directories (%d):", count($dirs)));
            foreach ($dirs as $d) {
                Logger::log_msg($formatter->format_dir($d));
            }
        } else {
            Logger::log_msg("\nMatching directories: 0");
        }
    }

    /**
     * @param FileResult[] $file_results
     * @return void
     */
    public function print_matching_files(array $file_results, FileResultFormatter $formatter): void
    {
        if (count($file_results) > 0) {
            Logger::log_msg(sprintf("\nMatching files (%d):", count($file_results)));
            foreach ($file_results as $fr) {
                Logger::log_msg($formatter->format_file_result($fr));
            }
        } else {
            Logger::log_msg("\nMatching files: 0");
        }
    }
}
