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
            throw new FindException(FindError::STARTPATH_NOT_DEFINED->value);
        }
        foreach ($this->settings->paths as $p) {
            if (!file_exists($p)) {
                $p = FileUtil::expand_path($p);
            }
            if (file_exists($p)) {
                if (!is_readable($p)) {
                    throw new FindException(FindError::STARTPATH_NOT_READABLE->value);
                }
                if (is_link($p)) {
                    if (!$this->settings->follow_symlinks) {
                        throw new FindException(FindError::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS->value);
                    }
                }
                if (is_dir($p)) {
                    if (!$this->is_traversable_dir_path($p)) {
                        throw new FindException(FindError::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS->value);
                    }
                } elseif (is_file($p)) {
                    if ($this->filter_to_file_result($p) == null) {
                        throw new FindException(FindError::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS->value);
                    }
                } else {
                    # TODO: handle start path as symlink
                    # TODO: start path is unknown/invalid type
                    throw new FindException(FindError::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS->value);
                }
            } else {
                throw new FindException(FindError::STARTPATH_NOT_FOUND->value);
            }
        }
        if ($this->settings->max_depth > -1 && $this->settings->min_depth > -1
            && $this->settings->max_depth < $this->settings->min_depth) {
            throw new FindException(FindError::INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH->value);
        }
        if ($this->settings->max_last_mod != null && $this->settings->min_last_mod != null
            && $this->settings->max_last_mod->getTimestamp() < $this->settings->min_last_mod->getTimestamp()) {
            throw new FindException(FindError::INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD->value);
        }
        if ($this->settings->max_size > 0 && $this->settings->max_size < $this->settings->min_size) {
            throw new FindException(FindError::INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE->value);
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
     * @param string $s
     * @param string[] $patterns
     * @return bool
     */
    private function empty_or_matches_any_pattern(string $s, array $patterns): bool
    {
        return count($patterns) == 0 || $this->matches_any_pattern($s, $patterns);
    }

    /**
     * @param string $s
     * @param string[] $patterns
     * @return bool
     */
    private function empty_or_not_matches_any_pattern(string $s, array $patterns): bool
    {
        return count($patterns) == 0 || !$this->matches_any_pattern($s, $patterns);
    }

    /**
     * @param string[] $slist
     * @param string[] $patterns
     * @return bool
     */
    private function empty_or_any_matches_any_pattern(array $slist, array $patterns): bool
    {
        return count($patterns) == 0 || $this->any_matches_any_pattern($slist, $patterns);
    }

    /**
     * @param string[] $slist
     * @param string[] $patterns
     * @return bool
     */
    private function empty_or_not_any_matches_any_pattern(array $slist, array $patterns): bool
    {
        return count($patterns) == 0 || !$this->any_matches_any_pattern($slist, $patterns);
    }

    /**
     * @param string $s
     * @param string[] $strings
     * @return bool
     */
    public function empty_or_matches_any_string(string $s, array $strings): bool
    {
        return (count($strings) == 0 || in_array($s, $strings));
    }

    /**
     * @param string $s
     * @param string[] $strings
     * @return bool
     */
    public function empty_or_not_matches_any_string(string $s, array $strings): bool
    {
        return (count($strings) == 0 || !in_array($s, $strings));
    }

    /**
     * @param FileType $file_type
     * @param FileType[] $file_types
     * @return bool
     */
    public function empty_or_matches_any_file_type(FileType $file_type, array $file_types): bool
    {
        return (count($file_types) == 0 || in_array($file_type, $file_types));
    }

    /**
     * @param FileType $file_type
     * @param FileType[] $file_types
     * @return bool
     */
    public function empty_or_not_matches_any_file_type(FileType $file_type, array $file_types): bool
    {
        return (count($file_types) == 0 || !in_array($file_type, $file_types));
    }

    /**
     * @param string $path
     * @return bool
     */
    public function is_matching_path_by_symlink(string $path): bool
    {
        return $this->settings->follow_symlinks || !is_link($path);
    }

    /**
     * @param string $dir_path
     * @return bool
     */
    public function is_matching_dir_path_by_hidden(string $dir_path): bool
    {
        if (!$this->settings->include_hidden && FileUtil::is_hidden_path($dir_path)) {
            return false;
        }
        return true;
    }

    /**
     * @param string $dir_path
     * @return bool
     */
    public function is_matching_dir_path_by_in_patterns(string $dir_path): bool
    {
        $path_elems = FileUtil::split_path($dir_path);
        return $this->empty_or_any_matches_any_pattern($path_elems, $this->settings->in_dir_patterns);
    }

    /**
     * @param string $dir_path
     * @return bool
     */
    public function is_matching_dir_path_by_out_patterns(string $dir_path): bool
    {
        $path_elems = FileUtil::split_path($dir_path);
        return $this->empty_or_not_any_matches_any_pattern($path_elems, $this->settings->out_dir_patterns);
    }

    /**
     * @param string $dir_path
     * @return bool
     */
    public function is_traversable_dir_path(string $dir_path): bool
    {
        return $this->is_matching_dir_path_by_hidden($dir_path) &&
            $this->is_matching_dir_path_by_out_patterns($dir_path);
    }

    /**
     * @param string $dir_path
     * @return bool
     */
    public function is_matching_dir_path(string $dir_path): bool
    {
        return $this->is_matching_dir_path_by_hidden($dir_path) &&
            $this->is_matching_dir_path_by_in_patterns($dir_path) &&
            $this->is_matching_dir_path_by_out_patterns($dir_path);
    }

    /**
     * @param string $dir_path
     * @return bool
     */
    public function is_null_or_matching_dir_path(string $dir_path): bool
    {
        // null or empty dir_path is a match
        if ($dir_path == NULL || $dir_path == '') {
            return true;
        }
        return $this->is_matching_dir_path($dir_path);
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_matching_file_name_by_hidden(string $file_name): bool
    {
        return $this->settings->include_hidden || !FileUtil::is_hidden_name($file_name);
    }

    /**
     * @param string $ext
     * @return bool
     */
    public function is_matching_archive_extension(string $ext): bool
    {
        return ($this->empty_or_matches_any_string($ext, $this->settings->in_archive_extensions)
            && $this->empty_or_not_matches_any_string($ext, $this->settings->out_archive_extensions));
    }

    /**
     * @param string $file_path
     * @return bool
     */
    public function is_matching_archive_extension_for_file_path(string $file_path): bool
    {
        if ($this->settings->in_archive_extensions || $this->settings->out_archive_extensions) {
            $ext = FileUtil::get_extension(basename($file_path));
            return $this->is_matching_archive_extension($ext);
        }
        return true;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_matching_archive_file_name(string $file_name): bool
    {
        return $this->is_matching_file_name_by_hidden($file_name)
            && $this->empty_or_matches_any_pattern($file_name, $this->settings->in_archive_file_patterns)
            && $this->empty_or_not_matches_any_pattern($file_name, $this->settings->out_archive_file_patterns);
    }

    /**
     * @param string $file_path
     * @return bool
     */
    public function is_matching_archive_file_name_for_file_path(string $file_path): bool
    {
        if ($this->settings->in_archive_file_patterns || $this->settings->out_archive_file_patterns) {
            return $this->is_matching_archive_file_name(basename($file_path));
        }
        return true;
    }

    /**
     * @param string $file_path
     * @return bool
     */
    public function is_matching_archive_file_path(string $file_path): bool
    {
        return $this->is_matching_archive_extension_for_file_path($file_path)
            && $this->is_matching_archive_file_name_for_file_path($file_path);
    }

    /**
     * @param FileResult $fr
     * @return bool
     */
    public function is_matching_archive_file_result(FileResult $fr): bool
    {
        return $this->is_matching_archive_file_path($fr->file_path);
    }


    /**
     * @param string $ext
     * @return bool
     */
    public function is_matching_extension(string $ext): bool
    {
        return ($this->empty_or_matches_any_string($ext, $this->settings->in_extensions)
            && $this->empty_or_not_matches_any_string($ext, $this->settings->out_extensions));
    }

    /**
     * @param string $file_path
     * @return bool
     */
    public function is_matching_extension_for_file_path(string $file_path): bool
    {
        if ($this->settings->in_extensions || $this->settings->out_extensions) {
            $ext = FileUtil::get_extension(basename($file_path));
            return $this->is_matching_extension($ext);
        }
        return true;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_matching_file_name(string $file_name): bool
    {
        return ($this->empty_or_matches_any_pattern($file_name, $this->settings->in_file_patterns))
            && ($this->empty_or_not_matches_any_pattern($file_name, $this->settings->out_file_patterns));
    }

    /**
     * @param string $file_path
     * @return bool
     */
    public function is_matching_file_name_for_file_path(string $file_path): bool
    {
        if ($this->settings->in_file_patterns || $this->settings->out_file_patterns) {
            return $this->is_matching_file_name(basename($file_path));
        }
        return true;
    }

    /**
     * @param string $file_path
     * @return bool
     */
    public function is_matching_file_path(string $file_path): bool
    {
        return $this->is_matching_extension_for_file_path($file_path)
            && $this->is_matching_file_name_for_file_path($file_path);
    }

    /**
     * @param FileType $file_type
     * @return bool
     */
    public function is_matching_file_type(FileType $file_type): bool
    {
        return ($this->empty_or_matches_any_file_type($file_type, $this->settings->in_file_types))
            && ($this->empty_or_not_matches_any_file_type($file_type, $this->settings->out_file_types));
    }

    /**
     * @param int $file_size
     * @return bool
     */
    public function is_matching_file_size(int $file_size): bool
    {
        return (($this->settings->max_size <= 0 || $file_size <= $this->settings->max_size)
            && ($this->settings->min_size <= 0 || $file_size >= $this->settings->min_size));
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
    public function is_matching_file_result(FileResult $fr): bool
    {
        return $this->is_matching_file_path($fr->file_path)
            && $this->is_matching_file_type($fr->file_type)
            && $this->is_matching_file_size($fr->file_size)
            && $this->is_matching_last_mod($fr->last_mod);
    }

    /**
     * @param string $file_path
     * @return FileResult|null
     */
    public function filter_archive_file_path_to_file_result(string $file_path): ?FileResult
    {
        if (!$this->settings->include_archives && !$this->settings->archives_only) {
            return null;
        }

        $file_size = 0;
        $last_mod = 0;
        $file_result = new FileResult($file_path, FileType::Archive, $file_size, $last_mod);
        if ($this->is_matching_archive_file_result($file_result)) {
            return $file_result;
        }
        return null;
    }

    /**
     * @param string $file_path
     * @param FileType $file_type
     * @return FileResult|null
     */
    public function filter_reg_file_path_to_file_result(string $file_path, FileType $file_type): ?FileResult
    {
        if ($this->settings->archives_only) {
            return null;
        }

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

        $file_result = new FileResult($file_path, $file_type, $file_size, $last_mod);
        if ($this->is_matching_file_result($file_result)) {
            return $file_result;
        }
        return null;
    }

    /**
     * @param string $file_path
     * @return FileResult|null
     */
    public function filter_to_file_result(string $file_path): ?FileResult
    {
        $dir = dirname($file_path);
        if (!$this->is_null_or_matching_dir_path($dir)) {
            return null;
        }

        $file_name = basename($file_path);
        if (!$this->settings->include_hidden && FileUtil::is_hidden_name($file_name)) {
            return null;
        }
        $file_type = $this->file_types->get_file_type($file_name);
        if ($file_type == FileType::Archive) {
            return $this->filter_archive_file_path_to_file_result($file_path);
        }
        return $this->filter_reg_file_path_to_file_result($file_path, $file_type);
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
            if (!$this->is_matching_path_by_symlink($entry_path)) {
                continue;
            }
            if (is_dir($entry_path)) {
                if ($recurse && $this->is_traversable_dir_path($entry)) {
                    $dir_results[] = $entry_path;
                }
            } else if (is_file($entry_path)) {
                if ($min_depth < 0 || $current_depth >= $min_depth) {
                    $file_result = $this->filter_to_file_result($entry_path);
                    if ($file_result != null) {
                        $file_results[] = $file_result;
                    }
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
            if (!file_exists($file_path)) {
                throw new FindException(FindError::STARTPATH_NOT_FOUND->value);
            }
        }
        if (is_link($file_path) && !$this->settings->follow_symlinks) {
            throw new FindException(FindError::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS->value);
        }
        if (is_dir($file_path)) {
            # if max_depth is zero, we can skip since a directory cannot be a result
            if ($this->settings->max_depth == 0) {
                return [];
            }
            if ($this->is_traversable_dir_path($file_path)) {
                $max_depth = $this->settings->max_depth;
                if (!$this->settings->recursive) {
                    $max_depth = 1;
                }
                $file_results = array_merge($file_results, $this->rec_get_file_results($file_path,
                    $this->settings->min_depth, $max_depth, 1));
            } else {
                throw new FindException(FindError::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS->value);
            }
        } elseif (is_file($file_path)) {
            # if min_depth > zero, we can skip since the file is at depth zero
            if ($this->settings->min_depth > 0) {
                return [];
            }
            $file_result = $this->filter_to_file_result($file_path);
            if ($file_result != null) {
                $file_results[] = $file_result;
            } else {
                throw new FindException(FindError::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS->value);
            }
        } else {
            throw new FindException(FindError::STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS->value);
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
        foreach ($file_results as $fr) {
            $dir = dirname($fr->file_path);
            if (!in_array($dir, $dirs)) {
                $dirs[] = $dir;
            }
        }
        sort($dirs);
        return $dirs;
    }

    /**
     * @param FileResult[] $file_results
     * @param FileResultFormatter $formatter
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
     * @param FileResultFormatter $formatter
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
