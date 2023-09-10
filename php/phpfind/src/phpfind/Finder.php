<?php declare(strict_types=1);

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
            throw new FindException('Startpath not defined');
        }
        foreach ($this->settings->paths as $p) {
            if (!file_exists($p)) {
                throw new FindException('Startpath not found');
            }
            if (!is_readable($p)) {
                throw new FindException('Startpath not readable');
            }
        }
        if ($this->settings->max_depth > -1 && $this->settings->max_depth < $this->settings->min_depth) {
            throw new FindException('Invalid range for mindepth and maxdepth');
        }
        if ($this->settings->max_last_mod != null && $this->settings->min_last_mod != null
            && $this->settings->max_last_mod->getTimestamp() < $this->settings->min_last_mod->getTimestamp()) {
            throw new FindException('Invalid range for minlastmod and maxlastmod');
        }
        if ($this->settings->max_size > 0 && $this->settings->max_size < $this->settings->min_size) {
            throw new FindException('Invalid range for minsize and maxsize');
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
        if (FileUtil::is_dot_dir($dir)) {
            return true;
        }
        if ($this->settings->exclude_hidden && FileUtil::is_hidden($dir)) {
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
        $path_and_filename = FileUtil::split_to_path_and_filename($file_path);
        $path = $path_and_filename[0];
        $file_name = $path_and_filename[1];
        $stat = false;
        if ($this->settings->need_stat()) {
            $stat = stat($file_path);
        }
        return new FileResult($path, $file_name, $this->file_types->get_file_type($file_name), $stat);
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
     * @param FileResult $fr
     * @return bool
     */
    public function is_matching_file_result(FileResult $fr): bool
    {
        if ($this->settings->in_extensions || $this->settings->out_extensions) {
            $ext = FileUtil::get_extension($fr->file_name);
            if ($this->settings->in_extensions && !in_array($ext, $this->settings->in_extensions)) {
                return false;
            }
            if ($this->settings->out_extensions && in_array($ext, $this->settings->out_extensions)) {
                return false;
            }
        }
        if ($this->settings->in_file_patterns &&
            !$this->matches_any_pattern($fr->file_name, $this->settings->in_file_patterns)) {
            return false;
        }
        if ($this->settings->out_file_patterns &&
            $this->matches_any_pattern($fr->file_name, $this->settings->out_file_patterns)) {
            return false;
        }
        if ($this->settings->in_file_types && !in_array($fr->file_type, $this->settings->in_file_types)) {
            return false;
        }
        if ($this->settings->out_file_types && in_array($fr->file_type, $this->settings->out_file_types)) {
            return false;
        }
        if ($fr->stat) {
            if (($this->settings->max_last_mod != null && $fr->stat['mtime'] > $this->settings->max_last_mod->getTimestamp())
                || ($this->settings->min_last_mod != null && $fr->stat['mtime'] < $this->settings->min_last_mod->getTimestamp())
                || ($this->settings->max_size > 0 && $fr->stat['size'] > $this->settings->max_size)
                || ($this->settings->min_size > 0 && $fr->stat['size'] < $this->settings->min_size)) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param string $file_name
     * @return bool
     */
    public function is_matching_archive_file(string $file_name): bool
    {
        $ext = FileUtil::get_extension($file_name);
        if ($this->settings->in_archive_extensions &&
            !in_array($ext, $this->settings->in_archive_extensions)) {
            return false;
        }
        if ($this->settings->out_archive_extensions &&
            in_array($ext, $this->settings->out_archive_extensions)) {
            return false;
        }
        if ($this->settings->in_archive_file_patterns &&
            !$this->matches_any_pattern($file_name, $this->settings->in_archive_file_patterns)) {
            return false;
        }
        if ($this->settings->out_archive_file_patterns &&
            $this->matches_any_pattern($file_name, $this->settings->out_archive_file_patterns)) {
            return false;
        }
        return true;
    }

    /**
     * @param string $dir
     * @param string $file_name
     * @return FileResult|null
     */
    public function filter_to_file_result(string $dir, string $file_name): ?FileResult
    {
        $file_path = FileUtil::join_path($dir, $file_name);
        if ($this->settings->exclude_hidden && FileUtil::is_hidden($file_path)) {
            return null;
        }
        $stat = false;
        if ($this->settings->need_stat()) {
            $stat = stat($file_path);
        }
        $file_result = new FileResult($dir, $file_name, $this->file_types->get_file_type($file_name), $stat);
        if ($file_result->file_type == FileType::Archive) {
            if ($this->settings->include_archives && $this->is_matching_archive_file($file_path)) {
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
     * @return string[]
     */
    private function get_non_dot_dirs(string $dir): array
    {
        $filter_non_dot_dirs = function ($f) use ($dir) {
            return (is_dir(FileUtil::join_path($dir, $f)) && !FileUtil::is_dot_dir($f));
        };
        return array_filter(scandir($dir), $filter_non_dot_dirs);
    }

    /**
     * @param string $dir
     * @return string[]
     */
    private function get_dir_dir_results(string $dir): array
    {
        $filter_dirs = function ($f) use ($dir) {
            return is_dir(FileUtil::join_path($dir, $f)) && $this->is_matching_dir($f);
        };
        $join_path = function ($f) use ($dir) {
            return FileUtil::join_path($dir, $f);
        };
        $dirresults = array_filter($this->get_non_dot_dirs($dir), $filter_dirs);
        return array_map($join_path, $dirresults);
    }

    /**
     * @param string $dir
     * @return FileResult[]
     */
    private function get_dir_file_results(string $dir): array
    {
        $file_results = array();
        foreach (scandir($dir) as $entry) {
            if (is_file(FileUtil::join_path($dir, $entry))) {
                $file_result = $this->filter_to_file_result($dir, $entry);
                if ($file_result != null) {
                    $file_results[] = $file_result;
                }
            }
        }
        return $file_results;
    }

    /**
     * @param string $dir
     * @param int $depth
     * @return FileResult[]
     */
    private function rec_get_file_results(string $dir, int $depth): array
    {
        $dir_results = array();
        if ($this->settings->max_depth < 1 || $depth <= $this->settings->max_depth) {
            $dir_results = $this->get_dir_dir_results($dir);
        }
        $file_results = array();
        if ($depth >= $this->settings->min_depth
            && ($this->settings->max_depth < 1 || $depth <= $this->settings->max_depth)) {
            $file_results = $this->get_dir_file_results($dir);
        }
        foreach ($dir_results as $dir_result) {
            $file_results = array_merge($file_results, $this->rec_get_file_results($dir_result, $depth + 1));
        }
        return $file_results;
    }

    /**
     * @return FileResult[]
     * @throws FindException
     */
    public function get_file_results(string $file_path): array
    {
        $file_results = array();
        if (is_dir($file_path)) {
            # if max_depth is zero, we can skip since a directory cannot be a result
            if ($this->settings->max_depth == 0) {
                return [];
            }
            if ($this->is_matching_dir($file_path)) {
                $depth = 1;
                if ($this->settings->recursive) {
                    $file_results = array_merge($file_results, $this->rec_get_file_results($file_path, $depth));
                } else {
                    $file_results = array_merge($file_results, $this->get_dir_file_results($file_path));
                }
            } else {
                throw new FindException("Startpath does not match find settings");
            }
        } elseif (is_file($file_path)) {
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
                throw new FindException("Startpath does not match find settings");
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
        $file_results = array();
        foreach ($this->settings->paths as $p) {
            $file_results = array_merge($file_results, $this->get_file_results($p));
        }
        return $this->sort_file_results($file_results);
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    private function cmp_file_result_path(FileResult $fr1, FileResult $fr2): int
    {
        [$path1, $path2] = $this->settings->sort_case_insensitive ?
            array(strtolower($fr1->path), strtolower($fr2->path)) :
            array($fr1->path, $fr2->path);
        if ($path1 == $path2) {
            [$file_name1, $file_name2] = $this->settings->sort_case_insensitive ?
                array(strtolower($fr1->file_name), strtolower($fr2->file_name)) :
                array($fr1->file_name, $fr2->file_name);
            return ($file_name1 < $file_name2) ? -1 : 1;
        }
        return ($path1 < $path2) ? -1 : 1;
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    private function cmp_file_result_file_name(FileResult $fr1, FileResult $fr2): int
    {
        [$file_name1, $file_name2] = $this->settings->sort_case_insensitive ?
            array(strtolower($fr1->file_name), strtolower($fr2->file_name)) :
            array($fr1->file_name, $fr2->file_name);
        if ($file_name1 == $file_name2) {
            [$path1, $path2] = $this->settings->sort_case_insensitive ?
                array(strtolower($fr1->path), strtolower($fr2->path)) :
                array($fr1->path, $fr2->path);
            return ($path1 < $path2) ? -1 : 1;
        }
        return ($file_name1 < $file_name2) ? -1 : 1;
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    private function cmp_file_result_file_size(FileResult $fr1, FileResult $fr2): int
    {
        if ($fr1->stat['size'] == $fr2->stat['size']) {
            return $this->cmp_file_result_path($fr1, $fr2);
        }
        return ($fr1->stat['size'] < $fr2->stat['size']) ? -1 : 1;
    }

    /**
     * @param FileType $ft1
     * @param FileType $ft2
     * @return int
     */
    private function cmp_file_type(FileType $ft1, FileType $ft2): int
    {
        $file_type_cases = FileType::cases();
        return array_search($ft1, $file_type_cases) - array_search($ft2, $file_type_cases);
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    private function cmp_file_result_file_type(FileResult $fr1, FileResult $fr2): int
    {
        $ftcmp = $this->cmp_file_type($fr1->file_type, $fr2->file_type);
        if ($ftcmp == 0) {
            return $this->cmp_file_result_path($fr1, $fr2);
        }
        return $ftcmp;
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    private function cmp_file_result_last_mod(FileResult $fr1, FileResult $fr2): int
    {
        if ($fr1->stat['mtime'] == $fr2->stat['mtime']) {
            return $this->cmp_file_result_path($fr1, $fr2);
        }
        return ($fr1->stat['mtime'] < $fr2->stat['mtime']) ? -1 : 1;
    }

    /**
     * @param FileResult[] $file_results
     * @return FileResult[]
     */
    private function sort_file_results(array $file_results): array
    {
        switch ($this->settings->sort_by) {
            case SortBy::Filename:
                usort($file_results, fn (FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_name($fr1, $fr2));
                break;
            case SortBy::Filesize:
                usort($file_results, fn (FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_size($fr1, $fr2));
                break;
            case SortBy::Filetype:
                usort($file_results, fn (FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_type($fr1, $fr2));
                break;
            case SortBy::LastMod:
                usort($file_results, fn (FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_last_mod($fr1, $fr2));
                break;
            default:
                usort($file_results, fn (FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_path($fr1, $fr2));
                break;
        }
        if ($this->settings->sort_descending) {
            return array_reverse($file_results);
        }
        return $file_results;
    }

    /**
     * @param FileResult[] $file_results
     * @return string[]
     */
    public function get_matching_dirs(array $file_results): array
    {
        $dirs = array();
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
    public function print_matching_dirs(array $file_results): void
    {
        $dirs = $this->get_matching_dirs($file_results);
        if (count($dirs) > 0) {
            Logger::log_msg(sprintf("\nMatching directories (%d):", count($dirs)));
            foreach ($dirs as $d) {
                Logger::log_msg($d);
            }
        } else {
            Logger::log_msg("\nMatching directories: 0");
        }
    }

    /**
     * @param FileResult[] $file_results
     * @return string[]
     */
    public function get_matching_files(array $file_results): array
    {
        $file_paths = array();
        foreach ($file_results as $f) {
            if (!in_array($f->file_name, $file_paths)) {
                $file_paths[] = FileUtil::join_path($f->path, $f->file_name);
            }
        }
        return $file_paths;
    }

    /**
     * @param FileResult[] $file_results
     * @return void
     */
    public function print_matching_files(array $file_results): void
    {
        $file_paths = $this->get_matching_files($file_results);
        if (count($file_paths) > 0) {
            Logger::log_msg(sprintf("\nMatching files (%d):", count($file_paths)));
            foreach ($file_paths as $f) {
                Logger::log_msg($f);
            }
        } else {
            Logger::log_msg("\nMatching files: 0");
        }
    }
}
