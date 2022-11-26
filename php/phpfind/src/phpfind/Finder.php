<?php declare(strict_types=1);

namespace phpfind;

//require_once __DIR__ . '/../autoload.php';
//require_once __DIR__ . '/common.php';

/**
 * Class Finder
 *
 * @property FindSettings $settings
 * @property FileTypes $filetypes
 */
class Finder
{
    private readonly FindSettings $settings;
    private readonly FileTypes $filetypes;

    /**
     * @param FindSettings $settings
     * @throws FindException
     */
    public function __construct(FindSettings $settings)
    {
        $this->settings = $settings;
        $this->filetypes = new FileTypes();
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
    }

    /**
     * @param string $s
     * @param array $patterns
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
     * @param array $slist
     * @param array $patterns
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
        if ($this->settings->excludehidden && FileUtil::is_hidden($dir)) {
            return false;
        }
        $path_elems = FileUtil::split_path($dir);
        if ($this->settings->in_dirpatterns &&
            !$this->any_matches_any_pattern($path_elems, $this->settings->in_dirpatterns)) {
            return false;
        }
        if ($this->settings->out_dirpatterns &&
            $this->any_matches_any_pattern($path_elems, $this->settings->out_dirpatterns)) {
            return false;
        }
        return true;
    }

    /**
     * @param string $filepath
     * @return FileResult
     */
    private function filepath_to_fileresult(string $filepath) {
        $path_and_filename = FileUtil::split_to_path_and_filename($filepath);
        $path = $path_and_filename[0];
        $filename = $path_and_filename[1];
        return new FileResult($path, $filename, $this->filetypes->get_filetype($filename));
    }

    /**
     * @param string $f
     * @return bool
     */
    public function is_matching_file(string $f): bool
    {
        return $this->is_matching_file_result($this->filepath_to_fileresult($f));
    }

    /**
     * @param FileResult $fr
     * @return bool
     */
    public function is_matching_file_result(FileResult $fr): bool
    {
        $ext = FileUtil::get_extension($fr->filename);
        if ($this->settings->in_extensions && !in_array($ext, $this->settings->in_extensions)) {
            return false;
        }
        if ($this->settings->out_extensions && in_array($ext, $this->settings->out_extensions)) {
            return false;
        }
        if ($this->settings->in_filepatterns &&
            !$this->matches_any_pattern($fr->filename, $this->settings->in_filepatterns)) {
            return false;
        }
        if ($this->settings->out_filepatterns &&
            $this->matches_any_pattern($fr->filename, $this->settings->out_filepatterns)) {
            return false;
        }
        if ($this->settings->in_filetypes && !in_array($fr->filetype, $this->settings->in_filetypes)) {
            return false;
        }
        if ($this->settings->out_filetypes && in_array($fr->filetype, $this->settings->out_filetypes)) {
            return false;
        }
        return true;
    }

    /**
     * @param string $filename
     * @return bool
     */
    public function is_matching_archive_file(string $filename): bool
    {
        $ext = FileUtil::get_extension($filename);
        if ($this->settings->in_archiveextensions &&
            !in_array($ext, $this->settings->in_archiveextensions)) {
            return false;
        }
        if ($this->settings->out_archiveextensions &&
            in_array($ext, $this->settings->out_archiveextensions)) {
            return false;
        }
        if ($this->settings->in_archivefilepatterns &&
            !$this->matches_any_pattern($filename, $this->settings->in_archivefilepatterns)) {
            return false;
        }
        if ($this->settings->out_archivefilepatterns &&
            $this->matches_any_pattern($filename, $this->settings->out_archivefilepatterns)) {
            return false;
        }
        return true;
    }

    /**
     * @param string $dir
     * @param string $filename
     * @return FileResult|null
     */
    public function filter_to_file_result(string $dir, string $filename): ?FileResult
    {
        $filepath = FileUtil::join_path($dir, $filename);
        if ($this->settings->excludehidden && FileUtil::is_hidden($filepath)) {
            return null;
        }
        $fileresult = new FileResult($dir, $filename, $this->filetypes->get_filetype($filename));
        if ($fileresult->filetype == FileType::Archive) {
            if ($this->settings->includearchives && $this->is_matching_archive_file($filepath)) {
                return $fileresult;
            }
            return null;
        }
        if (!$this->settings->archivesonly && $this->is_matching_file_result($fileresult)) {
            return $fileresult;
        }
        return null;
    }

    private function get_non_dot_dirs(string $dir): array
    {
        $filter_non_dot_dirs = function ($f) use ($dir) {
            return (is_dir(FileUtil::join_path($dir, $f)) && !FileUtil::is_dot_dir($f));
        };
        return array_filter(scandir($dir), $filter_non_dot_dirs);
    }

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

    private function get_dir_file_results(string $dir): array
    {
        $fileresults = array();
        foreach (scandir($dir) as $entry) {
            if (is_file(FileUtil::join_path($dir, $entry))) {
                $fileresult = $this->filter_to_file_result($dir, $entry);
                if ($fileresult != null) {
                    $fileresults[] = $fileresult;
                }
            }
        }
        return $fileresults;
    }

    private function rec_get_file_results(string $dir): array
    {
        $dirresults = $this->get_dir_dir_results($dir);
        $fileresults = $this->get_dir_file_results($dir);
        foreach ($dirresults as $dirresult) {
            $fileresults = array_merge($fileresults, $this->rec_get_file_results($dirresult));
        }
        return $fileresults;
    }

    /**
     * @throws FindException
     */
    public function find(): array
    {
        $fileresults = array();
        foreach ($this->settings->paths as $p) {
            if (is_dir($p)) {
                if ($this->is_matching_dir($p)) {
                    if ($this->settings->recursive) {
                        $fileresults = array_merge($fileresults, $this->rec_get_file_results($p));
                    } else {
                        $fileresults = array_merge($fileresults, $this->get_dir_file_results($p));
                    }
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            } elseif (is_file($p)) {
                $d = dirname($p);
                $f = basename($p);
                $fileresult = $this->filter_to_file_result($d, $f);
                if ($fileresult != null) {
                    $fileresults[] = $fileresult;
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            }
        }
        return $this->sort_file_results($fileresults);
    }

    private function cmp_file_result_path(FileResult $fr1, FileResult $fr2): int
    {
        if ($fr1->path == $fr2->path) {
            return ($fr1->filename < $fr2->filename) ? -1 : 1;
        }
        return ($fr1->path < $fr2->path) ? -1 : 1;
    }

    private function cmp_file_result_filename(FileResult $fr1, FileResult $fr2): int
    {
        if ($fr1->filename == $fr2->filename) {
            return ($fr1->path < $fr2->path) ? -1 : 1;
        }
        return ($fr1->filename < $fr2->filename) ? -1 : 1;
    }

    private function cmp_file_result_filetype(FileResult $fr1, FileResult $fr2): int
    {
        if ($fr1->filetype == $fr2->filetype) {
            return $this->cmp_file_result_path($fr1, $fr2);
        }
        return ($fr1->filetype < $fr2->filetype) ? -1 : 1;
    }

    private function sort_file_results(array $fileresults): array
    {
        switch ($this->settings->sortby) {
            case SortBy::Filename:
                print 'SortBy::Filename';
                usort($fileresults, 'self::cmp_file_result_filename');
                break;
            case SortBy::Filetype:
                usort($fileresults, 'self::cmp_file_result_filetype');
                break;
            default:
                usort($fileresults, 'self::cmp_file_result_path');
                break;
        }
        if ($this->settings->sort_descending) {
            return array_reverse($fileresults);
        }
        return $fileresults;
    }

    /**
     * @param array $fileresults
     * @return array
     */
    public function get_matching_dirs(array $fileresults): array
    {
        $dirs = array();
        foreach ($fileresults as $f) {
            if (!in_array($f->path, $dirs)) {
                $dirs[] = $f->path;
            }
        }
        sort($dirs);
        return $dirs;
    }

    /**
     * @param array $fileresults
     * @return void
     */
    public function print_matching_dirs(array $fileresults): void
    {
        $dirs = $this->get_matching_dirs($fileresults);
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
     * @param array $fileresults
     * @return array
     */
    public function get_matching_files(array $fileresults): array
    {
        $filepaths = array();
        foreach ($fileresults as $f) {
            if (!in_array($f->filename, $filepaths)) {
                $filepaths[] = FileUtil::join_path($f->path, $f->filename);
            }
        }
        return $filepaths;
    }

    /**
     * @param array $fileresults
     * @return void
     */
    public function print_matching_files(array $fileresults): void
    {
        $filepaths = $this->get_matching_files($fileresults);
        if (count($filepaths) > 0) {
            Logger::log_msg(sprintf("\nMatching files (%d):", count($filepaths)));
            foreach ($filepaths as $f) {
                Logger::log_msg($f);
            }
        } else {
            Logger::log_msg("\nMatching files: 0");
        }
    }
}
