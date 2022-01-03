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

    private function any_matches_any_pattern(array $slist, array $patterns): bool
    {
        foreach ($slist as $s) {
            if ($this->matches_any_pattern($s, $patterns)) {
                return true;
            }
        }
        return false;
    }

    public function is_find_dir(string $d): bool
    {
        if (FileUtil::is_dot_dir($d)) {
            return true;
        }
        if ($this->settings->excludehidden && FileUtil::is_hidden($d)) {
            return false;
        }
        $path_elems = FileUtil::split_path($d);
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

    public function is_find_file(string $f): bool
    {
        $ext = FileUtil::get_extension($f);
        if ($this->settings->in_extensions && !in_array($ext, $this->settings->in_extensions)) {
            return false;
        }
        if ($this->settings->out_extensions && in_array($ext, $this->settings->out_extensions)) {
            return false;
        }
        if ($this->settings->in_filepatterns &&
            !$this->matches_any_pattern($f, $this->settings->in_filepatterns)) {
            return false;
        }
        if ($this->settings->out_filepatterns &&
            $this->matches_any_pattern($f, $this->settings->out_filepatterns)) {
            return false;
        }
        $type = $this->filetypes->get_filetype($f);
        if ($this->settings->in_filetypes && !in_array($type, $this->settings->in_filetypes)) {
            return false;
        }
        if ($this->settings->out_filetypes && in_array($type, $this->settings->out_filetypes)) {
            return false;
        }
        return true;
    }

    public function is_archive_find_file(string $f): bool
    {
        $ext = FileUtil::get_extension($f);
        if ($this->settings->in_archiveextensions &&
            !in_array($ext, $this->settings->in_archiveextensions)) {
            return false;
        }
        if ($this->settings->out_archiveextensions &&
            in_array($ext, $this->settings->out_archiveextensions)) {
            return false;
        }
        if ($this->settings->in_archivefilepatterns &&
            !$this->matches_any_pattern($f, $this->settings->in_archivefilepatterns)) {
            return false;
        }
        if ($this->settings->out_archivefilepatterns &&
            $this->matches_any_pattern($f, $this->settings->out_archivefilepatterns)) {
            return false;
        }
        return true;
    }

    private function get_non_dot_dirs(string $d): array
    {
        $filter_non_dot_dirs = function ($f) use ($d) {
            return (is_dir(FileUtil::join_path($d, $f)) && !FileUtil::is_dot_dir($f));
        };
        return array_filter(scandir($d), $filter_non_dot_dirs);
    }

    private function get_dir_find_dirs(string $d): array
    {
        $filter_dirs = function ($f) use ($d) {
            return is_dir(FileUtil::join_path($d, $f)) && $this->is_find_dir($f);
        };
        $join_path = function ($f) use ($d) {
            return FileUtil::join_path($d, $f);
        };
        $finddirs = array_filter($this->get_non_dot_dirs($d), $filter_dirs);
        return array_map($join_path, $finddirs);
    }

    private function get_dir_find_files(string $d): array
    {
        $filter_files = function ($f) use ($d) {
            return is_file(FileUtil::join_path($d, $f)) && $this->filter_file($f);
        };
        $to_find_file = function ($f) use ($d) {
            return new FindFile($d, $f, $this->filetypes->get_filetype($f));
        };
        $findfiles = array_filter(scandir($d), $filter_files);
        return array_map($to_find_file, $findfiles);
    }

    private function rec_get_find_files(string $d): array
    {
        $finddirs = $this->get_dir_find_dirs($d);
        $findfiles = $this->get_dir_find_files($d);
        foreach ($finddirs as $finddir) {
            $findfiles = array_merge($findfiles, $this->rec_get_find_files($finddir));
        }
        return $findfiles;
    }

    public function filter_file(string $f): bool
    {
        if ($this->settings->excludehidden && FileUtil::is_hidden($f)) {
            return false;
        }
        if ($this->filetypes->is_archive($f)) {
            return $this->settings->includearchives && $this->is_archive_find_file($f);
        }
        return !$this->settings->archivesonly && $this->is_find_file($f);
    }

    /**
     * @throws FindException
     */
    public function find(): array
    {
        $findfiles = array();
        foreach ($this->settings->paths as $p) {
            if (is_dir($p)) {
                if ($this->is_find_dir($p)) {
                    if ($this->settings->recursive) {
                        $findfiles = array_merge($findfiles, $this->rec_get_find_files($p));
                    } else {
                        $findfiles = array_merge($findfiles, $this->get_dir_find_files($p));
                    }
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            } elseif (is_file($p)) {
                if ($this->filter_file($p)) {
                    $findfiles[] = $p;
                } else {
                    throw new FindException("Startpath does not match find settings");
                }
            }
        }
        sort($findfiles);
        return $findfiles;
    }

    public function get_matching_dirs(array $findfiles): array
    {
        $dirs = array();
        foreach ($findfiles as $f) {
            if (!in_array($f->path, $dirs)) {
                $dirs[] = $f->path;
            }
        }
        sort($dirs);
        return $dirs;
    }

    public function print_matching_dirs(array $findfiles): void
    {
        $dirs = $this->get_matching_dirs($findfiles);
        if (count($dirs) > 0) {
            Logger::log_msg(sprintf("\nMatching directories (%d):", count($dirs)));
            foreach ($dirs as $d) {
                Logger::log_msg($d);
            }
        } else {
            Logger::log_msg("\nMatching directories: 0");
        }
    }

    public function get_matching_files(array $findfiles): array
    {
        $files = array();
        foreach ($findfiles as $f) {
            if (!in_array($f->filename, $files)) {
                $files[] = FileUtil::join_path($f->path, $f->filename);
            }
        }
        sort($files);
        return $files;
    }

    public function print_matching_files(array $findfiles): void
    {
        $files = $this->get_matching_files($findfiles);
        if (count($files) > 0) {
            Logger::log_msg(sprintf("\nMatching files (%d):", count($files)));
            foreach ($files as $f) {
                Logger::log_msg($f);
            }
        } else {
            Logger::log_msg("\nMatching files: 0");
        }
    }
}
