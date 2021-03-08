<?php declare(strict_types=1);

namespace phpfind;

/**
 * Class FindSettings
 *
 * @property bool archivesonly
 * @property bool colorize
 * @property bool debug
 * @property bool excludehidden
 * @property array in_archiveextensions
 * @property array in_archivefilepatterns
 * @property array in_dirpatterns
 * @property array in_extensions
 * @property array in_filepatterns
 * @property array in_filetypes
 * @property bool includearchives
 * @property bool listdirs
 * @property bool listfiles
 * @property array out_archiveextensions
 * @property array out_archivefilepatterns
 * @property array out_dirpatterns
 * @property array out_extensions
 * @property array out_filepatterns
 * @property array out_filetypes
 * @property bool printusage
 * @property bool printversion
 * @property bool recursive
 * @property array paths
 * @property bool verbose
 */
class FindSettings
{
    public $archivesonly = false;
    public $colorize = true;
    public $debug = false;
    public $excludehidden = true;
    public $includearchives = false;
    public $listdirs = false;
    public $listfiles = false;
    public $printusage = false;
    public $printversion = false;
    public $recursive = true;
    public $findarchives = false;
    public $verbose = false;

    public $in_archiveextensions = array();
    public $in_archivefilepatterns = array();
    public $in_dirpatterns = array();
    public $in_extensions = array();
    public $in_filepatterns = array();
    public $in_filetypes = array();
    public $out_archiveextensions = array();
    public $out_archivefilepatterns = array();
    public $out_dirpatterns = array();
    public $out_extensions = array();
    public $out_filepatterns = array();
    public $out_filetypes = array();
    public $paths = array();

    public function add_exts($ext, &$exts)
    {
        if (gettype($ext) == 'string') {
            $xs = explode(',', $ext);
            foreach ($xs as $x) {
                $exts[] = $x;
            }
        } elseif (gettype($ext) == 'array') {
            foreach ($ext as $x) {
                $exts[] = $x;
            }
        }
    }

    public function add_filetypes($filetype, &$filetypes)
    {
        if (gettype($filetype) == 'string') {
            $fts = explode(',', $filetype);
            foreach ($fts as $ft) {
                $filetypes[] = FileTypes::from_name($ft);
            }
        } elseif (gettype($filetype) == 'array') {
            foreach ($filetype as $ft) {
                $filetypes[] = FileTypes::from_name($ft);
            }
        }
    }

    public function add_patterns($pattern, &$patterns)
    {
        if (gettype($pattern) == 'string') {
            $patterns[] = $pattern;
        } elseif (gettype($pattern) == 'array') {
            foreach ($pattern as $p) {
                $patterns[] = $p;
            }
        }
    }

    public function set_archivesonly(bool $b)
    {
        $this->archivesonly = $b;
        if ($b) {
            $this->findarchives = $b;
        }
    }

    public function set_debug(bool $b)
    {
        $this->debug = $b;
        if ($b) {
            $this->verbose = $b;
        }
    }

    private function arr_to_string(array $arr): string
    {
        $s = '["' . implode('","', $arr) . '"]';
        return $s;
    }

    private function bool_to_string(bool $b): string
    {
        return $b ? 'true' : 'false';
    }

    public function __toString(): string
    {
        $s = 'FindSettings(';
        $s .= 'archivesonly: ' . $this->bool_to_string($this->archivesonly);
        $s .= ', colorize: ' . $this->bool_to_string($this->colorize);
        $s .= ', debug: ' . $this->bool_to_string($this->debug);
        $s .= ', excludehidden: ' . $this->bool_to_string($this->excludehidden);
        $s .= ', in_archiveextensions: ' . $this->arr_to_string($this->in_archiveextensions);
        $s .= ', in_archivefilepatterns: ' . $this->arr_to_string($this->in_archivefilepatterns);
        $s .= ', in_dirpatterns: ' . $this->arr_to_string($this->in_dirpatterns);
        $s .= ', in_extensions: ' . $this->arr_to_string($this->in_extensions);
        $s .= ', in_filepatterns: ' . $this->arr_to_string($this->in_filepatterns);
        $s .= ', in_filetypes: ' . $this->arr_to_string($this->in_filetypes);
        $s .= ', includearchives: ' . $this->bool_to_string($this->includearchives);
        $s .= ', listdirs: ' . $this->bool_to_string($this->listdirs);
        $s .= ', listfiles: ' . $this->bool_to_string($this->listfiles);
        $s .= ', out_archiveextensions: ' . $this->arr_to_string($this->out_archiveextensions);
        $s .= ', out_archivefilepatterns: ' . $this->arr_to_string($this->out_archivefilepatterns);
        $s .= ', out_dirpatterns: ' . $this->arr_to_string($this->out_dirpatterns);
        $s .= ', out_extensions: ' . $this->arr_to_string($this->out_extensions);
        $s .= ', out_filepatterns: ' . $this->arr_to_string($this->out_filepatterns);
        $s .= ', out_filetypes: ' . $this->arr_to_string($this->out_filetypes);
        $s .= ', printusage: ' . $this->bool_to_string($this->printusage);
        $s .= ', printversion: ' . $this->bool_to_string($this->printversion);
        $s .= ', recursive: ' . $this->bool_to_string($this->recursive);
        $s .= ', paths: ' . $this->arr_to_string($this->paths);
        $s .= ', verbose: ' . $this->bool_to_string($this->verbose);
        $s .= ')';
        return $s;
    }
}
