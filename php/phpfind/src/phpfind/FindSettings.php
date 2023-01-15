<?php declare(strict_types=1);

namespace phpfind;

use DateTime;

/**
 * Class FindSettings
 *
 * @property bool archivesonly
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
 * @property DateTime maxlastmod
 * @property int maxsize
 * @property DateTime minlastmod
 * @property int minsize
 * @property array out_archiveextensions
 * @property array out_archivefilepatterns
 * @property array out_dirpatterns
 * @property array out_extensions
 * @property array out_filepatterns
 * @property array out_filetypes
 * @property array paths
 * @property bool printusage
 * @property bool printversion
 * @property bool recursive
 * @property SortBy sortby
 * @property bool sort_caseinsensitive
 * @property bool sort_descending
 * @property bool verbose
 */
class FindSettings
{
    public bool $archivesonly = false;
    public bool $debug = false;
    public bool $excludehidden = true;
    public array $in_archiveextensions = array();
    public array $in_archivefilepatterns = array();
    public array $in_dirpatterns = array();
    public array $in_extensions = array();
    public array $in_filepatterns = array();
    public array $in_filetypes = array();
    public bool $includearchives = false;
    public bool $listdirs = false;
    public bool $listfiles = false;
    public ?DateTime $maxlastmod = null;
    public int $maxsize = 0;
    public ?DateTime $minlastmod = null;
    public int $minsize = 0;
    public array $out_archiveextensions = array();
    public array $out_archivefilepatterns = array();
    public array $out_dirpatterns = array();
    public array $out_extensions = array();
    public array $out_filepatterns = array();
    public array $out_filetypes = array();
    public array $paths = array();
    public bool $printusage = false;
    public bool $printversion = false;
    public bool $recursive = true;
    public bool $sort_caseinsensitive = false;
    public bool $sort_descending = false;
    public SortBy $sortby = SortBy::Filepath;
    public bool $verbose = false;

    /**
     * @param $ext
     * @param $exts
     * @return void
     */
    public function add_exts($ext, &$exts): void
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

    /**
     * @param $filetype
     * @param $filetypes
     * @return void
     */
    public function add_filetypes($filetype, &$filetypes): void
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

    /**
     * @param $pattern
     * @param $patterns
     * @return void
     */
    public function add_patterns($pattern, &$patterns): void
    {
        if (gettype($pattern) == 'string') {
            $patterns[] = $pattern;
        } elseif (gettype($pattern) == 'array') {
            foreach ($pattern as $p) {
                $patterns[] = $p;
            }
        }
    }

    public function need_stat(): bool
    {
        return $this->sortby == SortBy::Filesize || $this->sortby == SortBy::LastMod ||
            $this->maxlastmod != null || $this->minlastmod != null ||
            $this->maxsize > 0 || $this->minsize > 0;
    }

    /**
     * @param bool $b
     * @return void
     */
    public function set_archivesonly(bool $b): void
    {
        $this->archivesonly = $b;
        if ($b) {
            $this->includearchives = $b;
        }
    }

    /**
     * @param bool $b
     * @return void
     */
    public function set_debug(bool $b): void
    {
        $this->debug = $b;
        if ($b) {
            $this->verbose = $b;
        }
    }

    public function set_sort_by(string $sort_by_name): void
    {
        switch (strtoupper($sort_by_name)) {
            case 'NAME':
                $this->sortby = SortBy::Filename;
                break;
            case 'SIZE':
                $this->sortby = SortBy::Filesize;
                break;
            case 'TYPE':
                $this->sortby = SortBy::Filetype;
                break;
            case 'LASTMOD':
                $this->sortby = SortBy::LastMod;
                break;
            default:
                $this->sortby = SortBy::Filepath;
                break;
        }
    }

    /**
     * @return string
     */
    public function __toString(): string
    {
        return sprintf('FindSettings(' .
            'archivesonly: %s' .
            ', debug: %s' .
            ', excludehidden: %s' .
            ', in_archiveextensions: %s' .
            ', in_archivefilepatterns: %s' .
            ', in_dirpatterns: %s' .
            ', in_extensions: %s' .
            ', in_filepatterns: %s' .
            ', in_filetypes: %s' .
            ', includearchives: %s' .
            ', listdirs: %s' .
            ', listfiles: %s' .
            ', maxlastmod: %s' .
            ', maxsize: %d' .
            ', minlastmod: %s' .
            ', minsize: %d' .
            ', out_archiveextensions: %s' .
            ', out_archivefilepatterns: %s' .
            ', out_dirpatterns: %s' .
            ', out_extensions: %s' .
            ', out_filepatterns: %s' .
            ', out_filetypes: %s' .
            ', paths: %s' .
            ', printusage: %s' .
            ', printversion: %s' .
            ', recursive: %s' .
            ', sort_caseinsensitive: %s' .
            ', sort_descending: %s' .
            ', sortby: %s' .
            ', verbose: %s' .
            ')',
            StringUtil::bool_to_string($this->archivesonly),
            StringUtil::bool_to_string($this->debug),
            StringUtil::bool_to_string($this->excludehidden),
            StringUtil::string_array_to_string($this->in_archiveextensions),
            StringUtil::string_array_to_string($this->in_archivefilepatterns),
            StringUtil::string_array_to_string($this->in_dirpatterns),
            StringUtil::string_array_to_string($this->in_extensions),
            StringUtil::string_array_to_string($this->in_filepatterns),
            StringUtil::filetype_array_to_string($this->in_filetypes),
            StringUtil::bool_to_string($this->includearchives),
            StringUtil::bool_to_string($this->listdirs),
            StringUtil::bool_to_string($this->listfiles),
            StringUtil::datetime_to_string($this->maxlastmod),
            $this->maxsize,
            StringUtil::datetime_to_string($this->minlastmod),
            $this->minsize,
            StringUtil::string_array_to_string($this->out_archiveextensions),
            StringUtil::string_array_to_string($this->out_archivefilepatterns),
            StringUtil::string_array_to_string($this->out_dirpatterns),
            StringUtil::string_array_to_string($this->out_extensions),
            StringUtil::string_array_to_string($this->out_filepatterns),
            StringUtil::filetype_array_to_string($this->out_filetypes),
            StringUtil::string_array_to_string($this->paths),
            StringUtil::bool_to_string($this->printusage),
            StringUtil::bool_to_string($this->printversion),
            StringUtil::bool_to_string($this->recursive),
            StringUtil::bool_to_string($this->sort_caseinsensitive),
            StringUtil::bool_to_string($this->sort_descending),
            $this->sortby->name,
            StringUtil::bool_to_string($this->verbose)
        );
    }
}
