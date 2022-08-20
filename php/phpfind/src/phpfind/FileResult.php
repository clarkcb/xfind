<?php declare(strict_types=1);

namespace phpfind;

/**
 * Class FindFile
 *
 * @property array containers
 * @property string path
 * @property string filename
 * @property FileType filetype
 */
class FileResult
{
    const CONTAINER_SEPARATOR = '!';

    public array $containers;
    public readonly string $path;
    public readonly string $filename;
    public readonly FileType $filetype;

    /**
     * @param string $path
     * @param string $filename
     * @param FileType $filetype
     */
    public function __construct(string $path, string $filename, FileType $filetype)
    {
        $this->containers = array();
        $this->path = $path;
        $this->filename = $filename;
        $this->filetype = $filetype;
    }

    /**
     * @return string
     */
    public function filepath(): string
    {
        return FileUtil::join_path($this->path, $this->filename);
    }

    /**
     * @return string
     */
    public function __toString(): string
    {
        $s = "";
        if ($this->containers) {
            $s = join(FileResult::CONTAINER_SEPARATOR, $this->containers) .
                FileResult::CONTAINER_SEPARATOR;
        }
        $s .= FileUtil::join_path($this->path, $this->filename);
        return $s;
    }
}
