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
class FindFile
{
    const CONTAINER_SEPARATOR = '!';

    public function __construct(string $path, string $filename, $filetype)
    {
        $this->containers = array();
        $this->path = $path;
        $this->filename = $filename;
        $this->filetype = $filetype;
    }

    public function filepath(): string
    {
        return FileUtil::join_path($this->path, $this->filename);
    }

    public function __toString(): string
    {
        $s = "";
        if ($this->containers) {
            $s = join(FindFile::CONTAINER_SEPARATOR, $this->containers) .
                FindFile::CONTAINER_SEPARATOR;
        }
        $s .= FileUtil::join_path($this->path, $this->filename);
        return $s;
    }
}
