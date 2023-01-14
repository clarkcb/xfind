<?php declare(strict_types=1);

namespace phpfind;

/**
 * Enum SortBy
 */
enum SortBy
{
    case Filepath;
    case Filename;
    case Filesize;
    case Filetype;
    case LastMod;
}
