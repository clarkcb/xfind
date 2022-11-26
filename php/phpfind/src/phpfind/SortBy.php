<?php declare(strict_types=1);

namespace phpfind;

/**
 * Enum SortBy
 */
enum SortBy
{
    case Filepath;
    case Filename;
    case Filetype;
    case Filesize;
    case LastMod;
}
