<?php declare(strict_types=1);

namespace phpfind;

/**
 * Enum SortBy
 */
enum SortBy: string
{
    case Filepath = 'filepath';
    case Filename = 'filename';
    case Filesize = 'filesize';
    case Filetype = 'filetype';
    case LastMod = 'lastmod';
}
