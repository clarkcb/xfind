<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Enum FileType
 */
enum FileType: string
{
    case Unknown = 'unknown';
    case Archive = 'archive';
    case Audio = 'audio';
    case Binary = 'binary';
    case Code = 'code';
    case Font = 'font';
    case Image = 'image';
    case Text = 'text';
    case Video = 'video';
    case Xml = 'xml';
}
