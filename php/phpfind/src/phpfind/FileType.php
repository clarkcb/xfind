<?php declare(strict_types=1);

namespace phpfind;

/**
 * Enum FileType
 */
enum FileType
{
    case Unknown;
    case Archive;
    case Audio;
    case Binary;
    case Code;
    case Font;
    case Image;
    case Text;
    case Video;
    case Xml;
}
