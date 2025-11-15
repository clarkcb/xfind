<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Enum ConsoleColor
 */
enum ConsoleColor: string
{
    case Reset = "\033[0m";
    case Black = "\033[0;30m";
    case Red = "\033[0;31m";
    case Green = "\033[0;32m";
    case Yellow = "\033[0;33m";
    case Blue = "\033[0;34m";
    case Magenta = "\033[0;35m";
    case Cyan = "\033[0;36m";
    case White = "\033[0;37m";

    case BoldBlack = "\033[1;30m";
    case BoldRed = "\033[1;31m";
    case BoldGreen = "\033[1;32m";
    case BoldYellow = "\033[1;33m";
    case BoldBlue = "\033[1;34m";
    case BoldMagenta = "\033[1;35m";
    case BoldCyan = "\033[1;36m";
    case BoldWhite = "\033[1;37m";
}
