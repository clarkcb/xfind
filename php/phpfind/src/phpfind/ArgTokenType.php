<?php

namespace phpfind;

/**
 * Enum ArgTokenType
 */
enum ArgTokenType: int
{
    case Unknown = 0;
    case Bool = 1;
    case Str = 2;
    case Int = 3;
}