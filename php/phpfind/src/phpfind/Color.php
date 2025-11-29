<?php

namespace phpfind;

/**
 * Enum Color
 */
enum Color: string
{
    case Black = 'black';
    case Red = 'red';
    case Green = 'green';
    case Yellow = 'yellow';
    case Blue = 'blue';
    case Magenta = 'magenta';
    case Cyan = 'cyan';
    case White = 'white';

    public function to_console_color(): ConsoleColor {
        return match ($this) {
            Color::Black => ConsoleColor::Black,
            Color::Red => ConsoleColor::Red,
            Color::Green => ConsoleColor::Green,
            Color::Yellow => ConsoleColor::Yellow,
            Color::Blue => ConsoleColor::Blue,
            Color::Magenta => ConsoleColor::Magenta,
            Color::Cyan => ConsoleColor::Cyan,
            Color::White => ConsoleColor::White
        };
    }
}
