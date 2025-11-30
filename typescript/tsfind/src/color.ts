/*
 * color.ts
 *
 * Generic colors
 */

'use strict'

import {ConsoleColor} from "./consolecolor";

export enum Color {
    BLACK = 'black',
    RED = 'red',
    GREEN = 'green',
    YELLOW = 'yellow',
    BLUE = 'blue',
    MAGENTA = 'magenta',
    CYAN = 'cyan',
    WHITE = 'white',
}

export function colorToConsoleColor(color: Color): ConsoleColor {
    switch (color) {
        case Color.BLACK:
            return ConsoleColor.BLACK;
        case Color.RED:
            return ConsoleColor.RED;
        case Color.GREEN:
            return ConsoleColor.GREEN;
        case Color.YELLOW:
            return ConsoleColor.YELLOW;
        case Color.BLUE:
            return ConsoleColor.BLUE;
        case Color.MAGENTA:
            return ConsoleColor.MAGENTA;
        case Color.CYAN:
            return ConsoleColor.CYAN;
        case Color.WHITE:
            return ConsoleColor.WHITE;
    }
}
