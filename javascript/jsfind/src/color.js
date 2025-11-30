/*
 * color.js
 *
 * Generic colors
 *
 */
const {ConsoleColor} = require('./consolecolor');

const Color = Object.freeze({
    BLACK: 'black',
    RED: 'red',
    GREEN: 'green',
    YELLOW: 'yellow',
    BLUE: 'blue',
    MAGENTA: 'magenta',
    CYAN: 'cyan',
    WHITE: 'white',
});

const nameToColor = name => {
    switch (name.toLowerCase()) {
        case 'black':
            return Color.BLACK;
        case 'red':
            return Color.RED;
        case 'green':
            return Color.GREEN;
        case 'yellow':
            return Color.YELLOW;
        case 'blue':
            return Color.BLUE;
        case 'magenta':
            return Color.MAGENTA;
        case 'cyan':
            return Color.CYAN;
        case 'white':
            return Color.WHITE;
        default:
            return Color.BLACK;
    }
};

const colorToConsoleColor = (color) => {
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
};

module.exports = {Color, nameToColor, colorToConsoleColor};
