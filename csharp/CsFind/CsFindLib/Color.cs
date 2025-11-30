using System;

namespace CsFindLib;

public enum Color
{
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White
}


public static class ColorUtil
{
    public static Color GetColorFromName(string colorName)
    {
        return Enum.TryParse<Color>(colorName, out var color) ? color : Color.Black;
    }

    public static string GetNameFromColor(Color color)
    {
        return color switch
        {
            Color.Black => "black",
            Color.Red => "red",
            Color.Green => "green",
            Color.Yellow => "yellow",
            Color.Blue => "blue",
            Color.Magenta => "magenta",
            Color.Cyan => "cyan",
            Color.White => "white",
            _ => "black"
        };
    }

    public static string ColorToConsoleColor(Color color, bool bold = false)
    {
        return color switch
        {
            Color.Black => bold ? ConsoleColor.BoldBlack : ConsoleColor.Black,
            Color.Red => bold ? ConsoleColor.BoldRed : ConsoleColor.Red,
            Color.Green => bold ? ConsoleColor.BoldGreen : ConsoleColor.Green,
            Color.Yellow => bold ? ConsoleColor.BoldYellow : ConsoleColor.Yellow,
            Color.Blue => bold ? ConsoleColor.BoldBlue : ConsoleColor.Blue,
            Color.Magenta => bold ? ConsoleColor.BoldMagenta : ConsoleColor.Magenta,
            Color.Cyan => bold ? ConsoleColor.BoldCyan : ConsoleColor.Cyan,
            Color.White => bold ? ConsoleColor.BoldWhite : ConsoleColor.White,
            _ => bold ? ConsoleColor.BoldBlack : ConsoleColor.Black
        };
    }

}
