namespace FsFindLib

type Color = 
    | Black = 0
    | Red = 1
    | Green = 2
    | Yellow = 3
    | Blue  = 4
    | Magenta  = 5
    | Cyan  = 6
    | White  = 7

module ColorUtil = 
    
    let ColorFromName (name : string) : Color =
        let lname = name.ToLowerInvariant()
        if lname.Equals("black") then Color.Black
        else if lname.Equals("red") then Color.Red
        else if lname.Equals("green") then Color.Green
        else if lname.Equals("yellow") then Color.Yellow
        else if lname.Equals("blue") then Color.Blue
        else if lname.Equals("magenta") then Color.Magenta
        else if lname.Equals("cyan") then Color.Cyan
        else if lname.Equals("white") then Color.White
        else Color.Black

    let ColorToName (color : Color) : string =
        if color = Color.Black then "black"
        else if color = Color.Red then "red"
        else if color = Color.Green then "green"
        else if color = Color.Yellow then "yellow"
        else if color = Color.Blue then "blue"
        else if color = Color.Magenta then "magenta"
        else if color = Color.Cyan then "cyan"
        else if color = Color.White then "white"
        else "black"

    let ColorToConsoleColor (color : Color) : string = 
        match color with
        | Color.Black -> ConsoleColor.Black
        | Color.Red -> ConsoleColor.Red
        | Color.Green -> ConsoleColor.Green
        | Color.Yellow -> ConsoleColor.Yellow
        | Color.Blue -> ConsoleColor.Blue
        | Color.Magenta -> ConsoleColor.Magenta
        | Color.Cyan -> ConsoleColor.Cyan
        | Color.White -> ConsoleColor.White
        | _    -> ConsoleColor.Black

    let Colorize (s : string) (matchStartIndex : int) (matchEndIndex : int) (color : Color) : string =
        let prefix =
            if matchStartIndex > 0
            then s.Substring(0, matchStartIndex)
            else ""
        let suffix =
            if matchEndIndex < s.Length
            then s.Substring(matchEndIndex)
            else ""
        let matchLength = matchEndIndex - matchStartIndex
        prefix +
            (ColorToConsoleColor color) + 
            s.Substring(matchStartIndex, matchLength) +
            ConsoleColor.Reset + 
            suffix

;;
