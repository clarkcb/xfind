namespace FsFindLib

module ConsoleColor =
    let Reset   = "\u001B[0m"
    let Black   = "\u001B[0;30m"
    let Red     = "\u001B[0;31m"
    let Green   = "\u001B[0;32m"
    let Yellow  = "\u001B[0;33m"
    let Blue    = "\u001B[0;34m"
    let Magenta = "\u001B[0;35m"
    let Cyan    = "\u001B[0;36m"
    let White   = "\u001B[0;37m"

    let BoldBlack   = "\u001B[1;30m"
    let BoldRed     = "\u001B[1;31m"
    let BoldGreen   = "\u001B[1;32m"
    let BoldYellow  = "\u001B[1;33m"
    let BoldBlue    = "\u001B[1;34m"
    let BoldMagenta = "\u001B[1;35m"
    let BoldCyan    = "\u001B[1;36m"
    let BoldWhite   = "\u001B[1;37m"

    let Colorize (s : string) (matchStartIndex : int) (matchEndIndex : int) : string =
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
            Green + 
            s.Substring(matchStartIndex, matchLength) +
            Reset + 
            suffix
;;
