namespace FsFindLib

module Color =
    let Reset =  "\u001B[0m"
    let Black =  "\u001B[30m"
    let Red =    "\u001B[31m"
    let Green =  "\u001B[32m"
    let Yellow = "\u001B[33m"
    let Blue =   "\u001B[34m"
    let Purple = "\u001B[35m"
    let Cyan =   "\u001B[36m"
    let White =  "\u001B[37m"

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
