module HsFind.ConsoleColor
  ( Color(..)
  , colorToConsoleColor
  , consoleReset
  , consoleBlack
  , consoleRed
  , consoleGreen
  , consoleYellow
  , consoleBlue
  , consoleMagenta
  , consoleCyan
  , consoleWhite
  , boldBlack
  , boldRed
  , boldGreen
  , boldYellow
  , boldBlue
  , boldMagenta
  , boldCyan
  , boldWhite
  ) where

data Color = ColorBlack
           | ColorRed
           | ColorGreen
           | ColorYellow
           | ColorBlue
           | ColorMagenta
           | ColorCyan
           | ColorWhite
  deriving (Show, Eq)

consoleReset :: String
consoleReset = "\x1b[0m"

consoleBlack :: String
consoleBlack = "\x1b[0;30m"

consoleRed :: String
consoleRed = "\x1b[0;31m" 

consoleGreen :: String
consoleGreen = "\x1b[0;32m"

consoleYellow :: String
consoleYellow = "\x1b[0;33m"

consoleBlue :: String
consoleBlue = "\x1b[0;34m"

consoleMagenta :: String
consoleMagenta = "\x1b[0;35m"

consoleCyan :: String
consoleCyan = "\x1b[0;36m"

consoleWhite :: String
consoleWhite = "\x1b[0;37m"

boldBlack :: String
boldBlack = "\x1b[1;30m"

boldRed :: String
boldRed = "\x1b[1;31m"

boldGreen :: String
boldGreen = "\x1b[1;32m"

boldYellow :: String
boldYellow = "\x1b[1;33m"

boldBlue :: String
boldBlue = "\x1b[1;34m"

boldMagenta :: String
boldMagenta = "\x1b[1;35m"

boldCyan :: String
boldCyan = "\x1b[1;36m"

boldWhite :: String
boldWhite = "\x1b[1;37m"

colorToConsoleColor :: Color -> String
colorToConsoleColor color =
  case color of
    ColorBlack -> consoleBlack
    ColorRed -> consoleRed
    ColorGreen -> consoleGreen
    ColorYellow -> consoleYellow
    ColorBlue -> consoleBlue
    ColorMagenta -> consoleMagenta
    ColorCyan -> consoleCyan
    ColorWhite -> consoleWhite
