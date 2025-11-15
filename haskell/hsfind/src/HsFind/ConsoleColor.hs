module HsFind.ConsoleColor
  (
      colorReset
    , colorBlack
    , colorRed
    , colorGreen
    , colorYellow
    , colorBlue
    , colorMagenta
    , colorCyan
    , colorWhite
    , boldBlack
    , boldRed
    , boldGreen
    , boldYellow
    , boldBlue
    , boldMagenta
    , boldCyan
    , boldWhite
  ) where


colorReset :: String
colorReset = "\x1b[0m"

colorBlack :: String
colorBlack = "\x1b[0;30m"

colorRed :: String
colorRed = "\x1b[0;31m"

colorGreen :: String
colorGreen = "\x1b[0;32m"

colorYellow :: String
colorYellow = "\x1b[0;33m"

colorBlue :: String
colorBlue = "\x1b[0;34m"

colorMagenta :: String
colorMagenta = "\x1b[0;35m"

colorCyan :: String
colorCyan = "\x1b[0;36m"

colorWhite :: String
colorWhite = "\x1b[0;37m"

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
