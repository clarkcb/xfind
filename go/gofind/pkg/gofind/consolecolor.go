package gofind

import "strings"

type Color int

const (
	ColorBlackName   = "black"
	ColorRedName     = "red"
	ColorGreenName   = "green"
	ColorYellowName  = "yellow"
	ColorBlueName    = "blue"
	ColorMagentaName = "magenta"
	ColorCyanName    = "cyan"
	ColorWhiteName   = "white"
)

const (
	ColorBlack   Color = iota
	ColorRed     Color = iota
	ColorGreen   Color = iota
	ColorYellow  Color = iota
	ColorBlue    Color = iota
	ColorMagenta Color = iota
	ColorCyan    Color = iota
	ColorWhite   Color = iota
)

func ColorForName(name string) Color {
	lname := strings.ToLower(name)
	if lname == ColorBlackName {
		return ColorBlack
	}
	if lname == ColorRedName {
		return ColorRed
	}
	if lname == ColorGreenName {
		return ColorGreen
	}
	if lname == ColorYellowName {
		return ColorYellow
	}
	if lname == ColorBlueName {
		return ColorBlue
	}
	if lname == ColorMagentaName {
		return ColorMagenta
	}
	if lname == ColorCyanName {
		return ColorCyan
	}
	return ColorWhite
}

func NameForColor(color Color) string {
	if color == ColorBlack {
		return ColorBlackName
	}
	if color == ColorRed {
		return ColorRedName
	}
	if color == ColorGreen {
		return ColorGreenName
	}
	if color == ColorYellow {
		return ColorYellowName
	}
	if color == ColorBlue {
		return ColorBlueName
	}
	if color == ColorMagenta {
		return ColorMagentaName
	}
	if color == ColorCyan {
		return ColorCyanName
	}
	return ColorWhiteName
}

const (
	ConsoleColorReset   = "\033[0m"
	ConsoleColorBlack   = "\033[0;30m"
	ConsoleColorRed     = "\033[0;31m"
	ConsoleColorGreen   = "\033[0;32m"
	ConsoleColorYellow  = "\033[0;33m"
	ConsoleColorBlue    = "\033[0;34m"
	ConsoleColorMagenta = "\033[0;35m"
	ConsoleColorCyan    = "\033[0;36m"
	ConsoleColorWhite   = "\033[0;37m"

	ConsoleBoldBlack   = "\033[1;30m"
	ConsoleBoldRed     = "\033[1;31m"
	ConsoleBoldGreen   = "\033[1;32m"
	ConsoleBoldYellow  = "\033[1;33m"
	ConsoleBoldBlue    = "\033[1;34m"
	ConsoleBoldMagenta = "\033[1;35m"
	ConsoleBoldCyan    = "\033[1;36m"
	ConsoleBoldWhite   = "\033[1;37m"
)

func ConsoleColorForColorBold(color Color, bold bool) string {
	switch color {
	case ColorBlack:
		if bold {
			return ConsoleBoldBlack
		}
		return ConsoleColorBlack
	case ColorRed:
		if bold {
			return ConsoleBoldRed
		}
		return ConsoleColorRed
	case ColorGreen:
		if bold {
			return ConsoleBoldGreen
		}
		return ConsoleColorGreen
	case ColorYellow:
		if bold {
			return ConsoleBoldYellow
		}
		return ConsoleColorYellow
	case ColorBlue:
		if bold {
			return ConsoleBoldBlue
		}
		return ConsoleColorBlue
	case ColorMagenta:
		if bold {
			return ConsoleBoldMagenta
		}
		return ConsoleColorMagenta
	case ColorCyan:
		if bold {
			return ConsoleBoldCyan
		}
		return ConsoleColorCyan
	case ColorWhite:
		if bold {
			return ConsoleBoldWhite
		}
		return ConsoleColorWhite
	}
	return ConsoleColorBlack
}

func ConsoleColorForColor(color Color) string {
	return ConsoleColorForColorBold(color, false)
}
