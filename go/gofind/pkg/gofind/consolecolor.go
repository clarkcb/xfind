package gofind

type Color int

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
		} else {
			return ConsoleColorBlack
		}
	case ColorRed:
		if bold {
			return ConsoleBoldRed
		} else {
			return ConsoleColorRed
		}
	case ColorGreen:
		if bold {
			return ConsoleBoldGreen
		} else {
			return ConsoleColorGreen
		}
	case ColorYellow:
		if bold {
			return ConsoleBoldYellow
		} else {
			return ConsoleColorYellow
		}
	case ColorBlue:
		if bold {
			return ConsoleBoldBlue
		} else {
			return ConsoleColorBlue
		}
	case ColorMagenta:
		if bold {
			return ConsoleBoldMagenta
		} else {
			return ConsoleColorMagenta
		}
	case ColorCyan:
		if bold {
			return ConsoleBoldCyan
		} else {
			return ConsoleColorCyan
		}
	case ColorWhite:
		if bold {
			return ConsoleBoldWhite
		} else {
			return ConsoleColorWhite
		}
	}
	return ConsoleColorBlack
}

func ConsoleColorForColor(color Color) string {
	return ConsoleColorForColorBold(color, false)
}
