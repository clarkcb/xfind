enum Color { black, red, green, yellow, blue, magenta, cyan, white }

class ConsoleColor {
  static const String reset = '\u001B[0m';
  static const String black = '\u001B[0;30m';
  static const String red = '\u001B[0;31m';
  static const String green = '\u001B[0;32m';
  static const String yellow = '\u001B[0;33m';
  static const String blue = '\u001B[0;34m';
  static const String magenta = '\u001B[0;35m';
  static const String cyan = '\u001B[0;36m';
  static const String white = '\u001B[0;37m';

  static const String boldBlack = '\u001B[1;30m';
  static const String boldRed = '\u001B[1;31m';
  static const String boldGreen = '\u001B[1;32m';
  static const String boldYellow = '\u001B[1;33m';
  static const String boldBlue = '\u001B[1;34m';
  static const String boldMagenta = '\u001B[1;35m';
  static const String boldCyan = '\u001B[1;36m';
  static const String boldWhite = '\u001B[1;37m';

  static String fromColor(Color color) {
    switch (color) {
      case Color.black:
        return black;
      case Color.red:
        return red;
      case Color.green:
        return green;
      case Color.yellow:
        return yellow;
      case Color.blue:
        return blue;
      case Color.magenta:
        return magenta;
      case Color.cyan:
        return cyan;
      case Color.white:
        return white;
    }
  }
}
