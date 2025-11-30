use crate::consolecolor::{CONSOLE_BLACK, CONSOLE_RED, CONSOLE_GREEN, CONSOLE_YELLOW, CONSOLE_BLUE, CONSOLE_MAGENTA, CONSOLE_CYAN, CONSOLE_WHITE, };

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
}

pub fn color_from_name(name: &str) -> Color {
    match name.to_ascii_lowercase().as_str() {
        "black" => Color::Black,
        "red" => Color::Red,
        "green" => Color::Green,
        "yellow" => Color::Yellow,
        "blue" => Color::Blue,
        "magenta" => Color::Magenta,
        "cyan" => Color::Cyan,
        "white" => Color::White,
        _ => Color::Black,
    }
}

pub fn name_from_color(color: &Color) -> String {
    match color {
        Color::Black => String::from("black"),
        Color::Red => String::from("red"),
        Color::Green => String::from("green"),
        Color::Yellow => String::from("yellow"),
        Color::Blue => String::from("blue"),
        Color::Magenta => String::from("magenta"),
        Color::Cyan => String::from("cyan"),
        Color::White => String::from("white"),
    }
}

pub fn console_color_from_color(color: &Color) -> String {
    match color {
        Color::Black => String::from(CONSOLE_BLACK),
        Color::Red => String::from(CONSOLE_RED),
        Color::Green => String::from(CONSOLE_GREEN),
        Color::Yellow => String::from(CONSOLE_YELLOW),
        Color::Blue => String::from(CONSOLE_BLUE),
        Color::Magenta => String::from(CONSOLE_MAGENTA),
        Color::Cyan => String::from(CONSOLE_CYAN),
        Color::White => String::from(CONSOLE_WHITE),
    }
}
