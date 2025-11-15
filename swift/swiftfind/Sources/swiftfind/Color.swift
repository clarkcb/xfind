//
//  Color.swift
//  swiftfind
//
//  Created by Cary Clark on 5/10/25.
//  Copyright (c) 2025 Cary Clark. All rights reserved.
//

import Foundation

public enum Color {
    public static let RESET = "\u{001B}[0m"
    public static let BLACK = "\u{001B}[0;30m"
    public static let RED = "\u{001B}[0;31m"
    public static let GREEN = "\u{001B}[0;32m"
    public static let YELLOW = "\u{001B}[0;33m"
    public static let BLUE = "\u{001B}[0;34m"
    public static let MAGENTA = "\u{001B}[0;35m"
    public static let CYAN = "\u{001B}[0;36m"
    public static let WHITE = "\u{001B}[0;37m"

    public static let BOLD_BLACK = "\u{001B}[1;30m"
    public static let BOLD_RED = "\u{001B}[1;31m"
    public static let BOLD_GREEN = "\u{001B}[1;32m"
    public static let BOLD_YELLOW = "\u{001B}[1;33m"
    public static let BOLD_BLUE = "\u{001B}[1;34m"
    public static let BOLD_MAGENTA = "\u{001B}[1;35m"
    public static let BOLD_CYAN = "\u{001B}[1;36m"
    public static let BOLD_WHITE = "\u{001B}[1;37m"
}
