# frozen_string_literal: true

################################################################################
#
# console_color.rb
#
# Add basic console color methods to String
#
# NOTE: for this to work as an add-on/mixin to String, it must not be in
#       a module
#
################################################################################

module RbFind
  module ConsoleColor
    RESET = 0
    BLACK = 30
    RED = 31
    GREEN = 32
    YELLOW = 33
    BLUE = 34
    MAGENTA = 35
    CYAN = 36
    WHITE = 37
  end
end

class String
  def black
    "\e[0;#{RbFind::ConsoleColor::BLACK}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def bold_black
    "\e[1;#{RbFind::ConsoleColor::BLACK}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def red
    "\e[0;#{RbFind::ConsoleColor::RED}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def bold_red
    "\e[1;#{RbFind::ConsoleColor::RED}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def green
    "\e[0;#{RbFind::ConsoleColor::GREEN}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def bold_green
    "\e[1;#{RbFind::ConsoleColor::GREEN}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def yellow
    "\e[0;#{RbFind::ConsoleColor::YELLOW}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def bold_yellow
    "\e[1;#{RbFind::ConsoleColor::YELLOW}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def blue
    "\e[0;#{RbFind::ConsoleColor::BLUE}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def bold_blue
    "\e[1;#{RbFind::ConsoleColor::BLUE}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def magenta
    "\e[0;#{RbFind::ConsoleColor::MAGENTA}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def bold_magenta
    "\e[1;#{RbFind::ConsoleColor::MAGENTA}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def cyan
    "\e[0;#{RbFind::ConsoleColor::CYAN}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def bold_cyan
    "\e[1;#{ConsoleColor::CYAN}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def white
    "\e[0;#{RbFind::ConsoleColor::WHITE}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def bold_white
    "\e[1;#{RbFind::ConsoleColor::WHITE}m#{self}\e[#{RbFind::ConsoleColor::RESET}m"
  end

  def in_color(console_color)
    case console_color
    in RbFind::ConsoleColor::BLACK
      black
    in RbFind::ConsoleColor::RED
      red
    in RbFind::ConsoleColor::GREEN
      green
    in RbFind::ConsoleColor::YELLOW
      yellow
    in RbFind::ConsoleColor::BLUE
      blue
    in RbFind::ConsoleColor::MAGENTA
      magenta
    in RbFind::ConsoleColor::CYAN
      cyan
    in RbFind::ConsoleColor::WHITE
      white
    end
  end
end
