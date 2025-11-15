# frozen_string_literal: true

################################################################################
#
# color.rb
#
# Add basic console color methods to String
#
# NOTE: for this to work as an add-on/mixin to String, it must not be in
#       a module
#
################################################################################

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

class String
  def black
    "\e[0;#{ConsoleColor::BLACK}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def bold_black
    "\e[1;#{ConsoleColor::BLACK}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def red
    "\e[0;#{ConsoleColor::RED}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def bold_red
    "\e[1;#{ConsoleColor::RED}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def green
    "\e[0;#{ConsoleColor::GREEN}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def bold_green
    "\e[1;#{ConsoleColor::GREEN}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def yellow
    "\e[0;#{ConsoleColor::YELLOW}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def bold_yellow
    "\e[1;#{ConsoleColor::YELLOW}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def blue
    "\e[0;#{ConsoleColor::BLUE}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def bold_blue
    "\e[1;#{ConsoleColor::BLUE}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def magenta
    "\e[0;#{ConsoleColor::MAGENTA}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def bold_magenta
    "\e[1;#{ConsoleColor::MAGENTA}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def cyan
    "\e[0;#{ConsoleColor::CYAN}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def bold_cyan
    "\e[1;#{ConsoleColor::CYAN}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def white
    "\e[0;#{ConsoleColor::WHITE}m#{self}\e[#{ConsoleColor::RESET}m"
  end

  def bold_white
    "\e[1;#{ConsoleColor::WHITE}m#{self}\e[#{ConsoleColor::RESET}m"
  end
end
