# frozen_string_literal: true

################################################################################
#
# color.rb
#
# Generic color support
#
################################################################################

module RbFind
  module Color
    BLACK = 0
    RED = 1
    GREEN = 2
    YELLOW = 3
    BLUE = 4
    MAGENTA = 5
    CYAN = 6
    WHITE = 7

    NAMES = Array[:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white].freeze

    module_function

    def from_name(name)
      from_sym(name.downcase.to_sym)
    end

    def from_sym(sym)
      idx = NAMES.index(sym)
      idx.nil? ? 0 : NAMES[idx]
    end

    def to_console_color(color)
      case color
      in Color::BLACK
        ConsoleColor::BLACK
      in Color::RED
        ConsoleColor::RED
      in Color::GREEN
        ConsoleColor::GREEN
      in Color::YELLOW
        ConsoleColor::YELLOW
      in Color::BLUE
        ConsoleColor::BLUE
      in Color::MAGENTA
        ConsoleColor::MAGENTA
      in Color::CYAN
        ConsoleColor::CYAN
      in Color::WHITE
        ConsoleColor::WHITE
      end
    end
  end
end

class String
  def console_color(color)
    case color
    in RbFind::Color::BLACK
      black
    in RbFind::Color::RED
      red
    in RbFind::Color::GREEN
      green
    in RbFind::Color::YELLOW
      yellow
    in RbFind::Color::BLUE
      blue
    in RbFind::Color::MAGENTA
      magenta
    in RbFind::Color::CYAN
      cyan
    in RbFind::Color::WHITE
      white
    end
  end
end
