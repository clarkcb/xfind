defmodule ExFind.Color do
  @moduledoc """
  Documentation for `ExFind.Color`.

  Using this instead of IO.ANSI so we can control the exact escape sequences and match with other language versions
  """

  @color_reset   <<27, 91, 48, 109>>

  # Normal
  @color_black   <<27, 91, 48, 59, 51, 48, 109>>
  @color_red     <<27, 91, 48, 59, 51, 49, 109>>
  @color_green   <<27, 91, 48, 59, 51, 50, 109>>
  @color_yellow  <<27, 91, 48, 59, 51, 51, 109>>
  @color_blue    <<27, 91, 48, 59, 51, 52, 109>>
  @color_magenta <<27, 91, 48, 59, 51, 53, 109>>
  @color_cyan    <<27, 91, 48, 59, 51, 54, 109>>
  @color_white   <<27, 91, 48, 59, 51, 55, 109>>

  # Bold
  @bold_black    <<27, 91, 49, 59, 51, 48, 109>>
  @bold_red      <<27, 91, 49, 59, 51, 49, 109>>
  @bold_green    <<27, 91, 49, 59, 51, 50, 109>>
  @bold_yellow   <<27, 91, 49, 59, 51, 51, 109>>
  @bold_blue     <<27, 91, 49, 59, 51, 52, 109>>
  @bold_magenta  <<27, 91, 49, 59, 51, 53, 109>>
  @bold_cyan     <<27, 91, 49, 59, 51, 54, 109>>
  @bold_white    <<27, 91, 49, 59, 51, 55, 109>>

  def reset, do: @color_reset

  def black, do: @color_black

  def red, do: @color_red

  def green, do: @color_green

  def yellow, do: @color_yellow

  def blue, do: @color_blue

  def magenta, do: @color_magenta

  def cyan, do: @color_cyan

  def white, do: @color_white

  def bold_black, do: @bold_black

  def bold_red, do: @bold_red

  def bold_green, do: @bold_green

  def bold_yellow, do: @bold_yellow

  def bold_blue, do: @bold_blue

  def bold_magenta, do: @bold_magenta

  def bold_cyan, do: @bold_cyan

  def bold_white, do: @bold_white

end
