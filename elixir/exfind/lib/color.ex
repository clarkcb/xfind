defmodule ExFind.Color do
  @moduledoc """
  Documentation for `ExFind.Color`.
  """
  alias ExFind.ConsoleColor

  @colors [:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white]

  def get_color_for_name(name) do
    colors = @colors
    name_atom = if is_atom(name), do: name, else: String.downcase(name) |> String.to_atom()
    cond do
      Enum.member?(colors, name_atom) -> name_atom
      true -> :black
    end
  end

  def get_console_color_for_color(color) do
    case color do
      :black -> ConsoleColor.black()
      :red -> ConsoleColor.red()
      :green -> ConsoleColor.green()
      :yellow -> ConsoleColor.yellow()
      :blue -> ConsoleColor.blue()
      :magenta -> ConsoleColor.magenta()
      :cyan -> ConsoleColor.cyan()
      :white -> ConsoleColor.white()
      _ -> ConsoleColor.black()
    end
  end
end
