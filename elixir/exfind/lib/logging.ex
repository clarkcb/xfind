defmodule ExFind.Logging do
  @moduledoc """
  Documentation for `ExFind.Logging`.
  """

  alias ExFind.ConsoleColor

  def log(message) do
    IO.puts(message)
  end

  def log_error(message, colorize \\ true) do
    if colorize do
      IO.puts(:stderr, ConsoleColor.bold_red <> message <> ConsoleColor.reset)
    else
      IO.puts(:stderr, message)
    end
  end
end
