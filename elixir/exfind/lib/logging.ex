defmodule ExFind.Logging do
  @moduledoc """
  Documentation for `ExFind.Logging`.
  """

  alias ExFind.Color

  def log(message) do
    IO.puts(message)
  end

  def log_error(message, colorize \\ true) do
    if colorize do
      IO.puts(:stderr, Color.bold_red <> message <> Color.reset)
    else
      IO.puts(:stderr, message)
    end
  end
end
