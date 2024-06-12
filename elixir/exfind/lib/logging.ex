defmodule ExFind.Logging do
  @moduledoc """
  Documentation for `ExFind.Logging`.
  """

  def log(message) do
    IO.puts(message)
  end

  def log_error(message) do
    IO.puts(:stderr, message)
  end
end
