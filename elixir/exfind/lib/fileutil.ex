defmodule ExFind.FileUtil do
  @moduledoc """
  Documentation for `ExFind.FileUtil`.
  """

@dot_dirs [".", ".."]

def dot_dir?(d) do
    Path.split(d) |> Enum.count() == 1
    and Enum.member?(@dot_dirs, Path.basename(d))
  end

  def hidden?(file_path) do
    Path.split(file_path)
    |> Enum.filter(fn p -> !dot_dir?(p) end)
    |> Enum.any?(fn p -> String.starts_with?(p, ".") end)
  end

  def get_extension(file_path) do
    case Path.extname(Path.basename(file_path)) do
      "" -> ""
      ext -> String.slice(ext, 1..-1//1)
    end
  end
end
