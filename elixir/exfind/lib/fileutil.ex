defmodule ExFind.FileUtil do
  @moduledoc """
  Documentation for `ExFind.FileUtil`.
  """

  @dot_dirs [".", ".."]

  def dot_dir?(d) do
    Path.split(d) |> Enum.count() == 1
    and Enum.member?(@dot_dirs, Path.basename(d))
  end

  def expand_path(file_path) do
    cond do
      file_path == "" || !String.starts_with?(file_path, "~") -> file_path
      file_path == "~" || String.starts_with?(file_path, "~/") -> Path.expand(file_path)
      true -> Path.join([Path.dirname(System.user_home()), String.slice(file_path, 1..-1//1)])
    end
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

  def symlink?(file_path) do
    case File.read_link(file_path) do
      {:ok, _} -> true
      _ -> false
    end
  end

  def get_symlink_target(file_path) do
    case File.read_link(file_path) do
      {:ok, target} -> target
      _ -> ""
    end
  end

end
