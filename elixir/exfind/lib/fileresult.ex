defmodule ExFind.FileResult do
  @moduledoc """
  Documentation for `ExFind.FileResult`.
  """
  @container_separator "!"

  def container_separator, do: @container_separator

  defstruct containers: [], path: "", name: "", file_type: :unknown, file_size: 0, last_mod: 0

  def new(path, file_name, file_type, file_size, last_mod) do
    __struct__([path: path, name: file_name, file_type: file_type, file_size: file_size, last_mod: last_mod])
  end

  def new(containers, path, file_name, file_type, file_size, last_mod) do
    __struct__([containers: containers, path: path, name: file_name, file_type: file_type, file_size: file_size, last_mod: last_mod])
  end

  def new(args), do: __struct__(args)

  def relative_path(file_result) do
    Path.join(file_result.path, file_result.name)
  end

  def to_string(file_result) do
    container_str = if Enum.empty?(file_result.containers), do: "", else: Enum.join(file_result.containers, @container_separator) <> @container_separator
    file_path = container_str <> Path.join(file_result.path, file_result.name)
    file_path
  end
end
