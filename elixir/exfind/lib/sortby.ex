defmodule ExFind.SortBy do
  @moduledoc """
  Documentation for `ExFind.SortBy`.
  """

  @sort_types [:file_path, :file_name, :file_size, :file_type, :last_mod]

  def get_sort_by_for_name(name) do
    sort_types = @sort_types
    name_atom = if is_atom(name), do: name, else: String.downcase(name) |> String.to_atom()
    cond do
      Enum.member?(sort_types, name_atom) -> name_atom
      name_atom == :name -> :file_name
      name_atom == :size -> :file_size
      name_atom == :type -> :file_type
      name_atom == :lastmod -> :last_mod
      true -> :file_path
    end
  end
end
