defmodule ExFind.FileType do
  @moduledoc """
  Documentation for `ExFind.FileType`.
  """

defstruct type_name: "", extensions: [], names: []

  def new(args), do: __struct__(args)
end

defmodule ExFind.FileTypesLoader do
  @moduledoc """
  Documentation for `ExFind.FileTypesLoader`.
  """

def load_file_types() do
    # Load the find options from the findoptions.json file.
    file_types_path = ExFind.Config.file_types_path
    IO.puts(file_types_path)
    {:ok, json} = File.read(file_types_path)
    file_types = JSON.decode!(json)
    IO.puts("\nfile_types: #{inspect(file_types)}")

    file_type_map = file_types["filetypes"]
                    |> Enum.map(fn t -> ExFind.FileType.new([type_name: String.to_atom(t["type"]), extensions: t["extensions"], names: t["names"]]) end)
                    |> Enum.map(fn t -> {t.type_name, t} end)
                    |> Map.new()
    IO.puts("\nfile_type_map: #{inspect(file_type_map)}")
    file_type_map
  end
end

defmodule ExFind.FileTypes do
  @moduledoc """
  Documentation for `ExFind.FileTypes`.
  """

  alias ExFind.FileUtil

  @file_types [:unknown, :archive, :audio, :binary, :code, :font, :image, :text, :video, :xml]

  defstruct file_types: MapSet.new(@file_types),
            file_types_maps: ExFind.FileTypesLoader.load_file_types()

  def new(), do: __struct__()

  def get_file_type_for_name(name) do
    file_types = @file_types
    name_atom = if is_atom(name), do: name, else: String.downcase(name) |> String.to_atom()
    if Enum.member?(file_types, name_atom), do: name_atom, else: :unknown
  end

  def get_file_type_for_name(name) when is_binary(name) do
    name_atom = String.downcase(name) |> String.to_atom()
    get_file_type_for_name(name_atom)
  end

  defp file_type_for_file_name?(file_types, file_type, file_name) do
    if Enum.member?(file_types.file_types_maps[file_type].names, file_name) do
      true
    else
      ext = FileUtil.get_extension(file_name)
      Enum.member?(file_types.file_types_maps[file_type].extensions, ext)
    end
  end

  def archive_file_name?(file_types, file_name) do
    file_type_for_file_name?(file_types, :archive, file_name)
  end

  def audio_file_name?(file_types, file_name) do
    file_type_for_file_name?(file_types, :audio, file_name)
  end

  def binary_file_name?(file_types, file_name) do
    file_type_for_file_name?(file_types, :binary, file_name)
  end

  def code_file_name?(file_types, file_name) do
    file_type_for_file_name?(file_types, :code, file_name)
  end

  def font_file_name?(file_types, file_name) do
    file_type_for_file_name?(file_types, :font, file_name)
  end

  def image_file_name?(file_types, file_name) do
    file_type_for_file_name?(file_types, :image, file_name)
  end

  def text_file_name?(file_types, file_name) do
    Enum.any?([:text, :code, :xml], fn t -> file_type_for_file_name?(file_types, t, file_name) end)
  end

  def video_file_name?(file_types, file_name) do
    file_type_for_file_name?(file_types, :video, file_name)
  end

  def xml_file_name?(file_types, file_name) do
    file_type_for_file_name?(file_types, :xml, file_name)
  end

  def get_file_type_for_file_name(file_types, file_name) do
    cond do
      # more specific types first
      code_file_name?(file_types, file_name) -> :code
      archive_file_name?(file_types, file_name) -> :archive
      audio_file_name?(file_types, file_name) -> :audio
      font_file_name?(file_types, file_name) -> :font
      image_file_name?(file_types, file_name) -> :image
      video_file_name?(file_types, file_name) -> :video
      # more general types last
      xml_file_name?(file_types, file_name) -> :xml
      text_file_name?(file_types, file_name) -> :text
      binary_file_name?(file_types, file_name) -> :binary
      true -> :unknown
    end
  end

  def get_file_type_for_path(file_types, file_path) do
    get_file_type_for_file_name(file_types, Path.basename(file_path))
  end
end
