defmodule ExFind.FileType do
  @moduledoc """
  Documentation for `ExFind.FileType`.
  """

  defstruct type_name: "", extensions: [], names: []

  def new(args), do: __struct__(args)
end

defmodule ExFind.FileTypes do
  @moduledoc """
  Documentation for `ExFind.FileTypes`.
  """

  alias ExFind.FileUtil

  @file_types [:unknown, :archive, :audio, :binary, :code, :font, :image, :text, :video, :xml]

  defstruct [:conn, :file_types]

  def new() do
    {:ok, conn} = Exqlite.Sqlite3.open(ExFind.Config.xfind_db_path, [:readonly])
    :ets.new(:file_ext_cache, [:set, :public, :named_table])
    __struct__([conn: conn, file_types: @file_types])
  end

  def get_file_type_for_name(name) do
    file_types = @file_types
    name_atom = if is_atom(name), do: name, else: String.downcase(name) |> String.to_atom()
    if Enum.member?(file_types, name_atom), do: name_atom, else: :unknown
  end

  def get_file_type_for_query_and_elem(file_types, query, elem) do
    {:ok, statement} = Exqlite.Sqlite3.prepare(file_types.conn, query)
    :ok = Exqlite.Sqlite3.bind(file_types.conn, statement, [elem])
    file_type = case Exqlite.Sqlite3.step(file_types.conn, statement) do
      {:row, [file_type_id]} -> Enum.at(file_types.file_types, file_type_id - 1)
      :done -> :unknown
    end
    :ok = Exqlite.Sqlite3.release(file_types.conn, statement)
    file_type
  end

  def get_db_file_type_for_file_name(file_types, file_name) do
    query = "SELECT file_type_id FROM file_name WHERE name = ?1;"
    file_type = get_file_type_for_query_and_elem(file_types, query, file_name)
    file_type
  end

  def get_db_file_type_for_extension(file_types, file_ext) do
    case :ets.lookup(:file_ext_cache, file_ext) do
      [{_, file_type}] ->
        file_type
      [] ->
        query = "SELECT file_type_id FROM file_extension WHERE extension = ?1;"
        file_type = get_file_type_for_query_and_elem(file_types, query, file_ext)
        :ets.insert(:file_ext_cache, {file_ext, file_type})
        file_type
      end
  end

  def get_file_type_for_file_name(file_types, file_name) do
    case get_db_file_type_for_file_name(file_types, file_name) do
      :unknown -> get_db_file_type_for_extension(file_types, FileUtil.get_extension(file_name))
      file_type -> file_type
    end
  end

  def get_file_type_for_path(file_types, file_path) do
    get_file_type_for_file_name(file_types, Path.basename(file_path))
  end

  def archive_file_name?(file_types, file_name) do
    get_file_type_for_file_name(file_types, file_name) == :archive
  end

  def audio_file_name?(file_types, file_name) do
    get_file_type_for_file_name(file_types, file_name) == :audio
  end

  def binary_file_name?(file_types, file_name) do
    get_file_type_for_file_name(file_types, file_name) == :binary
  end

  def code_file_name?(file_types, file_name) do
    get_file_type_for_file_name(file_types, file_name) == :code
  end

  def font_file_name?(file_types, file_name) do
    get_file_type_for_file_name(file_types, file_name) == :font
  end

  def image_file_name?(file_types, file_name) do
    get_file_type_for_file_name(file_types, file_name) == :image
  end

  def text_file_name?(file_types, file_name) do
    file_type = get_file_type_for_file_name(file_types, file_name)
    Enum.any?([:text, :code, :xml], fn t -> t == file_type end)
  end

  def video_file_name?(file_types, file_name) do
    get_file_type_for_file_name(file_types, file_name) == :video
  end

  def xml_file_name?(file_types, file_name) do
    get_file_type_for_file_name(file_types, file_name) == :xml
  end
end
