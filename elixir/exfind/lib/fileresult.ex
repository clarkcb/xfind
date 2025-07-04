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
    container_str <> Path.join(file_result.path, file_result.name)
  end
end

defmodule ExFind.FileResultFormatter do
  @moduledoc """
  Documentation for `ExFind.FileResultFormatter`.
  """

  alias ExFind.StringUtil

  defstruct [:settings, :fn_format_path, :fn_format_file_name]

  def new(settings) do
    fn_format_path = if settings.colorize and !Enum.empty?(settings.in_dir_patterns) do
      fn(settings, path) -> format_path_with_color(settings, path) end
    else
      fn(_settings, path) -> path end
    end
    fn_format_file_name = if settings.colorize and (!Enum.empty?(settings.in_extensions) or !Enum.empty?(settings.in_file_patterns)) do
      fn settings, file_name -> format_file_name_with_color(settings, file_name) end
    else
      fn _settings, file_name -> file_name end
    end
    __struct__([settings: settings, fn_format_path: fn_format_path, fn_format_file_name: fn_format_file_name])
  end

  def colorize(s, start_idx, end_idx) do
    prefix = if start_idx > 0 do
      String.slice(s, 0, start_idx)
    else
      ""
    end
    suffix = if end_idx < String.length(s) do
      String.slice(s, end_idx, String.length(s))
    else
      ""
    end
    match_text = String.slice(s, start_idx, end_idx - start_idx)
    colorized = IO.ANSI.green() <> match_text <> IO.ANSI.reset()
    prefix <> colorized <> suffix
  end

  def format_path_with_color(settings, path) do
    matching_dir_pattern = Enum.find(settings.in_dir_patterns, false, fn p -> Regex.match?(p, path) end)
    if matching_dir_pattern do
      {match_idx, match_length} = Regex.run(matching_dir_pattern, path, return: :index) |> Enum.at(0)
      colorize(path, match_idx, match_idx + match_length)
    else
      path
    end
  end

  def format_path(formatter, path) do
    formatter.fn_format_path.(formatter.settings, path)
  end

  def format_file_name_with_color(settings, file_name) do
    matching_file_pattern = Enum.find(settings.in_file_patterns, false, fn p -> Regex.match?(p, file_name) end)
    formatted_file_name =
      if matching_file_pattern do
        {match_idx, match_length} = Regex.run(matching_file_pattern, file_name, return: :index) |> Enum.at(0)
        colorize(file_name, match_idx, match_idx + match_length)
      else
        file_name
      end
    if Enum.empty?(settings.in_extensions) do
      formatted_file_name
    else
      idx = StringUtil.last_index_of(formatted_file_name, "\.")
      if idx < 1 || idx >= String.length(formatted_file_name) do
        formatted_file_name
      else
        colorize(formatted_file_name, idx + 1, String.length(formatted_file_name))
      end
    end
  end

  def format_file_name(formatter, file_name) do
    formatter.fn_format_file_name.(formatter.settings, file_name)
  end

  def format_file_result(formatter, file_result) do
    parent = formatter.fn_format_path.(formatter.settings, file_result.path)
    file_name = formatter.fn_format_file_name.(formatter.settings, file_result.name)
    Path.join(parent, file_name)
  end
end

defmodule ExFind.FileResultSorter do
  @moduledoc """
  Documentation for `ExFind.FileResultSorter`.
  """

  def get_file_path_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {String.downcase(r.path), String.downcase(r.name)} end
    else
      fn r -> {r.path, r.name} end
    end
  end

  def get_file_name_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {String.downcase(r.name), String.downcase(r.path)} end
    else
      fn r -> {r.name, r.path} end
    end
  end

  def get_file_size_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {r.file_size, String.downcase(r.path), String.downcase(r.name)} end
    else
      fn r -> {r.file_size, r.path, r.name} end
    end
  end

  def get_file_type_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {r.file_type, String.downcase(r.path), String.downcase(r.name)} end
    else
      fn r -> {r.file_type, r.path, r.name} end
    end
  end

  def get_last_mod_mapper(settings) do
    if settings.sort_case_insensitive do
      fn r -> {r.last_mod, String.downcase(r.path), String.downcase(r.name)} end
    else
      fn r -> {r.last_mod, r.path, r.name} end
    end
  end

  def get_file_result_mapper(settings) do
    case settings.sort_by do
      :file_name -> get_file_name_mapper(settings)
      :file_size -> get_file_size_mapper(settings)
      :file_type -> get_file_type_mapper(settings)
      :last_mod  -> get_last_mod_mapper(settings)
      _          -> get_file_path_mapper(settings)
    end
  end

  def sort(settings, results) do
    file_result_mapper = get_file_result_mapper(settings)
    direction = if settings.sort_descending, do: :desc, else: :asc
    Enum.sort_by(results, file_result_mapper, direction)
  end
end
