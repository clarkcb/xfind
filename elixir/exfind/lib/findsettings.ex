defmodule ExFind.FindSettings do
  @moduledoc """
  Documentation for `ExFind.FindSettings`.
  """

  alias ExFind.FindError

  defstruct archives_only: false,
            colorize: true,
            debug: false,
            follow_symlinks: false,
            in_archive_extensions: [],
            in_archive_file_patterns: [],
            in_dir_patterns: [],
            in_extensions: [],
            in_file_patterns: [],
            in_file_types: [],
            include_archives: false,
            include_hidden: false,
            max_depth: -1,
            max_last_mod: nil,
            max_size: 0,
            min_depth: -1,
            min_last_mod: nil,
            min_size: 0,
            out_archive_extensions: [],
            out_archive_file_patterns: [],
            out_dir_patterns: [],
            out_extensions: [],
            out_file_patterns: [],
            out_file_types: [],
            paths: [],
            print_dirs: false,
            print_files: false,
            print_usage: false,
            print_version: false,
            recursive: true,
            sort_by: :file_path,
            sort_case_insensitive: false,
            sort_descending: false,
            verbose: false

  def new(), do: __struct__()
  def new(args), do: __struct__(args)

  def add_extensions(settings, extensions, extensions_name) when is_list(extensions) do
    # Treat settings struct as a map to check for and update field
    if Map.has_key?(settings, extensions_name) and is_list(Map.get(settings, extensions_name)) do
      new_exts = Enum.map(extensions, fn e -> String.split(e, ",") end) |> List.flatten()
      ext_list = Map.get(settings, extensions_name)
      Map.put(settings, extensions_name, ext_list ++ new_exts)
    else
      settings
    end
  end

  def add_extensions(settings, extensions, extensions_name) when is_binary(extensions) do
    add_extensions(settings, [extensions], extensions_name)
  end

  def add_extension(settings, extension, extensions_name) do
    add_extensions(settings, [extension], extensions_name)
  end

  defp add_regexes(settings, regexes, patterns_name) do
    # Treat settings struct as a map to check for and update field
    if Map.has_key?(settings, patterns_name) and is_list(Map.get(settings, patterns_name)) do
      patterns_list = Map.get(settings, patterns_name)
      Map.put(settings, patterns_name, patterns_list ++ regexes)
    else
      settings
    end
  end

  defp regexes_and_errors_for_patterns(patterns, regexes, errors) do
    case patterns do
      [] -> {regexes, errors}
      [p | tail] ->
        case Regex.compile(p) do
          {:ok, r} -> regexes_and_errors_for_patterns(tail, regexes ++ [r], errors)
          {:error, e} -> regexes_and_errors_for_patterns(tail, regexes, errors ++ ["/#{p}/: #{inspect(e)}"])
        end
    end
  end

  def add_patterns(settings, patterns, patterns_name) do
    {regexes, errors} = regexes_and_errors_for_patterns(patterns, [], [])
    case {regexes, errors} do
      {_, []} -> add_regexes(settings, regexes, patterns_name)
      {_, [e | _tail]} -> raise FindError, message: "Invalid pattern: #{e}"
    end
  end

  def add_pattern(settings, pattern, patterns_name) do
    if is_list(pattern) do
      add_patterns(settings, pattern, patterns_name)
    else
      add_patterns(settings, [pattern], patterns_name)
    end
  end

  def set_archives_only(settings, archives_only) do
    include_archives = archives_only || settings.include_archives
    %{settings | archives_only: archives_only, include_archives: include_archives}
  end

  def set_debug(settings, debug) do
    verbose = debug || settings.verbose
    %{settings | debug: debug, verbose: verbose}
  end

  def need_last_mod?(settings) do
    settings.sort_by == :last_mod
    or settings.min_last_mod != nil
    or settings.max_last_mod != nil
  end

  def need_size?(settings) do
    settings.sort_by == :file_size
    or settings.min_size > 0
    or settings.max_size > 0
  end
end
