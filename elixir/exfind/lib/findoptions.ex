defmodule ExFind.FindOption do
  @moduledoc """
  Documentation for `ExFind.FindOption`.
  """

  defstruct short_arg: "", long_arg: "", description: ""

  def new(args), do: __struct__(args)

  def sort_arg(option) do
    if option.short_arg == "" do
      option.long_arg
    else
      "#{String.downcase(option.short_arg)}a#{option.long_arg}"
    end
  end

  def to_arg_string(option) do
    if option.short_arg == "" do
      "--#{option.long_arg}"
    else
      "-#{option.short_arg},--#{option.long_arg}"
    end
  end
end

defmodule ExFind.FindOptionsLoader do
  @moduledoc """
  Documentation for `ExFind.FindOptionsLoader`.
  """

  def load_options() do
    # Load the find options from the findoptions.json file.
    find_options_path = ExFind.Config.find_options_path
    # IO.puts(find_options_path)
    {:ok, json} = File.read(find_options_path)
    find_options = JSON.decode!(json)
    # IO.inspect(find_options)
    find_options["findoptions"]
    |> Enum.map(fn o -> ExFind.FindOption.new([short_arg: Map.get(o, "short", ""), long_arg: o["long"], description: o["desc"]]) end)
    |> Enum.sort(fn o1, o2 -> ExFind.FindOption.sort_arg(o1) <= ExFind.FindOption.sort_arg(o2) end)
  end
end

defmodule ExFind.FindOptions do
  @moduledoc """
  Documentation for `ExFind.FindOptions`.
  """

  alias ExFind.FileTypes
  alias ExFind.FileUtil
  alias ExFind.FindError
  alias ExFind.FindSettings
  alias ExFind.SortBy
  alias ExFind.StringUtil

  require OptionParser

  defstruct options: ExFind.FindOptionsLoader.load_options()

  def new(), do: __struct__()

  defp bool_arg_action_map() do
    %{
      archivesonly: fn b, settings -> FindSettings.set_archives_only(settings, b) end,
      debug: fn b, settings -> FindSettings.set_debug(settings, b) end,
      excludearchives: fn b, settings -> %{settings | include_archives: not b} end,
      excludehidden: fn b, settings -> %{settings | include_hidden: not b} end,
      followsymlinks: fn b, settings -> %{settings | follow_symlinks: b} end,
      help: fn b, settings -> %{settings | print_usage: b} end,
      includearchives: fn b, settings -> %{settings | include_archives: b} end,
      includehidden: fn b, settings -> %{settings | include_hidden: b} end,
      nofollowsymlinks: fn b, settings -> %{settings | follow_symlinks: not b} end,
      noprintdirs: fn b, settings -> %{settings | print_dirs: not b} end,
      noprintfiles: fn b, settings -> %{settings | print_files: not b} end,
      noprintusage: fn b, settings -> %{settings | print_usage: not b} end,
      norecursive: fn b, settings -> %{settings | recursive: not b} end,
      printdirs: fn b, settings -> %{settings | print_dirs: b} end,
      printfiles: fn b, settings -> %{settings | print_files: b} end,
      printusage: fn b, settings -> %{settings | print_usage: b} end,
      printversion: fn b, settings -> %{settings | print_version: b} end,
      recursive: fn b, settings -> %{settings | recursive: b} end,
      sort_ascending: fn b, settings -> %{settings | sort_descending: not b} end,
      sort_caseinsensitive: fn b, settings -> %{settings | sort_case_insensitive: b} end,
      sort_casesensitive: fn b, settings -> %{settings | sort_case_insensitive: not b} end,
      sort_descending: fn b, settings -> %{settings | sort_descending: b} end,
      verbose: fn b, settings -> %{settings | verbose: b} end,
      version: fn b, settings -> %{settings | print_version: b} end
    }
  end

  defp int_arg_action_map() do
    %{
      maxdepth: fn i, settings -> %{settings | max_depth: i} end,
      maxsize: fn i, settings -> %{settings | max_size: i} end,
      mindepth: fn i, settings -> %{settings | min_depth: i} end,
      minsize: fn i, settings -> %{settings | min_size: i} end
    }
  end

  defp str_arg_action_map() do
    %{
      in_archiveext: fn s, settings -> FindSettings.add_extensions(settings, s, :in_archive_extensions) end,
      in_archivefilepattern: fn s, settings -> FindSettings.add_pattern(settings, s, :in_archive_file_patterns) end,
      in_dirpattern: fn s, settings -> FindSettings.add_pattern(settings, s, :in_dir_patterns) end,
      in_ext: fn s, settings -> FindSettings.add_extensions(settings, s, :in_extensions) end,
      in_filepattern: fn s, settings -> FindSettings.add_pattern(settings, s, :in_file_patterns) end,
      in_filetype: fn s, settings -> %{settings | in_file_types: settings.in_file_types ++ [FileTypes.get_file_type_for_name(s)]} end,
      maxlastmod: fn s, settings -> %{settings | max_last_mod: StringUtil.to_datetime(s)} end,
      minlastmod: fn s, settings -> %{settings | min_last_mod: StringUtil.to_datetime(s)} end,
      out_archiveext: fn s, settings -> FindSettings.add_extensions(settings, s, :out_archive_extensions) end,
      out_archivefilepattern: fn s, settings -> FindSettings.add_pattern(settings, s, :out_archive_file_patterns) end,
      out_dirpattern: fn s, settings -> FindSettings.add_pattern(settings, s, :out_dir_patterns) end,
      out_ext: fn s, settings -> FindSettings.add_extensions(settings, s, :out_extensions) end,
      out_filepattern: fn s, settings -> FindSettings.add_pattern(settings, s, :out_file_patterns) end,
      out_filetype: fn s, settings -> %{settings | out_file_types: settings.out_file_types ++ [FileTypes.get_file_type_for_name(s)]} end,
      path: fn s, settings -> %{settings | paths: settings.paths ++ [s]} end,
      sort_by: fn s, settings -> %{settings | sort_by: SortBy.get_sort_by_for_name(s)} end
    }
  end

  defp arg_action_maps() do
    {bool_arg_action_map(), int_arg_action_map(), str_arg_action_map()}
  end

  def parse_args(args, options, arg_action_maps) do
    {bool_arg_action_map, int_arg_action_map, str_arg_action_map} = arg_action_maps
    bool_opts = Map.keys(bool_arg_action_map) |> Enum.map(fn k -> {k, :boolean} end)
    int_opts = Map.keys(int_arg_action_map) |> Enum.map(fn k -> {k, :integer} end)
    # str_opts = Map.keys(str_arg_action_map) |> Enum.map(fn k -> {k, :string} end)
    # :keep allows for duplicates, assumes :string type
    str_opts = Map.keys(str_arg_action_map) ++ [:settings_file] |> Enum.map(fn k -> {k, :keep} end)
    parser_opts = bool_opts ++ int_opts ++ str_opts
    alias_opts = options
                 |> Enum.filter(fn o -> o.short_arg != "" end)
                 |> Enum.map(fn o -> {String.to_atom(o.short_arg), String.to_atom(String.replace(o.long_arg, "-", "_"))} end)
    # IO.puts("\nparser_opts:")
    # IO.inspect(parser_opts)
    # IO.puts("\nalias_opts:")
    # IO.inspect(alias_opts)
    OptionParser.parse(args, strict: parser_opts, aliases: alias_opts)
  end

  def update_settings_from_args(settings, args, arg_action_maps) do
    # IO.puts("\nupdate_settings_from_args()")
    # IO.puts("settings=#{inspect(settings)}")
    # IO.puts("args=#{inspect(args)}")
    {bool_arg_action_map, int_arg_action_map, str_arg_action_map} = arg_action_maps
    case args do
      [] -> settings
      [{k, v} | rest] ->
        # IO.puts("k: #{inspect(k)}")
        # IO.puts("v: #{inspect(v)}")
        cond do
          Map.has_key?(bool_arg_action_map, k) -> update_settings_from_args(Map.get(bool_arg_action_map, k).(v, settings), rest, arg_action_maps)
          Map.has_key?(int_arg_action_map, k) -> update_settings_from_args(Map.get(int_arg_action_map, k).(v, settings), rest, arg_action_maps)
          Map.has_key?(str_arg_action_map, k) -> update_settings_from_args(Map.get(str_arg_action_map, k).(v, settings), rest, arg_action_maps)
          k == :settings_file -> case update_settings_from_file(settings, v) do
            {:ok, new_settings} -> update_settings_from_args(new_settings, rest, arg_action_maps)
            {:error, _} -> update_settings_from_args(settings, rest, arg_action_maps)
          end
          true -> update_settings_from_args(settings, rest, arg_action_maps)
        end
    end
  end

  def get_settings_from_args(args, options) do
    # IO.puts("\nget_settings_from_args()")
    # IO.puts("args=#{inspect(args)}")
    settings = FindSettings.new([print_files: true])
    # IO.puts("\nargs:")
    # IO.inspect(args)
    arg_action_maps = arg_action_maps()
    {parsed_args, paths, invalid} = parse_args(args, options, arg_action_maps)
    # IO.puts("\nparsed_args:")
    # IO.inspect(parsed_args)
    # IO.puts("\npaths:")
    # IO.inspect(paths)
    # IO.puts("\ninvalid:")
    # IO.inspect(invalid)
    parsed_args_with_paths = parsed_args ++ Enum.map(paths, fn p -> {:path, p} end)
    # IO.puts("parsed_args_with_paths=#{inspect(parsed_args_with_paths)}")

    case invalid do
      [{opt, nil} | _rest] -> {:error, "Invalid option: #{opt}"}
      [] -> {:ok, update_settings_from_args(settings, parsed_args_with_paths, arg_action_maps)}
    end
  end

  def get_settings_from_args!(args, options) do
    case get_settings_from_args(args, options) do
      {:error, message} -> raise FindError, message: message
      {:ok, settings} -> settings
    end
  end

  defp flatten_keyword_list(keyword_list, flattened) do
    case keyword_list do
      [] -> flattened
      [{k, v} | rest] ->
        case v do
          [] -> flatten_keyword_list(rest, flattened)
          [v1 | vs] -> flatten_keyword_list(rest ++ [{k, vs}], flattened ++ [{k, v1}])
          _ -> flatten_keyword_list(rest, flattened ++ [{k, v}])
        end
    end
  end

  defp convert_map_to_keyword_list(map) do
    # IO.puts("\nconvert_map_to_keyword_list()")
    # IO.puts("map=#{inspect(map)}")
    keyword_list = Enum.map(map, fn {k, v} -> {StringUtil.atomize(k), v} end)
    # IO.puts("keyword_list=#{inspect(keyword_list)}")
    flattened = flatten_keyword_list(keyword_list, [])
    # IO.puts("flattened=#{inspect(flattened)}")
    flattened
  end

  def update_settings_from_json(settings, json) do
    # IO.puts("\nupdate_settings_from_json()")
    # IO.puts("settings=#{inspect(settings)}")
    # IO.puts("json=#{inspect(json)}")
    case JSON.decode(json) do
      {:ok, parsed_json} ->
        json_keyword_list = convert_map_to_keyword_list(parsed_json)
        {:ok, update_settings_from_args(settings, json_keyword_list, arg_action_maps())}
      {:error, e} -> {:error, e}
    end
  end

  def get_settings_from_json(json) do
    update_settings_from_json(FindSettings.new(), json)
  end

  def get_settings_from_json!(json) do
    case get_settings_from_json(json) do
      {:error, message} -> raise FindError, message: message
      {:ok, settings} -> settings
    end
  end

  def update_settings_from_file(settings, json_file) do
    expanded_path = FileUtil.expand_path(json_file)
    cond do
      not File.exists?(expanded_path) -> {:error, "Settings file not found: #{json_file}"}
      not String.ends_with?(json_file, ".json") -> {:error, "Invalid settings file (must be JSON): #{json_file}"}
      true ->
        case File.read(expanded_path) do
          {:ok, json} -> update_settings_from_json(settings, json)
          {:error, _e} -> {:error, "Unable to parse JSON"}
        end
    end
  end

  def get_settings_from_file(json_file) do
    update_settings_from_file(FindSettings.new(), json_file)
  end

  defp get_usage_string(options) do
    opt_strings = options
                  |> Enum.map(fn o -> {ExFind.FindOption.to_arg_string(o), o.description} end)
    longest = Enum.map(opt_strings, fn {opt, _} -> String.length(opt) end) |> Enum.max()
    opt_lines = opt_strings
                |> Enum.map(fn {opt, desc} -> " #{opt}#{String.duplicate(" ", longest - String.length(opt))}  #{desc}" end)
    """
    \nUsage:
     exfind [options] <path> [<path> ...]

    Options:
    #{Enum.join(opt_lines, "\n")}
    """
  end

  def usage(options) do
    IO.puts(get_usage_string(options))
  end
end
