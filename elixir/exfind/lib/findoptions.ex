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
    {:ok, json} = File.read(find_options_path)
    find_options = JSON.decode!(json)
    find_options["findoptions"]
    |> Enum.map(fn o -> ExFind.FindOption.new([short_arg: Map.get(o, "short", ""), long_arg: o["long"], description: o["desc"]]) end)
  end
end

defmodule ExFind.FindOptions do
  @moduledoc """
  Documentation for `ExFind.FindOptions`.
  """

  alias ExFind.ArgTokenizer
  alias ExFind.FileTypes
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
      colorize: fn b, settings -> %{settings | colorize: b} end,
      debug: fn b, settings -> FindSettings.set_debug(settings, b) end,
      excludearchives: fn b, settings -> %{settings | include_archives: not b} end,
      excludehidden: fn b, settings -> %{settings | include_hidden: not b} end,
      followsymlinks: fn b, settings -> %{settings | follow_symlinks: b} end,
      help: fn b, settings -> %{settings | print_usage: b} end,
      includearchives: fn b, settings -> %{settings | include_archives: b} end,
      includehidden: fn b, settings -> %{settings | include_hidden: b} end,
      nocolorize: fn b, settings -> %{settings | colorize: not b} end,
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

  defp get_arg_tokenizer(options, arg_action_maps) do
    {bool_arg_action_map, int_arg_action_map, str_arg_action_map} = arg_action_maps
    %ArgTokenizer{
      options: options,
      bool_opts: Map.keys(bool_arg_action_map),
      int_opts: Map.keys(int_arg_action_map),
      str_opts: Map.keys(str_arg_action_map)
    }
  end

  def update_settings_from_json(settings, json, arg_tokenizer, arg_action_maps) do
    case ArgTokenizer.tokenize_json(json, arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens(settings, tokens, arg_tokenizer, arg_action_maps)
      {:error, message} -> {:error, message}
    end
  end

  def get_settings_from_json(json, options) do
    arg_action_maps = arg_action_maps()
    arg_tokenizer = get_arg_tokenizer(options, arg_action_maps)
    update_settings_from_json(FindSettings.new(), json, arg_tokenizer, arg_action_maps)
  end

  def get_settings_from_json!(json, options) do
    case get_settings_from_json(json, options) do
      {:error, message} -> raise FindError, message: message
      {:ok, settings} -> settings
    end
  end

  def update_settings_from_file(settings, json_file, arg_tokenizer, arg_action_maps) do
    case ArgTokenizer.tokenize_file(json_file, arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens(settings, tokens, arg_tokenizer, arg_action_maps)
      {:error, message} -> {:error, message}
    end
  end

  def get_settings_from_file(json_file, options) do
    arg_action_maps = arg_action_maps()
    arg_tokenizer = get_arg_tokenizer(options, arg_action_maps)
    case update_settings_from_file(FindSettings.new(), json_file, arg_tokenizer, arg_action_maps) do
      {:error, "Unable to parse JSON"} -> {:error, "Unable to parse JSON in settings file: #{json_file}"}
      {:error, message} -> {:error, message}
      {:ok, settings} -> {:ok, settings}
    end
  end

  def get_settings_from_file!(json_file, options) do
    case get_settings_from_file(json_file, options) do
      {:error, message} -> raise FindError, message: message
      {:ok, settings} -> settings
    end
  end

  def update_settings_from_tokens!(settings, tokens, arg_tokenizer, arg_action_maps) do
    {bool_arg_action_map, int_arg_action_map, str_arg_action_map} = arg_action_maps
    case tokens do
      [] -> settings
      [t | ts] ->
        case t.arg_type do
          :boolean ->
            k = t.name
            v = t.value
            cond do
              Map.has_key?(bool_arg_action_map, k) ->
                update_settings_from_tokens!(Map.get(bool_arg_action_map, k).(v, settings), ts, arg_tokenizer, arg_action_maps)
              true -> raise FindError, message: "Invalid option: #{k}"
            end
          :integer ->
            k = t.name
            v = t.value
            cond do
              Map.has_key?(int_arg_action_map, k) ->
                update_settings_from_tokens!(Map.get(int_arg_action_map, k).(v, settings), ts, arg_tokenizer, arg_action_maps)
              true -> raise FindError, message: "Invalid option: #{k}"
            end
          :string ->
            k = t.name
            v = t.value
            cond do
              Map.has_key?(str_arg_action_map, k) ->
                update_settings_from_tokens!(Map.get(str_arg_action_map, k).(v, settings), ts, arg_tokenizer, arg_action_maps)
              k == :settings_file -> case update_settings_from_file(settings, v, arg_tokenizer, arg_action_maps) do
                {:ok, new_settings} -> update_settings_from_tokens!(new_settings, ts, arg_tokenizer, arg_action_maps)
                {:error, message} -> raise FindError, message: message
              end
              true -> raise FindError, message: "Invalid option: #{k}"
            end
          :unknown ->
            raise FindError, message: "Invalid option: #{t.name}"
        end
    end
  end

  def update_settings_from_tokens(settings, tokens, arg_tokenizer, arg_action_maps) do
    try do
      {:ok, update_settings_from_tokens!(settings, tokens, arg_tokenizer, arg_action_maps)}
    rescue
      e in FindError -> {:error, e.message}
    end
  end

  def update_settings_from_args!(settings, args, options) do
    arg_action_maps = arg_action_maps()
    arg_tokenizer = get_arg_tokenizer(options, arg_action_maps)
    case ArgTokenizer.tokenize_args(args, arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens!(settings, tokens, arg_tokenizer, arg_action_maps)
      {:error, message} -> raise FindError, message: message
    end
  end

  def update_settings_from_args(settings, args, options) do
    try do
      {:ok, update_settings_from_args!(settings, args, options)}
    rescue
      e in FindError -> {:error, e.message}
    end
  end

  def get_settings_from_args(args, options) do
    settings = FindSettings.new([print_files: true])
    update_settings_from_args(settings, args, options)
  end

  def get_settings_from_args!(args, options) do
    case get_settings_from_args(args, options) do
      {:error, message} -> raise FindError, message: message
      {:ok, settings} -> settings
    end
  end

  defp get_usage_string(options) do
    opt_strings = options
                  |> Enum.sort(fn o1, o2 -> ExFind.FindOption.sort_arg(o1) <= ExFind.FindOption.sort_arg(o2) end)
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
