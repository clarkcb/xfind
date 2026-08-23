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

  def load_options(config) do
    # Load the find options from the findoptions.json file.
    {:ok, json} = File.read(config.find_options_path)
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

  defstruct [:config, :options, :arg_tokenizer, :arg_action_maps]

  def new(config) do
    options = ExFind.FindOptionsLoader.load_options(config)
    arg_action_maps = arg_action_maps()
    {bool_arg_action_map, int_arg_action_map, str_arg_action_map} = arg_action_maps
    arg_tokenizer = %ArgTokenizer{
      options: options,
      bool_opts: Map.keys(bool_arg_action_map),
      int_opts: Map.keys(int_arg_action_map),
      str_opts: Map.keys(str_arg_action_map)
    }
    __struct__([config: config, options: options, arg_tokenizer: arg_tokenizer, arg_action_maps: arg_action_maps])
  end

  defp bool_arg_action_map() do
    %{
      archivesonly: fn b, settings -> FindSettings.set_archives_only(settings, b) end,
      colorize: fn b, settings -> %{settings | colorize: b} end,
      debug: fn b, settings -> FindSettings.set_debug(settings, b) end,
      defaultfiles: fn b, settings -> %{settings | default_files: b} end,
      excludearchives: fn b, settings -> %{settings | include_archives: not b} end,
      excludehidden: fn b, settings -> %{settings | include_hidden: not b} end,
      followsymlinks: fn b, settings -> %{settings | follow_symlinks: b} end,
      help: fn b, settings -> %{settings | print_usage: b} end,
      includearchives: fn b, settings -> %{settings | include_archives: b} end,
      includehidden: fn b, settings -> %{settings | include_hidden: b} end,
      nocolorize: fn b, settings -> %{settings | colorize: not b} end,
      nodefaultfiles: fn b, settings -> %{settings | default_files: not b} end,
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

  def update_settings_from_json(find_options, settings, json) do
    case ArgTokenizer.tokenize_json(json, find_options.arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens(find_options, settings, tokens)
      {:error, message} -> {:error, message}
    end
  end

  def get_settings_from_json(find_options, json) do
    update_settings_from_json(find_options, FindSettings.new(), json)
  end

  def get_settings_from_json!(find_options, json) do
    case get_settings_from_json(find_options, json) do
      {:error, message} -> raise FindError, message: message
      {:ok, settings} -> settings
    end
  end

  def update_settings_from_file(find_options, settings, json_file) do
    case ArgTokenizer.tokenize_file(json_file, find_options.arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens(find_options, settings, tokens)
      {:error, message} -> {:error, message}
    end
  end

  def get_settings_from_file(find_options, json_file) do
    case update_settings_from_file(find_options, FindSettings.new(), json_file) do
      {:error, "Unable to parse JSON"} -> {:error, "Unable to parse JSON in settings file: #{json_file}"}
      {:error, message} -> {:error, message}
      {:ok, settings} -> {:ok, settings}
    end
  end

  def get_settings_from_file!(find_options, json_file) do
    case get_settings_from_file(find_options, json_file) do
      {:error, message} -> raise FindError, message: message
      {:ok, settings} -> settings
    end
  end

  def update_settings_from_tokens!(find_options, settings, tokens) do
    {bool_arg_action_map, int_arg_action_map, str_arg_action_map} = find_options.arg_action_maps
    case tokens do
      [] -> settings
      [t | ts] ->
        case t.arg_type do
          :boolean ->
            k = t.name
            v = t.value
            cond do
              k == :defaultfiles && v == true ->
                case update_settings_from_default_files(find_options, settings) do
                  {:ok, new_settings} -> update_settings_from_tokens!(find_options, new_settings, ts)
                  {:error, message} -> raise FindError, message: message
                end
              Map.has_key?(bool_arg_action_map, k) ->
                update_settings_from_tokens!(find_options, Map.get(bool_arg_action_map, k).(v, settings), ts)
              true -> raise FindError, message: "Invalid value for option: #{k}"
            end
          :integer ->
            k = t.name
            v = t.value
            cond do
              Map.has_key?(int_arg_action_map, k) ->
                update_settings_from_tokens!(find_options, Map.get(int_arg_action_map, k).(v, settings), ts)
              true -> raise FindError, message: "Invalid value for option: #{k}"
            end
          :string ->
            k = t.name
            v = t.value
            cond do
              Map.has_key?(str_arg_action_map, k) ->
                update_settings_from_tokens!(find_options, Map.get(str_arg_action_map, k).(v, settings), ts)
              k == :settings_file ->  case update_settings_from_file(find_options, settings, v) do
                {:ok, new_settings} -> update_settings_from_tokens!(find_options, new_settings, ts)
                {:error, message} -> raise FindError, message: message
              end
              true -> raise FindError, message: "Invalid value for option: #{k}"
            end
          :unknown ->
            raise FindError, message: "Invalid option: #{t.name}"
        end
    end
  end

  def update_settings_from_tokens(find_options, settings, tokens) do
    try do
      {:ok, update_settings_from_tokens!(find_options, settings, tokens)}
    rescue
      e in FindError -> {:error, e.message}
    end
  end

  def update_settings_from_args!(find_options, settings, args) do
    case ArgTokenizer.tokenize_args(args, find_options.arg_tokenizer) do
      {:ok, tokens} -> update_settings_from_tokens!(find_options, settings, tokens)
      {:error, message} -> raise FindError, message: message
    end
  end

  def update_settings_from_args(find_options, settings, args) do
    try do
      {:ok, update_settings_from_args!(find_options, settings, args)}
    rescue
      e in FindError -> {:error, e.message}
    end
  end

  def update_settings_from_default_files(find_options, settings) do
    if File.exists?(find_options.config.default_find_settings_path) do
      update_settings_from_file(find_options, settings, find_options.config.default_find_settings_path)
    else
      {:ok, settings}
    end
  end

  def get_settings_from_args(find_options, args) do
    settings = FindSettings.new([print_files: true])
    if Enum.empty?(args) do
      {:ok, settings}
    else
      if Enum.any?(args, fn a -> a == "--defaultfiles" || a == "--nodefaultfiles" end) do
        update_settings_from_args(find_options, settings, args)
      else
        # if a defaultfiles option isn't included, go ahead and apply default files now
        case update_settings_from_default_files(find_options, settings) do
          {:error, message} -> {:error, message}
          {:ok, settings} ->
            update_settings_from_args(find_options, settings, args)
        end
      end
    end
  end

  def get_settings_from_args!(find_options, args) do
    case get_settings_from_args(find_options, args) do
      {:error, message} -> raise FindError, message: message
      {:ok, settings} -> settings
    end
  end

  defp get_usage_string(find_options) do
    opt_strings = find_options.options
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

  def usage(find_options) do
    IO.puts(get_usage_string(find_options))
  end
end
