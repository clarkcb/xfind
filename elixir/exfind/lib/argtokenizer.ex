defmodule ExFind.ArgToken do
  @moduledoc """
  Documentation for `ExFind.ArgToken`.
  """

  defstruct name: :name, arg_type: :unknown, value: nil

  def new(args), do: __struct__(args)

end

defmodule ExFind.ArgTokenizer do
  @moduledoc """
  Documentation for `ExFind.ArgTokenizer`.
  """
  alias ExFind.ArgToken
  alias ExFind.FileUtil
  alias ExFind.FindError
  alias ExFind.StringUtil

  require OptionParser

  @num_modifier_regex ~r/^\d+([ckmgtp])$/i

  defstruct options: [], bool_opts: [], str_opts: [], int_opts: []

  def new(args), do: __struct__(args)

  def parsed_string_arg_to_token(arg_name, str_value) do
    ArgToken.new(name: arg_name, arg_type: :string, value: str_value)
  end

  def parsed_args_to_tokens(parsed_args, arg_tokenizer, tokens) do
    case parsed_args do
      [] -> {:ok, tokens}
      [p | ps] ->
        case p do
          {arg_name, true} ->
            token = ArgToken.new(name: arg_name, arg_type: :boolean, value: true)
            parsed_args_to_tokens(ps, arg_tokenizer, tokens ++ [token])
          {arg_name, false} ->
            token = ArgToken.new(name: arg_name, arg_type: :boolean, value: false)
            parsed_args_to_tokens(ps, arg_tokenizer, tokens ++ [token])
          {arg_name, str_value} when is_binary(str_value) ->
            token = ArgToken.new(name: arg_name, arg_type: :string, value: str_value)
            parsed_args_to_tokens(ps, arg_tokenizer, tokens ++ [token])
          {arg_name, list_value} when is_list(list_value) ->
            new_tokens = Enum.map(list_value, fn s -> ArgToken.new(name: arg_name, arg_type: :string, value: s) end)
            parsed_args_to_tokens(ps, arg_tokenizer, tokens ++ new_tokens)
          {arg_name, int_value} when is_integer(int_value) ->
            token = ArgToken.new(name: arg_name, arg_type: :integer, value: int_value)
            parsed_args_to_tokens(ps, arg_tokenizer, tokens ++ [token])
          {arg_name, _other} ->
            {:error, "Invalid option: #{arg_name}"}
        end
    end
  end

  def trim_option(option) do
    String.replace_leading(option, "-", "")
  end

  def parse_size!(size) do
    num_modifier_regex = @num_modifier_regex
    cond do
      is_integer(size) -> size
      is_binary(size) and Regex.match?(num_modifier_regex, size) ->
        multiplier = case String.downcase(String.slice(size, -1..-1//1)) do
          "c" -> 1
          "k" -> 1024
          "m" -> 1024 * 1024
          "g" -> 1024 * 1024 * 1024
          "t" -> 1024 * 1024 * 1024 * 1024
          "p" -> 1024 * 1024 * 1024 * 1024 * 1024
        end
        num_part = String.slice(size, 0..-2//1) |> String.to_integer()
        num_part * multiplier
      true -> raise FindError, message: "Invalid size: #{size}"
    end
  end

  def parse_size(size) do
    try do
      parsed_size = parse_size!(size)
      {:ok, parsed_size}
    rescue
      e in FindError -> {:error, e.message}
    end
  end

  def process_invalid_args(invalid_args, arg_tokenizer, processed_args) do
    case invalid_args do
      [] -> {:ok, processed_args}
      [{opt, "(-1)"} | rest] -> process_invalid_args(rest, arg_tokenizer, processed_args ++ [{String.to_atom(String.slice(opt, 2..-1//1)), -1}])
      [{"--maxsize", arg_val} | rest] ->
        case parse_size(arg_val) do
          {:ok, size} -> process_invalid_args(rest, arg_tokenizer, processed_args ++ [{:maxsize, size}])
          {:error, _e} -> {:error, "Invalid value for option maxsize: #{arg_val}"}
        end
      [{"--minsize", arg_val} | rest] ->
        case parse_size(arg_val) do
          {:ok, size} -> process_invalid_args(rest, arg_tokenizer, processed_args ++ [{:minsize, size}])
          {:error, _e} -> {:error, "Invalid value for option minsize: #{arg_val}"}
        end
      [{opt, _} | _rest] -> {:error, "Invalid option: #{trim_option(opt)}"}
    end
  end

  def tokenize_size_arg(arg_name, str_value) do
    IO.puts("tokenize_size_arg(#{arg_name}, #{str_value})")
    case parse_size(str_value) do
      {:ok, size} -> {:ok, [ArgToken.new(name: arg_name, arg_type: :integer, value: size)]}
      {:error, _e} -> {:error, "Invalid size option: #{arg_name}, value: #{str_value}"}
    end
  end

  def tokenize_args(args, arg_tokenizer) do
    bool_opts = arg_tokenizer.bool_opts |> Enum.map(fn o -> {o, :boolean} end)
    int_opts = arg_tokenizer.int_opts |> Enum.map(fn o -> {o, :integer} end)
    # str_opts = arg_tokenizer.str_opts |> Enum.map(fn o -> {o, :string} end)
    # :keep allows for duplicates, assumes :string type
    str_opts = arg_tokenizer.str_opts ++ [:path, :settings_file] |> Enum.map(fn o -> {o, :keep} end)
    parser_opts = bool_opts ++ int_opts ++ str_opts
    alias_opts = arg_tokenizer.options
                 |> Enum.filter(fn o -> o.short_arg != "" end)
                 |> Enum.map(fn o -> {String.to_atom(o.short_arg), String.to_atom(String.replace(o.long_arg, "-", "_"))} end)
    {parsed_args, paths, invalid} = OptionParser.parse(args, strict: parser_opts, aliases: alias_opts)
    parsed_args_with_paths = parsed_args ++ Enum.map(paths, fn p -> {:path, p} end)
    case process_invalid_args(invalid, arg_tokenizer, []) do
      {:ok, processed_invalid_args} -> parsed_args_to_tokens(parsed_args_with_paths ++ processed_invalid_args, arg_tokenizer, [])
      {:error, _e} = error -> error
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
    keyword_list = Enum.map(map, fn {k, v} -> {StringUtil.atomize(k), v} end)
    flattened = flatten_keyword_list(keyword_list, [])
    flattened
  end

  def tokenize_json(json, arg_tokenizer) do
    case JSON.decode(json) do
      {:ok, parsed_json} ->
        json_keyword_list = convert_map_to_keyword_list(parsed_json)
        parsed_args_to_tokens(json_keyword_list, arg_tokenizer, [])
      {:error, _e} -> {:error, "Unable to parse JSON"}
    end
  end

  def tokenize_file(file_path, arg_tokenizer) do
    expanded_path = FileUtil.expand_path(file_path)
    cond do
      not File.exists?(expanded_path) -> {:error, "Settings file not found: #{file_path}"}
      not String.ends_with?(file_path, ".json") -> {:error, "Invalid settings file (must be JSON): #{file_path}"}
      true ->
        case File.read(expanded_path) do
          {:ok, json} -> tokenize_json(json, arg_tokenizer)
          {:error, _e} -> {:error, "Unable to read settings file: #{file_path}"}
        end
    end
  end

end
