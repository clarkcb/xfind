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
  alias ExFind.StringUtil

  require OptionParser

  defstruct options: [], bool_opts: [], str_opts: [], int_opts: []

  def new(args), do: __struct__(args)

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

    case invalid do
      [{opt, nil} | _rest] -> {:error, "Invalid option: #{trim_option(opt)}"}
      [] -> parsed_args_to_tokens(parsed_args_with_paths, arg_tokenizer, [])
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
