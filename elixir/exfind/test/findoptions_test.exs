defmodule ExFindTest.FindOptionsTest do
  alias ExFind.FindOptions
  use ExUnit.Case
  doctest ExFind.FindOptions

  test "no args" do
    find_options = FindOptions.new()
    {:ok, settings} = FindOptions.get_settings_from_args([], find_options.options)
    assert settings.archives_only == false
    assert settings.debug == false
    assert settings.follow_symlinks == false
    assert settings.in_archive_extensions == []
    assert settings.in_archive_file_patterns == []
    assert settings.in_dir_patterns == []
    assert settings.in_extensions == []
    assert settings.in_file_patterns == []
    assert settings.in_file_types == []
    assert settings.include_archives == false
    assert settings.include_hidden == false
    assert settings.max_depth == -1
    assert settings.max_last_mod == nil
    assert settings.max_size == 0
    assert settings.min_depth == -1
    assert settings.min_last_mod == nil
    assert settings.min_size == 0
    assert settings.out_archive_extensions == []
    assert settings.out_archive_file_patterns == []
    assert settings.out_dir_patterns == []
    assert settings.out_extensions == []
    assert settings.out_file_patterns == []
    assert settings.out_file_types == []
    assert settings.paths == []
    assert settings.print_dirs == false
    assert settings.print_files == true
    assert settings.print_usage == false
    assert settings.print_version == false
    assert settings.recursive == true
    assert settings.sort_by == :file_path
    assert settings.sort_case_insensitive == false
    assert settings.sort_descending == false
    assert settings.verbose == false
  end

  test "valid args" do
    find_options = FindOptions.new()
    {:ok, settings} = FindOptions.get_settings_from_args(["-x", "ex,exs", "src", "-f", "find"], find_options.options)
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["src"]
    # assert settings.in_file_patterns == [~r/find/]
    in_file_patterns_source = settings.in_file_patterns |> Enum.map(fn p -> p.source end)
    assert in_file_patterns_source == [~r/find/.source]
  end

  test "set archives_only" do
    find_options = FindOptions.new()
    {:ok, settings} = FindOptions.get_settings_from_args(["--archivesonly"], find_options.options)
    assert settings.archives_only == true
    assert settings.include_archives == true
  end

  test "set debug" do
    find_options = FindOptions.new()
    {:ok, settings} = FindOptions.get_settings_from_args(["--debug"], find_options.options)
    assert settings.debug == true
    assert settings.verbose == true
  end

  test "settings from json" do
    json = """
    {
      "in-ext": ["ex", "exs"],
      "path": ["~/src/xfind/elixir/exfind"],
      "out-dirpattern": ["dep"]
      "out-filepattern": ["test"],
      "debug": true,
      "followsymlinks": true,
      "includehidden": true
    }
    """
    find_options = FindOptions.new()
    {status, settings} = FindOptions.get_settings_from_json(json, find_options.options)
    assert status == :ok
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["~/src/xfind/elixir/exfind"]
    # assert settings.out_dir_patterns == [~r/dep/]
    out_dir_patterns_source = settings.out_dir_patterns |> Enum.map(fn p -> p.source end)
    assert out_dir_patterns_source == [~r/dep/.source]

    # assert settings.out_file_patterns == [~r/test/]
    out_file_patterns_source = settings.out_file_patterns |> Enum.map(fn p -> p.source end)
    assert out_file_patterns_source == [~r/test/.source]

    assert settings.debug == true
    assert settings.verbose == true
    assert settings.follow_symlinks == true
    assert settings.include_hidden == true
  end

  test "settings from invalid json" do
    json = """
    {
      "in-ext": ["ex", "exs",
      "path": ["~/src/xfind/elixir/exfind"],
      "out-dirpattern": ["dep"]
      "out-filepattern": ["test"],
      "debug": true,
      "includehidden": true,
    }
    """
    find_options = FindOptions.new()
    {status, _value} = FindOptions.get_settings_from_json(json, find_options.options)
    assert status == :error
  end

  test "settings! from json" do
    json = """
    {
      "in-ext": ["ex", "exs"],
      "path": ["~/src/xfind/elixir/exfind"],
      "out-dirpattern": ["dep"]
      "out-filepattern": ["test"],
      "debug": true,
      "includehidden": true
    }
    """
    find_options = FindOptions.new()
    settings = FindOptions.get_settings_from_json!(json, find_options.options)
    assert settings.in_extensions == ["ex", "exs"]
    assert settings.paths == ["~/src/xfind/elixir/exfind"]
    # assert settings.out_dir_patterns == [~r/dep/]
    out_dir_patterns_source = settings.out_dir_patterns |> Enum.map(fn p -> p.source end)
    assert out_dir_patterns_source == [~r/dep/.source]

    # assert settings.out_file_patterns == [~r/test/]
    out_file_patterns_source = settings.out_file_patterns |> Enum.map(fn p -> p.source end)
    assert out_file_patterns_source == [~r/test/.source]

    assert settings.debug == true
    assert settings.include_hidden == true
  end

  test "settings! from invalid json" do
    json = """
    {
      "in-ext": ["ex", "exs",
      "path": ["~/src/xfind/elixir/exfind"],
      "out-dirpattern": ["dep"]
      "out-filepattern": ["test"],
      "debug": true,
      "includehidden": true,
    }
    """
    find_options = FindOptions.new()
    assert_raise ExFind.FindError, fn ->
      _ = FindOptions.get_settings_from_json!(json, find_options.options)
    end
  end

  test "settings from non-existent file" do
    json_file = "/non/existent/file.json"
    find_options = FindOptions.new()
    {status, _value} = FindOptions.get_settings_from_file(json_file, find_options.options)
    assert status == :error
  end
end
