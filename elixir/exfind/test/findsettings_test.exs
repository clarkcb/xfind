defmodule ExFindTest.FindSettingsTest do
  alias ExFind.FindSettings
  alias ExFind.StringUtil
  use ExUnit.Case
  doctest ExFind.FindSettings

  test "default settings" do
    settings = FindSettings.new()
    assert settings.archives_only == false
    assert settings.debug == false
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
    assert settings.print_files == false
    assert settings.print_usage == false
    assert settings.print_version == false
    assert settings.recursive == true
    assert settings.sort_by == :file_path
    assert settings.sort_case_insensitive == false
    assert settings.sort_descending == false
    assert settings.verbose == false
  end

  test "add an extension" do
    settings = FindSettings.new()
    settings = FindSettings.add_extensions(settings, "ex", :in_extensions)
    assert settings.in_extensions == ["ex"]
  end

  test "add comma-delimited extensions" do
    settings = FindSettings.new()
    settings = FindSettings.add_extensions(settings, "ex,exs", :in_extensions)
    assert settings.in_extensions == ["ex", "exs"]
  end

  test "add list of extensions" do
    settings = FindSettings.new()
    settings = FindSettings.add_extensions(settings, ["ex", "exs"], :in_extensions)
    assert settings.in_extensions == ["ex", "exs"]
  end

  test "add a pattern" do
    settings = FindSettings.new()
    settings = FindSettings.add_pattern(settings, "find", :in_file_patterns)
    assert settings.in_file_patterns == [~r/find/]
  end

  test "add an invalid pattern" do
    settings = FindSettings.new()
    assert_raise ExFind.FindError, fn ->
      _ = FindSettings.add_patterns(settings, ["file", "find)"], :in_file_patterns)
    end
  end

  test "add list of patterns" do
    settings = FindSettings.new()
    settings = FindSettings.add_patterns(settings, ["find", "file"], :in_file_patterns)
    assert settings.in_file_patterns == [~r/find/, ~r/file/]
  end

  test "set archives_only sets include_archives" do
    settings = FindSettings.new()
    settings = FindSettings.set_archives_only(settings, true)
    assert settings.archives_only == true
    assert settings.include_archives == true
  end

  test "set debug sets verbose" do
    settings = FindSettings.new()
    settings = FindSettings.set_debug(settings, true)
    assert settings.debug == true
    assert settings.verbose == true
  end

  test "convert date string to datetime instance" do
    date_str = "2024-06-01"
    datetime = StringUtil.to_datetime(date_str)
    assert datetime == ~U[2024-06-01 00:00:00Z]
  end

  test "convert datetime string to datetime instance" do
    datetime_str = "2024-06-01T09:30:00Z"
    datetime = StringUtil.to_datetime(datetime_str)
    assert datetime == ~U[2024-06-01 09:30:00Z]
  end

end
