defmodule ExFindTest.FinderTest do
  alias ExFind.Config
  alias ExFind.Finder
  alias ExFind.FindSettings

  use ExUnit.Case
  doctest ExFind.Finder

  ################################################################################
  # matching_dir? tests
  ################################################################################
  test "no dir patterns" do
    settings = FindSettings.new([paths: ["."]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, "exfind") == true
  end

  test "dir matches in dir patterns" do
    settings = FindSettings.new() |> FindSettings.add_patterns(["find"], :in_dir_patterns)
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, "exfind") == true
  end

  test "dir does not match in dir patterns" do
    settings = FindSettings.new() |> FindSettings.add_patterns(["find"], :in_dir_patterns)
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, "file") == false
  end

  test "dir matches out dir patterns" do
    settings = FindSettings.new() |> FindSettings.add_patterns(["find"], :out_dir_patterns)
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, "exfind") == false
  end

  test "dir does not match out dir patterns" do
    settings = FindSettings.new() |> FindSettings.add_patterns(["find"], :out_dir_patterns)
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, "file") == true
  end

  test "dir matches single dot" do
    settings = FindSettings.new()
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, ".") == true
  end

  test "dir matches double dot" do
    settings = FindSettings.new()
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, "..") == true
  end


  test "dir is hidden" do
    settings = FindSettings.new()
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, ".hidden") == false
  end

  test "dir is hidden and include_hidden == true" do
    settings = FindSettings.new([include_hidden: true])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    assert Finder.matching_dir?(finder, ".hidden") == true
  end

  ################################################################################
  # filter_to_file_results tests
  ################################################################################
  test "non-hidden file path matches by default" do
    settings = FindSettings.new([paths: ["."]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "hidden file path does not match by default" do
    settings = FindSettings.new([paths: ["."]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, ".gitignore"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "file path has matching in extension" do
    settings = FindSettings.new([paths: ["."], in_extensions: ["ex"]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "file path does not have matching in extension" do
    settings = FindSettings.new([paths: ["."], in_extensions: ["rb"]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "file path has matching out extension" do
    settings = FindSettings.new([paths: ["."], out_extensions: ["ex"]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "file path does not have matching out extension" do
    settings = FindSettings.new([paths: ["."], out_extensions: ["rb"]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "file path matches in file pattern" do
    settings = FindSettings.new([paths: ["."], in_file_patterns: [~r/file/]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "file path does not match in file pattern" do
    settings = FindSettings.new([paths: ["."], in_file_patterns: [~r/find/]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "file path matches out file pattern" do
    settings = FindSettings.new([paths: ["."], out_file_patterns: [~r/file/]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "file path does not match out file pattern" do
    settings = FindSettings.new([paths: ["."], out_file_patterns: [~r/find/]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "file path has matching in file type" do
    settings = FindSettings.new([paths: ["."], in_file_types: [:code]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "file path does not have matching in file type" do
    settings = FindSettings.new([paths: ["."], in_file_types: [:xml]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "file path has matching out file type" do
    settings = FindSettings.new([paths: ["."], out_file_types: [:code]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "file path does not have matching out file type" do
    settings = FindSettings.new([paths: ["."], out_file_types: [:xml]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.xfind_path, "elixir", "exfind", "lib", "fileutil.ex"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "archive file path does not match by default" do
    settings = FindSettings.new([paths: ["."]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "archive file path matches with include_archives" do
    settings = FindSettings.new([paths: ["."], include_archives: true])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "archive file path matches with set_archives_only" do
    settings = FindSettings.new([paths: ["."]]) |> FindSettings.set_archives_only(true)
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "archive file path has matching in archive extension" do
    settings = FindSettings.new([paths: ["."], include_archives: true, in_archive_extensions: ["zip"]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "archive file path does not have matching in archive extension" do
    settings = FindSettings.new([paths: ["."], include_archives: true, in_archive_extensions: ["gz"]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "archive file path matches in archive file pattern" do
    settings = FindSettings.new([paths: ["."], include_archives: true, in_archive_file_patterns: [~r/arch/]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  test "archive file path does not match in archive file pattern" do
    settings = FindSettings.new([paths: ["."], include_archives: true, in_archive_file_patterns: [~r/comp/]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "archive file path matches out archive file pattern" do
    settings = FindSettings.new([paths: ["."], include_archives: true, out_archive_file_patterns: [~r/arch/]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 0
  end

  test "archive file path does not match out archive file pattern" do
    settings = FindSettings.new([paths: ["."], include_archives: true, out_archive_file_patterns: [~r/comp/]])
    finder = Finder.new([file_types: ExFind.FileTypes.new(), settings: settings])
    file_path = Path.join([Config.shared_path, "testFiles", "archive.zip"])
    results = Finder.filter_to_file_results(finder, file_path)
    assert length(results) == 1
  end

  ################################################################################
  # validate_settings tests
  ################################################################################
  test "valid settings" do
    settings = FindSettings.new([paths: ["."]])
    assert Finder.validate_settings(settings) == {:ok, "Settings are valid"}
  end

  test "startpath not defined" do
    settings = FindSettings.new([paths: []])
    assert Finder.validate_settings(settings) == {:error, "Startpath not defined"}
  end

  test "startpath not found" do
    settings = FindSettings.new([paths: ["/non/existent/path"]])
    assert Finder.validate_settings(settings) == {:error, "Startpath not found"}
  end

test "invalid range for mindepth and maxdepth" do
    settings = FindSettings.new([min_depth: 2, max_depth: 1, paths: ["."]])
    assert Finder.validate_settings(settings) == {:error, "Invalid range for mindepth and maxdepth"}
  end

  test "invalid range for minsize and maxsize" do
    settings = FindSettings.new([min_size: 2000, max_size: 1000, paths: ["."]])
    assert Finder.validate_settings(settings) == {:error, "Invalid range for minsize and maxsize"}
  end

  test "invalid range for minlastmod and maxlastmod" do
    min_last_mod = DateTime.utc_now |> DateTime.add(-1000)
    max_last_mod = DateTime.utc_now |> DateTime.add(-2000)
    settings = FindSettings.new([min_last_mod: min_last_mod, max_last_mod: max_last_mod, paths: ["."]])
    assert Finder.validate_settings(settings) == {:error, "Invalid range for minlastmod and maxlastmod"}
  end
end
