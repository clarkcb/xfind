defmodule ExFind.Finder do
  @moduledoc """
  Documentation for `ExFind.Finder`.
  """

  alias ExFind.FileResult
  alias ExFind.FileResultFormatter
  alias ExFind.FileResultSorter
  alias ExFind.FileTypes
  alias ExFind.FileUtil
  alias ExFind.FindError
  alias ExFind.FindSettings
  alias ExFind.Logging
  alias ExFind.StringUtil

  defstruct [:file_types, :settings]

  # def new(args), do: __struct__(args)
  def new(settings) do
    file_types = ExFind.FileTypes.new()
    __struct__([file_types: file_types, settings: settings])
  end

  def matches_any_string?(string, strings) do
    Enum.any?(strings, fn s -> s == string end)
  end

  def empty_or_matches_any_string?(string, strings) do
    Enum.empty?(strings) or matches_any_string?(string, strings)
  end

  def empty_or_not_matches_any_string?(string, strings) do
    Enum.empty?(strings) or not matches_any_string?(string, strings)
  end

  def matches_any_pattern?(string, patterns) do
    Enum.any?(patterns, fn p -> Regex.match?(p, string) end)
  end

  def empty_or_matches_any_pattern?(string, patterns) do
    Enum.empty?(patterns) or matches_any_pattern?(string, patterns)
  end

  def empty_or_not_matches_any_pattern?(string, patterns) do
    Enum.empty?(patterns) or not matches_any_pattern?(string, patterns)
  end

  def any_matches_any_pattern?(strings, patterns) do
    Enum.any?(strings, fn s -> matches_any_pattern?(s, patterns) end)
  end

  def empty_or_any_matches_any_pattern?(strings, patterns) do
    Enum.empty?(patterns) or any_matches_any_pattern?(strings, patterns)
  end

  def empty_or_not_any_matches_any_pattern?(strings, patterns) do
    Enum.empty?(patterns) or not any_matches_any_pattern?(strings, patterns)
  end

  def matching_dir_by_hidden?(finder, dir) do
    # IO.puts("matching_dir_by_hidden?(#{dir})")
    (finder.settings.include_hidden or not FileUtil.hidden_path?(dir))
  end

  def matching_dir_by_in_patterns?(finder, dir) do
    # IO.puts("matching_dir_by_in_patterns?(#{dir})")
     empty_or_any_matches_any_pattern?(Path.split(dir), finder.settings.in_dir_patterns)
  end

  def matching_dir_by_out_patterns?(finder, dir) do
    # IO.puts("matching_dir_by_out_patterns?(#{dir})")
    empty_or_not_any_matches_any_pattern?(Path.split(dir), finder.settings.out_dir_patterns)
  end

  defp traversable_dir?(finder, dir) do
    matching_dir_by_hidden?(finder, dir) and matching_dir_by_out_patterns?(finder, dir)
  end

  def matching_dir?(finder, dir) do
    # IO.puts("matching_dir?(#{dir})")
    matching_dir_by_hidden?(finder, dir)
    and matching_dir_by_in_patterns?(finder, dir)
    and matching_dir_by_out_patterns?(finder, dir)
  end

  def null_or_matching_dir?(finder, dir) do
    # IO.puts("null_or_matching_dir?(#{dir})")
    dir == nil or String.trim(dir) == "" or matching_dir?(finder, dir)
  end

  def matching_file_name_by_hidden?(finder, file_name) do
    # IO.puts("matching_file_name_by_hidden?(#{file_name})")
    (finder.settings.include_hidden or not FileUtil.hidden_name?(file_name))
  end

  def matching_archive_extension?(finder, ext) do
    # IO.puts("matching_extension?(#{ext})")
    empty_or_matches_any_string?(ext, finder.settings.in_archive_extensions)
    and empty_or_not_matches_any_string?(ext, finder.settings.out_archive_extensions)
  end

  def has_matching_archive_extension?(finder, file_name) do
    # IO.puts("has_matching_archive_extension?(#{file_name})")
    if not Enum.empty?(finder.settings.in_archive_extensions) or not Enum.empty?(finder.settings.out_archive_extensions) do
      ext = FileUtil.get_extension(file_name)
      matching_archive_extension?(finder, ext)
    else
      true
    end
  end

  def matching_extension?(finder, ext) do
    # IO.puts("matching_extension?(#{ext})")
    empty_or_matches_any_string?(ext, finder.settings.in_extensions)
    and empty_or_not_matches_any_string?(ext, finder.settings.out_extensions)
  end

  def has_matching_extension?(finder, file_name) do
    # IO.puts("has_matching_extension?(#{file_name})")
    if not Enum.empty?(finder.settings.in_extensions) or not Enum.empty?(finder.settings.out_extensions) do
      ext = FileUtil.get_extension(file_name)
      matching_extension?(finder, ext)
    else
      true
    end
  end

  def matching_archive_file_name?(finder, file_name) do
    # IO.puts("matching_file_name?(#{file_name})")
    empty_or_matches_any_pattern?(file_name, finder.settings.in_archive_file_patterns)
    and empty_or_not_matches_any_pattern?(file_name, finder.settings.out_archive_file_patterns)
  end

  def matching_archive_file_path?(finder, file_path) do
    # IO.puts("matching_archive_file_path?(#{file_path})")
    file_name = Path.basename(file_path)
    has_matching_archive_extension?(finder, file_name) and matching_archive_file_name?(finder, file_name)
  end

  # TODO: change this to matching_archive_file_result
  def matching_archive_file_path?(finder, file_path, _file_size, _last_mod) do
    # IO.puts("matching_archive_file_path?(#{file_path})")
    file_name = Path.basename(file_path)
    (finder.settings.include_hidden or not FileUtil.hidden_name?(Path.basename(file_name)))
    and has_matching_archive_extension?(finder, file_name)
    and matching_archive_file_name?(finder, file_name)
  end

  def matching_file_name?(finder, file_name) do
    # IO.puts("matching_file_name?(#{file_name})")
    empty_or_matches_any_pattern?(file_name, finder.settings.in_file_patterns)
    and empty_or_not_matches_any_pattern?(file_name, finder.settings.out_file_patterns)
  end

  def matching_file_path?(finder, file_path) do
    # IO.puts("matching_file_path?(#{file_path})")
    file_name = Path.basename(file_path)
    has_matching_extension?(finder, file_name) and matching_file_name?(finder, file_name)
  end

  def matching_file_type?(finder, file_type) do
    # IO.puts("matching_file_type?(#{file_type})")
    (Enum.empty?(finder.settings.in_file_types) or Enum.any?(finder.settings.in_file_types, fn t -> t == file_type end))
    and (Enum.empty?(finder.settings.out_file_types) or !Enum.any?(finder.settings.out_file_types, fn t -> t == file_type end))
  end

  def matching_file_size?(finder, file_size) do
    # IO.puts("matching_file_size?(#{file_size})")
    (finder.settings.min_size == 0 or file_size >= finder.settings.min_size)
    and (finder.settings.max_size == 0 or file_size <= finder.settings.max_size)
  end

  def matching_last_mod?(finder, last_mod) do
    # IO.puts("matching_last_mod?(#{inspect(last_mod)})")
    (finder.settings.min_last_mod == nil
     or last_mod >= DateTime.to_unix(finder.settings.min_last_mod))
    and (finder.settings.max_last_mod == nil
         or last_mod <= DateTime.to_unix(finder.settings.max_last_mod))
  end

  # TODO: change this to matching_file_result
  def matching_file_path?(finder, file_path, file_type, file_size, last_mod) do
    # IO.puts("matching_file_path?(#{file_path})")
    file_name = Path.basename(file_path)
    (finder.settings.include_hidden or not FileUtil.hidden_name?(Path.basename(file_name)))
    and has_matching_extension?(finder, file_name)
    and matching_file_name?(finder, file_name)
    and matching_file_type?(finder, file_type)
    and matching_file_size?(finder, file_size)
    and matching_last_mod?(finder, last_mod)
  end

  def filter_archive_file_path_to_file_results(finder, file_path) do
    # IO.puts("filter_archive_file_path_to_file_results(#{file_path})")
    if not finder.settings.include_archives and not finder.settings.archives_only do
      []
    else
      if not matching_archive_file_path?(finder, file_path) do
        []
      else
        [FileResult.new(Path.dirname(file_path), Path.basename(file_path), :archive, 0, 0)]
      end
    end
  end

  def filter_regular_file_path_to_file_results(finder, file_path, file_type) do
    # IO.puts("filter_regular_file_path_to_file_results(#{file_path})")
    if finder.settings.archives_only do
      []
    else
      if not matching_file_path?(finder, file_path) or not matching_file_type?(finder, file_type) do
        []
      else
        file_stat =
          if FindSettings.need_size?(finder.settings) or FindSettings.need_last_mod?(finder.settings) do
            File.stat!(file_path, [time: :posix])
          else
            nil
          end
        {file_size, last_mod} =
          if file_stat != nil do
            {file_stat.size, file_stat.mtime}
          else
            {0, 0}
          end
        if not matching_file_size?(finder, file_size) or not matching_last_mod?(finder, last_mod) do
          []
        else
          [FileResult.new(Path.dirname(file_path), Path.basename(file_path), file_type, file_size, last_mod)]
        end
      end
    end
  end

  def filter_to_file_results(finder, file_path) do
    # IO.puts("filter_to_file_results(#{file_path})")
    if not matching_dir?(finder, Path.dirname(file_path)) or not matching_file_name_by_hidden?(finder, Path.basename(file_path)) do
      []
    else
      case FileTypes.get_file_type_for_file_name(finder.file_types, Path.basename(file_path)) do
        :archive -> filter_archive_file_path_to_file_results(finder, file_path)
        file_type -> filter_regular_file_path_to_file_results(finder, file_path, file_type)
      end
    end
  end

  def rec_find_path(finder, path, min_depth, max_depth, current_depth) do
    # IO.puts("rec_find_path(#{path})")
    if max_depth > -1 and current_depth > max_depth do
      []
    else
      {dirs, files} = File.ls!(path)
                      |> Enum.map(fn f -> Path.join(path, f) end)
                      |> Enum.filter(fn f -> File.regular?(f) or File.dir?(f) end)
                      |> Enum.split_with(fn f -> File.dir?(f) end)
      recurse = max_depth == -1 or current_depth < max_depth
      dirs = if recurse, do: dirs, else: []
      dirs = Enum.filter(dirs, fn d -> finder.settings.follow_symlinks or not FileUtil.symlink?(d) end)
             |> Enum.filter(fn d -> traversable_dir?(finder, d) end)

      files = Enum.filter(files, fn f -> finder.settings.follow_symlinks or not FileUtil.symlink?(f) end)

      file_results = if min_depth < 0 or current_depth >= min_depth do
        Enum.map(files, fn f -> filter_to_file_results(finder, f) end) |> List.flatten()
      else
        []
      end
      dir_results = dirs
                    |> Enum.map(fn d -> rec_find_path(finder, d, min_depth, max_depth, current_depth + 1) end)
                    |> List.flatten()
      file_results ++ dir_results
    end
  end

  def find_path!(finder, path) do
    # IO.puts("find_path!(#{path})")
    p = if File.exists?(path) do
      path
    else
      FileUtil.expand_path(path)
    end

    cond do
      not File.exists?(p) -> raise FindError, message: FindError.startpath_not_match_find_settings
      File.dir?(p) ->
        if not finder.settings.follow_symlinks and FileUtil.symlink?(p) do
          raise FindError, message: FindError.startpath_not_match_find_settings
        end
        if finder.settings.max_depth == 0 do
          []
        else
          if traversable_dir?(finder, p) do
            max_depth = if finder.settings.recursive, do: finder.settings.max_depth, else: 1
            rec_find_path(finder, p, finder.settings.min_depth, max_depth, 1)
          else
            raise FindError, message: FindError.startpath_not_match_find_settings
          end
        end
      File.regular?(p) ->
        if not finder.settings.follow_symlinks and FileUtil.symlink?(p) do
          raise FindError, message: FindError.startpath_not_match_find_settings
        end
        if finder.settings.min_depth > 0 do
          []
        else
          case filter_to_file_results(finder, p) do
            [] -> raise FindError, message: FindError.startpath_not_match_find_settings
            file_results -> file_results
          end
        end
      true -> raise FindError, message: FindError.startpath_not_match_find_settings
    end
  end

  def find_path(finder, path) do
    try do
      path_results = find_path!(finder, path)
      {:ok, path_results}
    rescue
      e in FindError -> {:error, e.message}
    end
  end

  # This is the sequential version of find_files
  defp find_files(finder) do
    # IO.puts("find_files()")
    try do
      results = Enum.map(finder.settings.paths, fn path -> find_path!(finder, path) end)
                |> List.flatten()
      {:ok, FileResultSorter.sort(finder.settings, results)}
    rescue
      e in FindError -> {:error, e.message}
    end
  end

  # This is the asynchronous version of find_files
  defp find_files_async(finder) do
    # IO.puts("find_files_async()")
    try do
      tasks = Enum.map(finder.settings.paths, fn p -> Task.async(fn -> send(self(), find_path!(finder, p)) end) end)
      # # IO.puts("tasks: #{inspect(tasks)})")
      results = Task.await_many(tasks, 1000) |> List.flatten()
      {:ok, FileResultSorter.sort(finder.settings, results)}
    rescue
      e in FindError -> {:error, e.message}
    end
  end

  def find(finder) do
    case validate_settings(finder) do
      {:error, message} -> {:error, message}
      {:ok, _} -> if Enum.count(finder.settings.paths) > 1, do: find_files_async(finder), else: find_files(finder)
    end
  end

  def find!(finder) do
    case find(finder) do
      {:error, message} -> raise FindError, message: message
      {:ok, results} -> results
    end
  end

  # defp readable?(path) do
  #   case File.stat(path) do
  #     {:ok, stat} -> stat.mode && 0o400 != 0
  #     _ -> false
  #   end
  # end

  def validate_settings(finder) do
    if finder.settings.paths == nil or Enum.empty?(finder.settings.paths) do
      {:error, FindError.startpath_not_defined}
    else
      expanded_paths = Enum.map(finder.settings.paths, fn p -> FileUtil.expand_path(p) end)
      cond do
        Enum.any?(expanded_paths, fn p -> !File.exists?(p) end) ->
          {:error, FindError.startpath_not_found}
        # Enum.any?(expanded_paths, fn p -> !readable?(p) end) ->
        #   {:error, "Startpath not readable"}
        not finder.settings.follow_symlinks && Enum.any?(expanded_paths, fn p -> FileUtil.symlink?(p) end) ->
          {:error, FindError.startpath_not_match_find_settings}
        Enum.any?(expanded_paths, fn p -> File.dir?(p) && !traversable_dir?(finder, p) end) ->
          {:error, FindError.startpath_not_match_find_settings}
        Enum.any?(expanded_paths, fn p -> !File.dir?(p) && filter_to_file_results(finder, p) == [] end) ->
          {:error, FindError.startpath_not_match_find_settings}
        finder.settings.min_depth > -1 and finder.settings.max_depth > -1
        and finder.settings.min_depth > finder.settings.max_depth ->
          {:error, FindError.invalid_range_mindepth_and_maxdepth}
        finder.settings.min_size > 0 and finder.settings.max_size > 0
        and finder.settings.min_size > finder.settings.max_size ->
          {:error, FindError.invalid_range_minsize_and_maxsize}
        finder.settings.min_last_mod != nil and finder.settings.max_last_mod != nil
        and DateTime.after?(finder.settings.min_last_mod, finder.settings.max_last_mod) ->
          {:error, FindError.invalid_range_minlastmod_and_maxlastmod}
        true -> {:ok, "Settings are valid"}
      end
    end
  end

  def print_dirs(results, formatter) do
    dirs = Enum.map(results, fn r -> r.path end) |> Enum.uniq() |> Enum.sort()
    if dirs == [] do
      Logging.log("\nMatching directories: 0")
    else
      Logging.log("\nMatching directories (#{Enum.count(dirs)}):")
      formatted_dirs = Enum.map(dirs, fn d -> FileResultFormatter.format_path(formatter, d) end)
      Logging.log("#{Enum.join(formatted_dirs, "\n")}")
    end
  end

  def print_files(results, formatter) do
    if results == [] do
      Logging.log("\nMatching files: 0")
    else
      Logging.log("\nMatching files (#{Enum.count(results)}):")
      formatted_files = Enum.map(results, fn r -> FileResultFormatter.format_file_result(formatter, r) end)
      Logging.log("#{Enum.join(formatted_files, "\n")}")
    end
  end
end
