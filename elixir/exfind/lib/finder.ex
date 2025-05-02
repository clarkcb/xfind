defmodule ExFind.Finder do
  @moduledoc """
  Documentation for `ExFind.Finder`.
  """

  alias ExFind.FileResult
  alias ExFind.FileTypes
  alias ExFind.FileUtil
  alias ExFind.FindError
  alias ExFind.FindSettings
  alias ExFind.StringUtil

  defstruct [:file_types, :settings]

  # def new(args), do: __struct__(args)
  def new(settings) do
    file_types = ExFind.FileTypes.new()
    __struct__([file_types: file_types, settings: settings])
  end

  def matching_dir?(finder, dir) do
    # IO.puts("matching_dir?(#{dir})")
    (finder.settings.include_hidden or not FileUtil.hidden?(dir))
    and (Enum.empty?(finder.settings.in_dir_patterns)
         or StringUtil.any_matches_any_pattern(Path.split(dir), finder.settings.in_dir_patterns))
    and (Enum.empty?(finder.settings.out_dir_patterns)
         or !StringUtil.any_matches_any_pattern(Path.split(dir), finder.settings.out_dir_patterns))
  end

  defp matching_extension?(ext, in_extensions, out_extensions) do
    # IO.puts("matching_extension?(#{ext})")
    case {in_extensions, out_extensions} do
      {[], []} -> true
      {in_exts, []} -> Enum.any?(in_exts, fn e -> e == ext end)
      {[], out_exts} -> !Enum.any?(out_exts, fn e -> e == ext end)
      {in_exts, out_exts} ->
        Enum.any?(in_exts, fn e -> e == ext end)
        and !Enum.any?(out_exts, fn e -> e == ext end)
    end
  end

  def has_matching_archive_extension?(finder, file_name) do
    # IO.puts("has_matching_archive_extension?(#{file_name})")
    case {finder.settings.in_archive_extensions, finder.settings.out_archive_extensions} do
      {[], []} -> true
      {in_exts, out_exts} ->
        ext = FileUtil.get_extension(file_name)
        matching_extension?(ext, in_exts, out_exts)
    end
  end

  def has_matching_extension?(finder, file_name) do
    # IO.puts("has_matching_extension?(#{file_name})")
    case {finder.settings.in_extensions, finder.settings.out_extensions} do
      {[], []} -> true
      {in_exts, out_exts} ->
        ext = FileUtil.get_extension(file_name)
        matching_extension?(ext, in_exts, out_exts)
    end
  end

  defp matching_file_name?(file_name, in_file_patterns, out_file_patterns) do
    # IO.puts("_matching_file_name?(#{file_name})")
    (Enum.empty?(in_file_patterns)
     or Enum.any?(in_file_patterns, fn p -> Regex.match?(p, file_name) end))
    and (Enum.empty?(out_file_patterns)
         or !Enum.any?(out_file_patterns, fn p -> Regex.match?(p, file_name) end))
  end

  def matching_archive_file_name?(finder, file_name) do
    # IO.puts("matching_file_name?(#{file_name})")
    case {finder.settings.in_archive_file_patterns, finder.settings.out_archive_file_patterns} do
      {[], []} -> true
      {in_patterns, out_patterns} ->
        matching_file_name?(file_name, in_patterns, out_patterns)
    end
  end

  def matching_file_name?(finder, file_name) do
    # IO.puts("matching_file_name?(#{file_name})")
    case {finder.settings.in_file_patterns, finder.settings.out_file_patterns} do
      {[], []} -> true
      {in_patterns, out_patterns} ->
        matching_file_name?(file_name, in_patterns, out_patterns)
    end
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

  def matching_archive_file?(finder, file_path, _file_size, _last_mod) do
    # IO.puts("matching_archive_file?(#{file_path})")
    file_name = Path.basename(file_path)
    (finder.settings.include_hidden or not FileUtil.hidden?(Path.basename(file_name)))
    and has_matching_archive_extension?(finder, file_name)
    and matching_archive_file_name?(finder, file_name)
  end

  def matching_file?(finder, file_path, file_type, file_size, last_mod) do
    # IO.puts("matching_file?(#{file_path})")
    file_name = Path.basename(file_path)
    (finder.settings.include_hidden or not FileUtil.hidden?(Path.basename(file_name)))
    and has_matching_extension?(finder, file_name)
    and matching_file_name?(finder, file_name)
    and matching_file_type?(finder, file_type)
    and matching_file_size?(finder, file_size)
    and matching_last_mod?(finder, last_mod)
  end

  def filter_to_file_results(finder, file_path) do
    # IO.puts("filter_to_file_results(#{file_path})")
    if FileUtil.symlink?(file_path) do
      if finder.settings.follow_symlinks do
        case filter_to_file_results(finder, FileUtil.get_symlink_target(file_path)) do
          [result] -> [FileResult.new(Path.dirname(file_path), Path.basename(file_path), result.file_type, result.file_size, result.last_mod)]
          _ -> []
        end
      else
        []
      end
    else
      if not finder.settings.include_hidden and FileUtil.hidden?(Path.basename(file_path)) do
        []
      else
        file_type = FileTypes.get_file_type_for_file_name(finder.file_types, Path.basename(file_path))
        if file_type == :archive and not finder.settings.include_archives and not finder.settings.archives_only do
          []
        else
          {file_size, last_mod} =
            if FindSettings.need_size?(finder.settings) or FindSettings.need_last_mod?(finder.settings) do
              file_stat = File.stat!(file_path, [time: :posix])
              {file_stat.size, file_stat.mtime}
            else
              {0, 0}
            end
          if file_type == :archive do
            if matching_archive_file?(finder, file_path, file_size, last_mod) do
              [FileResult.new(Path.dirname(file_path), Path.basename(file_path), file_type, file_size, last_mod)]
            else
              []
            end
          else
            if matching_file?(finder, file_path, file_type, file_size, last_mod) do
              [FileResult.new(Path.dirname(file_path), Path.basename(file_path), file_type, file_size, last_mod)]
            else
              []
            end
          end
        end
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
      file_results = if min_depth < 0 or current_depth >= min_depth do
        Enum.map(files, fn f -> filter_to_file_results(finder, f) end) |> List.flatten()
      else
        []
      end
      # dir_results = if Enum.empty?(dirs), do: [], else: process_dirs(finder, dirs, current_depth + 1)
      dir_results = dirs
                    |> Enum.filter(fn d -> matching_dir?(finder, d) end)
                    |> Enum.map(fn d -> rec_find_path(finder, d, min_depth, max_depth, current_depth + 1) end)
                    |> List.flatten()
      file_results ++ dir_results
    end
  end

  def find_path(finder, path) do
    # IO.puts("find_path(#{path})")
    p = if File.exists?(path) do
      path
    else
      FileUtil.expand_path(path)
    end
    if File.dir?(p) do
      if finder.settings.max_depth == 0 do
        []
      else
        if matching_dir?(finder, p) do
          max_depth = if finder.settings.recursive, do: finder.settings.max_depth, else: 1
          rec_find_path(finder, p, finder.settings.min_depth, max_depth, 1)
        else
          []
        end
      end
    else
      if finder.settings.min_depth > 0 do
        []
      else
        filter_to_file_results(finder, p)
      end
    end
  end

  # This is the sequential version of find_files
  # defp find_files(finder) do
  #   # IO.puts("find_files()")
  #   results = Enum.map(finder.settings.paths, fn path -> find_path(finder, path) end)
  #             |> List.flatten()
  #   {:ok, sort_results(finder, results)}
  # end

  # This is the asynchronous version of find_files
  defp find_files_async(finder) do
    # IO.puts("find_files_async()")
    tasks = Enum.map(finder.settings.paths, fn p -> Task.async(fn -> send(self(), find_path(finder, p)) end) end)
    # # IO.puts("tasks: #{inspect(tasks)})")
    results = Task.await_many(tasks, 1000) |> List.flatten()
    {:ok, sort_results(finder, results)}
  end

  def find(finder) do
    case validate_settings(finder.settings) do
      {:error, message} -> {:error, message}
      {:ok, _} -> find_files_async(finder)
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

  def validate_settings(settings) do
    cond do
      Enum.empty?(settings.paths) ->
        {:error, "Startpath not defined"}
      Enum.any?(settings.paths, fn p -> !File.exists?(p) && !File.exists?(FileUtil.expand_path(p)) end) ->
        {:error, "Startpath not found"}
      # Enum.any?(settings.paths, fn p -> !readable?(p) end) ->
      #   {:error, "Startpath not readable"}
      settings.min_depth > -1 and settings.max_depth > -1
      and settings.min_depth > settings.max_depth ->
        {:error, "Invalid range for mindepth and maxdepth"}
      settings.min_size > 0 and settings.max_size > 0
      and settings.min_size > settings.max_size ->
        {:error, "Invalid range for minsize and maxsize"}
      settings.min_last_mod != nil and settings.max_last_mod != nil
      and DateTime.after?(settings.min_last_mod, settings.max_last_mod) ->
        {:error, "Invalid range for minlastmod and maxlastmod"}
      true -> {:ok, "Settings are valid"}
    end
  end

  def get_sort_mapper(finder) do
    if finder.settings.sort_case_insensitive do
      case finder.settings.sort_by do
        :file_name -> fn r -> {String.downcase(r.name), String.downcase(r.path)} end
        :file_size -> fn r -> {r.file_size, String.downcase(r.path), String.downcase(r.name)} end
        :file_type -> fn r -> {r.file_type, String.downcase(r.path), String.downcase(r.name)} end
        :last_mod  -> fn r -> {r.last_mod, String.downcase(r.path), String.downcase(r.name)} end
        _          -> fn r -> {String.downcase(r.path), String.downcase(r.name)} end
      end
    else
      case finder.settings.sort_by do
        :file_name -> fn r -> {r.name, r.path} end
        :file_size -> fn r -> {r.file_size, r.path, r.name} end
        :file_type -> fn r -> {r.file_type, r.path, r.name} end
        :last_mod  -> fn r -> {r.last_mod, r.path, r.name} end
        _          -> fn r -> {r.path, r.name} end
      end
    end
  end

  def sort_results(finder, results) do
    sort_mapper = get_sort_mapper(finder)
    direction = if finder.settings.sort_descending, do: :desc, else: :asc
    Enum.sort_by(results, sort_mapper, direction)
  end
end
