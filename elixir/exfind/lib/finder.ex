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

  def new(args), do: __struct__(args)

  def matching_dir?(finder, dir) do
    # IO.puts("matching_dir?(#{dir})")
    (finder.settings.include_hidden or not FileUtil.hidden?(dir))
    and (Enum.empty?(finder.settings.in_dir_patterns)
         or StringUtil.any_matches_any_pattern(Path.split(dir), finder.settings.in_dir_patterns))
    and (Enum.empty?(finder.settings.out_dir_patterns)
         or !StringUtil.any_matches_any_pattern(Path.split(dir), finder.settings.out_dir_patterns))
  end

  defp process_dirs(finder, dirs, depth) do
    # IO.puts("process_dirs(#{inspect(dirs)})")
    tasks = Enum.map(dirs, fn d -> Task.async(fn -> find_dir(finder, d, depth) end) end)
    # IO.puts("tasks: #{inspect(tasks)})")
    Task.await_many(tasks) |> List.flatten()
  end

  defp find_in_dir(finder, dir, depth) do
    # IO.puts("find_in_dir(#{dir})")
    {dirs, files} = File.ls!(dir)
                    |> Enum.map(fn f -> Path.join(dir, f) end)
                    |> Enum.filter(fn f -> File.regular?(f) or File.dir?(f) end)
                    |> Enum.split_with(fn f -> File.dir?(f) end)
    file_results = if depth >= finder.settings.min_depth do
      Enum.map(files, fn f -> find_file(finder, f) end) |> List.flatten()
    else
      []
    end
    # dir_results = Enum.map(dirs, fn d -> find_dir(finder, d, depth + 1) end) |> List.flatten()
    dir_results = if Enum.empty?(dirs), do: [], else: process_dirs(finder, dirs, depth + 1)
    file_results ++ dir_results
  end

  def find_dir(finder, dir, depth) do
    # IO.puts("find_dir(#{dir})")
    cond do
      !finder.settings.recursive and depth > 1 -> []
      finder.settings.max_depth > 0 and depth > finder.settings.max_depth -> []
      !matching_dir?(finder, dir) -> []
      true -> find_in_dir(finder, dir, depth)
    end
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
    # if finder.settings.min_last_mod != nil do
    #   IO.puts("min_last_mod: #{DateTime.to_unix(finder.settings.min_last_mod)}")
    # end
    # if finder.settings.max_last_mod != nil do
    #   IO.puts("max_last_mod: #{DateTime.to_unix(finder.settings.max_last_mod)}")
    # end
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
    file_type = FileTypes.get_file_type_for_file_name(finder.file_types, Path.basename(file_path))
    {file_size, last_mod} =
      if FindSettings.need_size?(finder.settings) or FindSettings.need_last_mod?(finder.settings) do
        file_stat = File.stat!(file_path, [time: :posix])
        {file_stat.size, file_stat.mtime}
      else
        {0, 0}
      end
    if file_type == :archive do
      if finder.settings.include_archives and matching_archive_file?(finder, file_path, file_size, last_mod) do
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

  def find_file(finder, file_path) do
    # IO.puts("find_file(#{file_path})")
    filter_to_file_results(finder, file_path)
  end

  def find_path(finder, path) do
    # IO.puts("find_path(#{path})")
    if File.dir?(path) do
      if finder.settings.max_depth == 0 do
        []
      else
        find_dir(finder, path, 1)
      end
    else
      if finder.settings.min_depth > 0 do
        []
      else
        find_file(finder, path)
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
      Enum.any?(settings.paths, fn p -> !File.exists?(p) end) ->
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

  def sort_results(finder, results) do
    direction = if finder.settings.sort_descending, do: :desc, else: :asc
    if finder.settings.sort_case_insensitive do
      case finder.settings.sort_by do
        :file_name -> Enum.sort_by(results, fn r -> {String.downcase(r.name), String.downcase(r.path)} end, direction)
        :file_size -> Enum.sort_by(results, fn r -> {r.file_size, String.downcase(r.path), String.downcase(r.name)} end, direction)
        :file_type -> Enum.sort_by(results, fn r -> {r.file_type, String.downcase(r.path), String.downcase(r.name)} end, direction)
        :last_mod -> Enum.sort_by(results, fn r -> {r.last_mod, String.downcase(r.path), String.downcase(r.name)} end, direction)
        _ -> Enum.sort_by(results, fn r -> {String.downcase(r.path), String.downcase(r.name)} end, direction)
      end
    else
      case finder.settings.sort_by do
        :file_name -> Enum.sort_by(results, fn r -> {r.name, r.path} end, direction)
        :file_size -> Enum.sort_by(results, fn r -> {r.file_size, r.path, r.name} end, direction)
        :file_type -> Enum.sort_by(results, fn r -> {r.file_type, r.path, r.name} end, direction)
        :last_mod -> Enum.sort_by(results, fn r -> {r.last_mod, r.path, r.name} end, direction)
        _ -> Enum.sort_by(results, fn r -> {r.path, r.name} end, direction)
      end
    end
  end
end
