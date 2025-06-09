defmodule ExFind.App do
  @moduledoc """
  Documentation for `ExFind.App`.
  """

  alias ExFind.FileResultFormatter
  alias ExFind.Finder
  alias ExFind.FindError
  alias ExFind.FindOptions
  alias ExFind.Logging

  def handle_error(message, find_options) do
    Logging.log_error("\nERROR: #{message}")
    FindOptions.usage(find_options.options)
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

  def handle_results(results, settings) do
    formatter = FileResultFormatter.new(settings)
    if settings.print_dirs do
      print_dirs(results, formatter)
    end
    if settings.print_files do
      print_files(results, formatter)
    end
  end

  def find(settings, find_options) do
    if settings.debug do
      Logging.log("\nsettings: #{inspect(settings)}\n")
    end

    if settings.print_usage or settings.print_version do
      if settings.print_usage do
        FindOptions.usage(find_options.options)
      else
        Logging.log("\nExFind version: #{ExFind.Config.version()}")
      end
    else
      finder = Finder.new(settings)
      case Finder.find(finder) do
        {:error, message} -> handle_error(message, find_options)
        {:ok, results} -> handle_results(results, settings)
      end
    end
  end

  def main(args) do
    find_options = FindOptions.new()
    try do
      case FindOptions.get_settings_from_args(args, find_options.options) do
        {:error, message} -> handle_error(message, find_options)
        {:ok, settings} -> find(settings, find_options)
      end
    rescue
      e in FindError -> handle_error(e.message, find_options)
    end
  end
end
