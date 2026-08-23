defmodule ExFind.App do
  @moduledoc """
  Documentation for `ExFind.App`.
  """

  alias ExFind.FileResultFormatter
  alias ExFind.FindConfig
  alias ExFind.Finder
  alias ExFind.FindError
  alias ExFind.FindOptions
  alias ExFind.Logging

  def handle_error(message, colorize, find_options) do
    Logging.log_error("\nERROR: #{message}", colorize)
    FindOptions.usage(find_options)
  end

  def handle_results(results, settings) do
    formatter = FileResultFormatter.new(settings)
    if settings.print_dirs do
      Finder.print_dirs(results, formatter)
    end
    if settings.print_files do
      Finder.print_files(results, formatter)
    end
  end

  def find(config, settings, find_options) do
    if settings.debug do
      Logging.log("\nsettings: #{inspect(settings)}\n")
    end

    if settings.print_usage or settings.print_version do
      if settings.print_usage do
        FindOptions.usage(find_options)
      else
        Logging.log("\nExFind version: #{config.version()}")
      end
    else
      finder = Finder.new(config, settings)
      case Finder.find(finder) do
        {:error, message} -> handle_error(message, settings.colorize, find_options)
        {:ok, results} -> handle_results(results, settings)
      end
    end
  end

  def main(args) do
    config = FindConfig.new()
    find_options = FindOptions.new(config)
    try do
      case FindOptions.get_settings_from_args(find_options, args) do
        {:error, message} -> handle_error(message, true, find_options)
        {:ok, settings} -> find(config, settings, find_options)
      end
    rescue
      e in FindError -> handle_error(e.message, true, find_options)
    end
  end
end
