defmodule ExFind.Config do
  @moduledoc """
  Documentation for `ExFind.Config`.
  """

  @xfind_path System.get_env("XFIND_PATH") || Path.join([System.user_home(), "src", "xfind"])
  @shared_path Path.join([@xfind_path, "shared"])
  @file_types_path Path.join([@shared_path, "filetypes.json"])
  @find_options_path Path.join([@shared_path, "findoptions.json"])
  @default_find_settings_path Path.join([System.user_home(), ".config", "xfind", "settings.json"])

  @version "0.1.0"

  def xfind_path, do: @xfind_path

  def shared_path, do: @shared_path

  def file_types_path, do: @file_types_path

  def find_options_path, do: @find_options_path

  def default_find_settings_path, do: @default_find_settings_path

  def version, do: @version

end
