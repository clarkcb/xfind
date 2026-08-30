defmodule ExFind.FindConfig do
  @moduledoc """
  Documentation for `ExFind.FindConfig`.
  """

  @default_xfind_config_dir Path.join([System.user_home(), ".config", "xfind"])
  @default_xfind_path Path.join([System.user_home(), "src", "xfind"])
  @shared_name "shared"
  @file_types_name "filetypes.json"
  @find_options_name "findoptions.json"

  def get_xfind_config_dir() do
    System.get_env("XFIND_CONFIG_DIR") || @default_xfind_config_dir
  end

  def get_default_find_settings_path() do
    xfind_config_dir = get_xfind_config_dir()
    Path.join([xfind_config_dir, "settings.json"])
  end

  def get_xfind_path() do
    System.get_env("XFIND_PATH") || @default_xfind_path
  end

  def get_file_types_path() do
    xfind_path = get_xfind_path()
    shared_path = Path.join([xfind_path, @shared_name])
    Path.join([shared_path, @file_types_name])
  end

  def get_find_options_path() do
    xfind_path = get_xfind_path()
    shared_path = Path.join([xfind_path, @shared_name])
    Path.join([shared_path, @find_options_name])
  end

  defstruct file_types_path: "", find_options_path: "", default_find_settings_path: ""

  def new() do
    __struct__([
      file_types_path: get_file_types_path(),
      find_options_path: get_find_options_path(),
      default_find_settings_path: get_default_find_settings_path()
    ])
  end
end
