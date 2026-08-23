defmodule ExFind.FindConfig do
  @moduledoc """
  Documentation for `ExFind.FindConfig`.
  """

  @xfind_path System.get_env("XFIND_PATH") || Path.join([System.user_home(), "src", "xfind"])
  @shared_path Path.join([@xfind_path, "shared"])
  @file_types_path Path.join([@shared_path, "filetypes.json"])
  @find_options_path Path.join([@shared_path, "findoptions.json"])
  @default_find_settings_path Path.join([System.user_home(), ".config", "xfind", "settings.json"])

  defstruct xfind_path: "", shared_path: "", file_types_path: "", find_options_path: "", default_find_settings_path: ""

  def new(), do: __struct__([xfind_path: @xfind_path, shared_path: @shared_path, file_types_path: @file_types_path, find_options_path: @find_options_path, default_find_settings_path: @default_find_settings_path])
end
