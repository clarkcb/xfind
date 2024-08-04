defmodule ExFind.Config do
  @moduledoc """
  Documentation for `ExFind.Config`.
  """

  @xfind_path System.get_env("XFIND_PATH") || Path.join([System.user_home(), "src", "xfind"])
  @shared_path Path.join([@xfind_path, "shared"])
  @file_types_path Path.join([@shared_path, "filetypes.json"])
  @find_options_path Path.join([@shared_path, "findoptions.json"])
  @xfind_db_path Path.join([@shared_path, "xfind.db"])

  @version "0.1.0"

  def xfind_path, do: @xfind_path

  def shared_path, do: @shared_path

  def file_types_path, do: @file_types_path

  def find_options_path, do: @find_options_path

  def xfind_db_path, do: @xfind_db_path

  def version, do: @version

end
