defmodule ExFind.FindError do
  @moduledoc """
  Documentation for `ExFind.FindError`.
  """

  defexception message: "An unknown error occurred."

  @startpath_not_defined "Startpath not defined"

  @startpath_not_found "Startpath not found"

  @startpath_not_readable "Startpath not readable"

  @startpath_not_match_find_settings "Startpath does not match find settings"

  @invalid_range_mindepth_and_maxdepth "Invalid range for mindepth and maxdepth"

  @invalid_range_minsize_and_maxsize "Invalid range for minsize and maxsize"

  @invalid_range_minlastmod_and_maxlastmod "Invalid range for minlastmod and maxlastmod"

  def startpath_not_defined, do: @startpath_not_defined

  def startpath_not_found, do: @startpath_not_found

  def startpath_not_readable, do: @startpath_not_readable

  def startpath_not_match_find_settings, do: @startpath_not_match_find_settings

  def invalid_range_mindepth_and_maxdepth, do: @invalid_range_mindepth_and_maxdepth

  def invalid_range_minsize_and_maxsize, do: @invalid_range_minsize_and_maxsize

  def invalid_range_minlastmod_and_maxlastmod, do: @invalid_range_minlastmod_and_maxlastmod

end
