defmodule ExFindTest.FileUtilTest do
  alias ExFind.FileUtil
  use ExUnit.Case
  doctest ExFind.FileUtil

  ############################################################################
  # get_extension tests
  ############################################################################
  test "file extension matches extension" do
    file_name = "filename.txt"
    assert FileUtil.get_extension(file_name) == "txt"
  end

  test "file with missing extension matches empty extension" do
    file_name = "filename."
    assert FileUtil.get_extension(file_name) == ""
  end

  test "file with not extension matches empty extension" do
    file_name = "filename"
    assert FileUtil.get_extension(file_name) == ""
  end

  test "hidden file extension matches extension" do
    file_name = ".filename.txt"
    assert FileUtil.get_extension(file_name) == "txt"
  end

  test "hidden file with missing extension matches empty extension" do
    file_name = ".filename."
    assert FileUtil.get_extension(file_name) == ""
  end

  test "hidden file with not extension matches empty extension" do
    file_name = ".filename"
    assert FileUtil.get_extension(file_name) == ""
  end

  ############################################################################
  # dot_dir? tests
  ############################################################################
  test "single dot is dot dir" do
    assert FileUtil.dot_dir?(".")
  end

  test "double dot is dot dir" do
    assert FileUtil.dot_dir?("..")
  end

  test "single dot with slash is dot dir" do
    assert FileUtil.dot_dir?("./")
  end

  test "double dot with slash is dot dir" do
    assert FileUtil.dot_dir?("../")
  end

  test "hidden dir is not dot dir" do
    assert !FileUtil.dot_dir?(".git")
  end

  ############################################################################
  # expand_path tests
  ############################################################################
  test "expand tilde path" do
    assert FileUtil.expand_path("~") == System.user_home()
  end

  test "expand path with tilde" do
    assert FileUtil.expand_path("~/src/xfind") == Path.join([System.user_home(), "src", "xfind"])
  end

  test "expand path with tilde and name" do
    assert FileUtil.expand_path("~cary/src/xfind") == Path.join([System.user_home(), "src", "xfind"])
  end

  ############################################################################
  # hidden? tests
  ############################################################################
  test "hidden file is hidden" do
    assert FileUtil.hidden?(".filename.txt")
  end

  test "non-hidden file is not hidden" do
    assert !FileUtil.hidden?("filename.txt")
  end

  test "single dot is not hidden" do
    assert !FileUtil.hidden?(".")
  end

  test "double dot is not hidden" do
    assert !FileUtil.hidden?("..")
  end
end
