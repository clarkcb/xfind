defmodule ExFindTest.FileResultTest do
  alias ExFind.FileResult
  use ExUnit.Case
  doctest ExFind.FileResult

  test "file_result is valid" do
    file_result = FileResult.new(".", "fileresult_test.exs", :code, 0, 0)
    assert file_result.path == "."
    assert file_result.name == "fileresult_test.exs"
    assert file_result.file_type == :code
    assert file_result.file_size == 0
    assert file_result.last_mod == 0
    assert FileResult.to_string(file_result) == "./fileresult_test.exs"
  end

  test "file_result in container is valid" do
    file_result = FileResult.new(["/some/container/file.zip"], ".", "fileresult_test.exs", :code, 0, 0)
    assert Enum.member?(file_result.containers, "/some/container/file.zip")
    assert file_result.path == "."
    assert file_result.name == "fileresult_test.exs"
    assert file_result.file_type == :code
    assert file_result.file_size == 0
    assert file_result.last_mod == 0
    assert FileResult.to_string(file_result) == "/some/container/file.zip!./fileresult_test.exs"
  end

end
