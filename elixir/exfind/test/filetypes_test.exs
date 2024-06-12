defmodule ExFindTest.FileTypesTest do
  alias ExFind.FileTypes
  use ExUnit.Case
  doctest ExFind.FileTypes

  test "file with archive extension is :archive type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "archive.zip") == :archive
  end

  test "file with audio extension is :audio type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "music.mp3") == :audio
  end

  test "file with binary extension is :binary type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "binary.exe") == :binary
  end

  test "file with code extension is :code type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "code.ex") == :code
  end

  test "file with font extension is :font type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "font.ttf") == :font
  end

  test "file with image extension is :image type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "image.png") == :image
  end

  test "file with text extension is :text type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "text.txt") == :text
  end

  test "file with video extension is :video type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "video.mp4") == :video
  end

  test "file with xml extension is :xml type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "pom.xml") == :xml
  end

  test "file with unknown extension is :unknown type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "unknown.xyz") == :unknown
  end

  test "file CMakeLists.txt is :code type" do
    file_types = ExFind.FileTypes.new()
    assert FileTypes.get_file_type_for_file_name(file_types, "CMakeLists.txt") == :code
  end
end
