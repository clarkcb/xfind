################################################################################
#
# test_filetypes.rb
#
# Test the FileTypes class
#
################################################################################

require_relative '../lib/rbfind'
require 'minitest/autorun'

module RbFind

  class FileTypesTest < Minitest::Test
    def setup
      @file_types = FileTypes.new
    end

    def test_get_file_type_archive_file
      file_name = 'archive.zip'
      assert_equal(@file_types.get_file_type(file_name), FileType::ARCHIVE)
    end

    def test_get_file_type_audio_file
      file_name = 'music.mp3'
      assert_equal(@file_types.get_file_type(file_name), FileType::AUDIO)
    end

    def test_get_file_type_binary_file
      file_name = 'binary.exe'
      assert_equal(@file_types.get_file_type(file_name), FileType::BINARY)
    end

    def test_get_file_type_code_file
      file_name = 'code.rb'
      assert_equal(@file_types.get_file_type(file_name), FileType::CODE)
    end

    def test_get_file_type_font_file
      file_name = 'font.ttf'
      assert_equal(@file_types.get_file_type(file_name), FileType::FONT)
    end

    def test_get_file_type_image_file
      file_name = 'image.png'
      assert_equal(@file_types.get_file_type(file_name), FileType::IMAGE)
    end

    def test_get_file_type_text_file
      file_name = 'text.txt'
      assert_equal(@file_types.get_file_type(file_name), FileType::TEXT)
    end

    def test_get_file_type_video_file
      file_name = 'movie.mp4'
      assert_equal(@file_types.get_file_type(file_name), FileType::VIDEO)
    end

    def test_get_file_type_xml_file
      file_name = 'markup.xml'
      assert_equal(@file_types.get_file_type(file_name), FileType::XML)
    end

    def test_get_file_type_unknown_file
      file_name = 'unknown.xyz'
      assert_equal(@file_types.get_file_type(file_name), FileType::UNKNOWN)
    end
  end
end
