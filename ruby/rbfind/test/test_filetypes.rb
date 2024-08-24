################################################################################
#
# test_filetypes.rb
#
# Test the FileTypes class
#
################################################################################

require_relative '../lib/rbfind'
require 'test/unit'

module RbFind

  class FileTypesTest < Test::Unit::TestCase
    def setup
      @file_types = FileTypes.new
    end

    def test_get_file_type_archive_file
      file_path = Pathname.new('archive.zip')
      assert_equal(@file_types.get_file_type(file_path), FileType::ARCHIVE)
    end

    def test_get_file_type_audio_file
      file_path = Pathname.new('music.mp3')
      assert_equal(@file_types.get_file_type(file_path), FileType::AUDIO)
    end

    def test_get_file_type_binary_file
      file_path = Pathname.new('binary.exe')
      assert_equal(@file_types.get_file_type(file_path), FileType::BINARY)
    end

    def test_get_file_type_code_file
      file_path = Pathname.new('code.rb')
      assert_equal(@file_types.get_file_type(file_path), FileType::CODE)
    end

    def test_get_file_type_font_file
      file_path = Pathname.new('font.ttf')
      assert_equal(@file_types.get_file_type(file_path), FileType::FONT)
    end

    def test_get_file_type_image_file
      file_path = Pathname.new('image.png')
      assert_equal(@file_types.get_file_type(file_path), FileType::IMAGE)
    end

    def test_get_file_type_text_file
      file_path = Pathname.new('text.txt')
      assert_equal(@file_types.get_file_type(file_path), FileType::TEXT)
    end

    def test_get_file_type_video_file
      file_path = Pathname.new('movie.mp4')
      assert_equal(@file_types.get_file_type(file_path), FileType::VIDEO)
    end

    def test_get_file_type_xml_file
      file_path = Pathname.new('markup.xml')
      assert_equal(@file_types.get_file_type(file_path), FileType::XML)
    end

    def test_get_file_type_unknown_file
      file_path = Pathname.new('unknown.xyz')
      assert_equal(@file_types.get_file_type(file_path), FileType::UNKNOWN)
    end
  end
end
