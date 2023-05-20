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

    def test_get_file_type_binary_file
      file_name = 'binary.exe'
      assert_equal(@file_types.get_file_type(file_name), FileType::BINARY)
    end

    def test_get_file_type_code_file
      file_name = 'code.rb'
      assert_equal(@file_types.get_file_type(file_name), FileType::CODE)
    end

    def test_get_file_type_text_file
      file_name = 'text.txt'
      assert_equal(@file_types.get_file_type(file_name), FileType::TEXT)
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
