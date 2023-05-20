################################################################################
#
# test_fileresult.rb
#
# Test the FileResult class
#
################################################################################

require_relative '../lib/rbfind'
require 'minitest/autorun'

module RbFind

  class FileResultTest < Minitest::Test
    def test_file_result_abs_path
      path = ENV['HOME'] + '/src/xfind/ruby/rbfind'
      file_name = 'fileresult.rb'
      file_result = FileResult.new(path, file_name, FileType::CODE, nil)
      assert_equal(ENV['HOME'] + '/src/xfind/ruby/rbfind/fileresult.rb', file_result.relative_path)
    end

    def test_file_result_rel_path1
      path = '.'
      file_name = 'fileresult.rb'
      file_result = FileResult.new(path, file_name, FileType::CODE, nil)
      assert_equal('./fileresult.rb', file_result.relative_path)
    end

    def test_file_result_rel_path2
      path = './'
      file_name = 'fileresult.rb'
      file_result = FileResult.new(path, file_name, FileType::CODE, nil)
      assert_equal('./fileresult.rb', file_result.relative_path)
    end

    def test_file_result_rel_path3
      path = '..'
      file_name = 'fileresult.rb'
      file_result = FileResult.new(path, file_name, FileType::CODE, nil)
      assert_equal('../fileresult.rb', file_result.relative_path)
    end
  end
end
