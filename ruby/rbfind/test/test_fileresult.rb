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
    def test_fileresult_abs_path
      path = ENV['HOME'] + '/src/xfind/ruby/rbfind'
      filename = 'fileresult.rb'
      fileresult = FileResult.new(path, filename, FileType::CODE)
      assert_equal(ENV['HOME'] + '/src/xfind/ruby/rbfind/fileresult.rb', fileresult.relativepath)
    end

    def test_fileresult_rel_path1
      path = '.'
      filename = 'fileresult.rb'
      fileresult = FileResult.new(path, filename, FileType::CODE)
      assert_equal('./fileresult.rb', fileresult.relativepath)
    end

    def test_fileresult_rel_path2
      path = './'
      filename = 'fileresult.rb'
      fileresult = FileResult.new(path, filename, FileType::CODE)
      assert_equal('./fileresult.rb', fileresult.relativepath)
    end

    def test_fileresult_rel_path3
      path = '..'
      filename = 'fileresult.rb'
      fileresult = FileResult.new(path, filename, FileType::CODE)
      assert_equal('../fileresult.rb', fileresult.relativepath)
    end
  end
end
