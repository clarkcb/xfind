################################################################################
#
# test_fileresult.rb
#
# Test the FileResult class
#
################################################################################

require_relative '../lib/rbfind'
require 'test/unit'
require 'pathname'

module RbFind

  class FileResultTest < Test::Unit::TestCase
    def test_file_result_abs_path
      path = Pathname.new(ENV['HOME'] + '/src/xfind/ruby/rbfind/fileresult.rb')
      file_result = FileResult.new(path, FileType::CODE, 0, nil)
      assert_equal(ENV['HOME'] + '/src/xfind/ruby/rbfind/fileresult.rb', file_result.relative_path.to_s)
    end

    def test_file_result_rel_path1
      path = Pathname.new('./fileresult.rb')
      file_result = FileResult.new(path, FileType::CODE, 0, nil)
      assert_equal('./fileresult.rb', file_result.relative_path.to_s)
    end

    def test_file_result_rel_path2
      path = Pathname.new('../fileresult.rb')
      file_result = FileResult.new(path, FileType::CODE, 0, nil)
      assert_equal('../fileresult.rb', file_result.relative_path.to_s)
    end
  end
end
