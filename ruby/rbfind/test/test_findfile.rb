################################################################################
#
# findfile_test.rb
#
# Test the FindFile class
#
################################################################################

require_relative '../lib/rbfind'
require 'minitest/autorun'

module RbFind

  class FindFileTest < Minitest::Test
    def test_findfile_abs_path
      path = '/Users/cary/src/xfind/ruby/rbfind'
      filename = 'findfile.rb'
      findfile = FindFile.new(path, filename, FileType::CODE)
      assert_equal('/Users/cary/src/xfind/ruby/rbfind/findfile.rb', findfile.relativepath)
    end

    def test_findfile_rel_path1
      path = '.'
      filename = 'findfile.rb'
      findfile = FindFile.new(path, filename, FileType::CODE)
      assert_equal('./findfile.rb', findfile.relativepath)
    end

    def test_findfile_rel_path2
      path = './'
      filename = 'findfile.rb'
      findfile = FindFile.new(path, filename, FileType::CODE)
      assert_equal('./findfile.rb', findfile.relativepath)
    end

    def test_findfile_rel_path3
      path = '..'
      filename = 'findfile.rb'
      findfile = FindFile.new(path, filename, FileType::CODE)
      assert_equal('../findfile.rb', findfile.relativepath)
    end
  end
end
