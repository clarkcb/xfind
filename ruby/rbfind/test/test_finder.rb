################################################################################
#
# test_finder.rb
#
# Finder testing
#
################################################################################

require_relative '../lib/rbfind'
require 'minitest/autorun'

module RbFind

  class FinderTest < Minitest::Test

    def get_settings
      settings = FindSettings.new
      settings.paths.add('.')
      settings
    end

    def get_test_file
      # File.expand_path("#{SHAREDPATH}/testFiles/testFile2.txt")
      File.join(File.dirname(__FILE__), "fixtures/testFile2.txt")
    end

    ################################################################################
    # is_find_dir tests
    ################################################################################
    def test_is_find_dir_no_patterns
      settings = get_settings
      finder = Finder.new(settings)
      dir = 'plfind'
      assert(finder.matching_dir?(dir))
    end

    def test_is_find_dir_matches_in_pattern
      settings = get_settings
      settings.add_pattern('plfind', settings.in_dir_patterns)
      finder = Finder.new(settings)
      dir = 'plfind'
      assert(finder.matching_dir?(dir))
    end

    def test_is_find_dir_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('plfind', settings.in_dir_patterns)
      finder = Finder.new(settings)
      dir = 'pyfind'
      assert(!finder.matching_dir?(dir))
    end

    def test_is_find_dir_matches_out_pattern
      settings = get_settings
      settings.add_pattern('pyfind', settings.out_dir_patterns)
      finder = Finder.new(settings)
      dir = 'pyfind'
      assert(!finder.matching_dir?(dir))
    end

    def test_is_find_dir_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('pyfind', settings.out_dir_patterns)
      finder = Finder.new(settings)
      dir = 'plfind'
      assert(finder.matching_dir?(dir))
    end

    def test_is_find_dir_single_dot
      settings = get_settings
      finder = Finder.new(settings)
      dir = '.'
      assert(finder.matching_dir?(dir))
    end

    def test_is_find_dir_double_dot
      settings = get_settings
      finder = Finder.new(settings)
      dir = '..'
      assert(finder.matching_dir?(dir))
    end

    def test_is_find_dir_hidden_dir
      settings = get_settings
      finder = Finder.new(settings)
      dir = '.git'
      assert(!finder.matching_dir?(dir))
    end

    def test_is_find_dir_hidden_dir_include_hidden
      settings = get_settings
      settings.include_hidden = true
      finder = Finder.new(settings)
      dir = '.git'
      assert(finder.matching_dir?(dir))
    end

    ################################################################################
    # is_find_file tests
    ################################################################################
    def test_is_find_file_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.matching_file?(f))
    end

    def test_is_find_file_matches_in_extension
      settings = get_settings
      settings.add_exts('rb', settings.in_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.matching_file?(f))
    end

    def test_is_find_file_no_match_in_extension
      settings = get_settings
      settings.add_exts('py', settings.in_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(!finder.matching_file?(f))
    end

    def test_is_find_file_matches_out_extension
      settings = get_settings
      settings.add_exts('rb', settings.out_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(!finder.matching_file?(f))
    end

    def test_is_find_file_no_match_out_extension
      settings = get_settings
      settings.add_exts('py', settings.out_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.matching_file?(f))
    end

    def test_is_find_file_matches_in_pattern
      settings = get_settings
      settings.add_pattern('find', settings.in_file_patterns)
      finder = Finder.new(settings)
      f = 'finder.rb'
      assert(finder.matching_file?(f))
    end

    def test_is_find_file_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('find', settings.in_file_patterns)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(!finder.matching_file?(f))
    end

    def test_is_find_file_matches_out_pattern
      settings = get_settings
      settings.add_pattern('find', settings.out_file_patterns)
      finder = Finder.new(settings)
      f = 'finder.rb'
      assert(!finder.matching_file?(f))
    end

    def test_is_find_file_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('find', settings.out_file_patterns)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.matching_file?(f))
    end

    ################################################################################
    # is__archive_find_file tests
    ################################################################################
    def test_is_archive_find_file_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.matching_archive_file?(f))
    end

    def test_is_archive_find_file_matches_in_extension
      settings = get_settings
      settings.add_exts('zip', settings.in_archive_extensions)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.matching_archive_file?(f))
    end

    def test_is_archive_find_file_no_match_in_extension
      settings = get_settings
      settings.add_exts('gz', settings.in_archive_extensions)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.matching_archive_file?(f))
    end

    def test_is_archive_find_file_matches_out_extension
      settings = get_settings
      settings.add_exts('zip', settings.out_archive_extensions)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.matching_archive_file?(f))
    end

    def test_is_archive_find_file_no_match_out_extension
      settings = get_settings
      settings.add_exts('gz', settings.out_archive_extensions)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.matching_archive_file?(f))
    end

    def test_is_archive_find_file_matches_in_pattern
      settings = get_settings
      settings.add_pattern('arch', settings.in_archive_file_patterns)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.matching_archive_file?(f))
    end

    def test_is_archive_find_file_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('archives', settings.in_archive_file_patterns)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.matching_archive_file?(f))
    end

    def test_is_archive_find_file_matches_out_pattern
      settings = get_settings
      settings.add_pattern('arch', settings.out_archive_file_patterns)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.matching_archive_file?(f))
    end

    def test_is_archive_find_file_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('archives', settings.out_archive_file_patterns)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.matching_archive_file?(f))
    end

    ################################################################################
    # filter_to_file_result tests
    ################################################################################
    def test_filter_to_file_result_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_is_find_file
      settings = get_settings
      settings.add_exts('rb', settings.in_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_not_is_find_file
      settings = get_settings
      settings.add_exts('pl', settings.in_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.filter_to_file_result(f) == nil)
    end

    def test_filter_to_file_result_is_hidden_file
      settings = get_settings
      finder = Finder.new(settings)
      f = '.gitignore'
      assert(finder.filter_to_file_result(f) == nil)
    end

    def test_filter_to_file_result_hidden_include_hidden
      settings = get_settings
      settings.include_hidden = true
      finder = Finder.new(settings)
      f = '.gitignore'
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_archive_no_include_archives
      settings = get_settings
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.filter_to_file_result(f) == nil)
    end

    def test_filter_to_file_result_archive_include_archives
      settings = get_settings
      settings.include_archives = true
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_archive_archives_only
      settings = get_settings
      settings.archives_only = true
      settings.include_archives = true
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_nonarchive_archives_only
      settings = get_settings
      settings.archives_only = true
      settings.include_archives = true
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.filter_to_file_result(f) == nil)
    end
  end
end
