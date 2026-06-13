################################################################################
#
# test_finder.rb
#
# Finder testing
#
################################################################################

require_relative '../lib/rbfind'
require 'test/unit'
require 'pathname'

module RbFind

  class FinderTest < Test::Unit::TestCase

    def get_settings
      settings = FindSettings.new
      settings.paths.add(Pathname.new('.'))
      settings
    end

    def get_test_file
      # File.expand_path("#{SHAREDPATH}/testFiles/testFile2.txt")
      Pathname.new(File.dirname(__FILE__)).join("fixtures/testFile2.txt")
    end

    ################################################################################
    # matching_dir_path? tests
    ################################################################################
    def test_matching_dir_path_no_patterns
      settings = get_settings
      finder = Finder.new(settings)
      dir = Pathname.new('plfind')
      assert(finder.matching_dir_path?(dir))
    end

    def test_matching_dir_path_matches_in_pattern
      settings = get_settings
      settings.add_pattern('plfind', settings.in_dir_patterns)
      finder = Finder.new(settings)
      dir = Pathname.new('plfind')
      assert(finder.matching_dir_path?(dir))
    end

    def test_matching_dir_path_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('plfind', settings.in_dir_patterns)
      finder = Finder.new(settings)
      dir = Pathname.new('pyfind')
      assert(!finder.matching_dir_path?(dir))
    end

    def test_matching_dir_path_matches_out_pattern
      settings = get_settings
      settings.add_pattern('pyfind', settings.out_dir_patterns)
      finder = Finder.new(settings)
      dir = Pathname.new('pyfind')
      assert(!finder.matching_dir_path?(dir))
    end

    def test_matching_dir_path_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('pyfind', settings.out_dir_patterns)
      finder = Finder.new(settings)
      dir = Pathname.new('plfind')
      assert(finder.matching_dir_path?(dir))
    end

    def test_matching_dir_path_single_dot
      settings = get_settings
      finder = Finder.new(settings)
      dir = Pathname.new('.')
      assert(finder.matching_dir_path?(dir))
    end

    def test_matching_dir_path_double_dot
      settings = get_settings
      finder = Finder.new(settings)
      dir = Pathname.new('..')
      assert(finder.matching_dir_path?(dir))
    end

    def test_matching_dir_path_hidden_dir
      settings = get_settings
      finder = Finder.new(settings)
      dir = Pathname.new('.git')
      assert(!finder.matching_dir_path?(dir))
    end

    def test_matching_dir_path_hidden_dir_include_hidden
      settings = get_settings
      settings.include_hidden = true
      finder = Finder.new(settings)
      dir = Pathname.new('.git')
      assert(finder.matching_dir_path?(dir))
    end

    ################################################################################
    # matching_file_path? tests
    ################################################################################
    def test_matching_file_path_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(finder.matching_file_path?(f))
    end

    def test_matching_file_path_matches_in_extension
      settings = get_settings
      settings.add_exts('rb', settings.in_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(finder.matching_file_path?(f))
    end

    def test_matching_file_path_no_match_in_extension
      settings = get_settings
      settings.add_exts('py', settings.in_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(!finder.matching_file_path?(f))
    end

    def test_matching_file_path_matches_out_extension
      settings = get_settings
      settings.add_exts('rb', settings.out_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(!finder.matching_file_path?(f))
    end

    def test_matching_file_path_no_match_out_extension
      settings = get_settings
      settings.add_exts('py', settings.out_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(finder.matching_file_path?(f))
    end

    def test_matching_file_path_matches_in_pattern
      settings = get_settings
      settings.add_pattern('find', settings.in_file_patterns)
      finder = Finder.new(settings)
      f = Pathname.new('finder.rb')
      assert(finder.matching_file_path?(f))
    end

    def test_matching_file_path_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('find', settings.in_file_patterns)
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(!finder.matching_file_path?(f))
    end

    def test_matching_file_path_matches_out_pattern
      settings = get_settings
      settings.add_pattern('find', settings.out_file_patterns)
      finder = Finder.new(settings)
      f = Pathname.new('finder.rb')
      assert(!finder.matching_file_path?(f))
    end

    def test_matching_file_path_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('find', settings.out_file_patterns)
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(finder.matching_file_path?(f))
    end

    ################################################################################
    # matching_archive_file_path? tests
    ################################################################################
    def test_matching_archive_file_path_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(finder.matching_archive_file_path?(f))
    end

    def test_matching_archive_file_path_matches_in_extension
      settings = get_settings
      settings.add_exts('zip', settings.in_archive_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(finder.matching_archive_file_path?(f))
    end

    def test_matching_archive_file_path_no_match_in_extension
      settings = get_settings
      settings.add_exts('gz', settings.in_archive_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(!finder.matching_archive_file_path?(f))
    end

    def test_matching_archive_file_path_matches_out_extension
      settings = get_settings
      settings.add_exts('zip', settings.out_archive_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(!finder.matching_archive_file_path?(f))
    end

    def test_matching_archive_file_path_no_match_out_extension
      settings = get_settings
      settings.add_exts('gz', settings.out_archive_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(finder.matching_archive_file_path?(f))
    end

    def test_matching_archive_file_path_matches_in_pattern
      settings = get_settings
      settings.add_pattern('arch', settings.in_archive_file_patterns)
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(finder.matching_archive_file_path?(f))
    end

    def test_matching_archive_file_path_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('archives', settings.in_archive_file_patterns)
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(!finder.matching_archive_file_path?(f))
    end

    def test_matching_archive_file_path_matches_out_pattern
      settings = get_settings
      settings.add_pattern('arch', settings.out_archive_file_patterns)
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(!finder.matching_archive_file_path?(f))
    end

    def test_matching_archive_file_path_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('archives', settings.out_archive_file_patterns)
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(finder.matching_archive_file_path?(f))
    end

    ################################################################################
    # filter_to_file_result tests
    ################################################################################
    def test_filter_to_file_result_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_is_find_file
      settings = get_settings
      settings.add_exts('rb', settings.in_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_not_is_find_file
      settings = get_settings
      settings.add_exts('pl', settings.in_extensions)
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(finder.filter_to_file_result(f) == nil)
    end

    def test_filter_to_file_result_is_hidden_file
      settings = get_settings
      finder = Finder.new(settings)
      f = Pathname.new('.gitignore')
      assert(finder.filter_to_file_result(f) == nil)
    end

    def test_filter_to_file_result_hidden_include_hidden
      settings = get_settings
      settings.include_hidden = true
      finder = Finder.new(settings)
      f = Pathname.new('.gitignore')
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_archive_no_include_archives
      settings = get_settings
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(finder.filter_to_file_result(f) == nil)
    end

    def test_filter_to_file_result_archive_include_archives
      settings = get_settings
      settings.include_archives = true
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_archive_archives_only
      settings = get_settings
      settings.archives_only = true
      settings.include_archives = true
      finder = Finder.new(settings)
      f = Pathname.new('archive.zip')
      assert(finder.filter_to_file_result(f) != nil)
    end

    def test_filter_to_file_result_nonarchive_archives_only
      settings = get_settings
      settings.archives_only = true
      settings.include_archives = true
      finder = Finder.new(settings)
      f = Pathname.new('fileutil.rb')
      assert(finder.filter_to_file_result(f) == nil)
    end

    ################################################################################
    # test filtering symlink files
    ################################################################################
    def test_default_no_symlinks
      settings = FindSettings.new
      bin_path = File.join(File.dirname(__FILE__), "../../../bin")
      settings.paths.add(Pathname.new(bin_path))
      finder = Finder.new(settings)
      file_results = finder.find
      assert(file_results.length == 0 || file_results.length < 4)
    end

    def test_follow_symlinks
      settings = FindSettings.new
      bin_path = File.join(File.dirname(__FILE__), "../../../bin")
      settings.paths.add(Pathname.new(bin_path))
      settings.follow_symlinks = true
      finder = Finder.new(settings)
      file_results = finder.find
      assert(file_results.length == 0 || file_results.length > 2)
    end

    def test_no_follow_symlinks
      settings = FindSettings.new
      bin_path = File.join(File.dirname(__FILE__), "../../../bin")
      settings.paths.add(Pathname.new(bin_path))
      settings.follow_symlinks = false
      finder = Finder.new(settings)
      file_results = finder.find
      assert(file_results.length == 0 || file_results.length < 4)
    end
  end
end
