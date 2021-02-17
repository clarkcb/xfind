################################################################################
#
# finder_test.rb
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
      settings.startpath = '.'
      settings.add_pattern('Finder', settings.findpatterns)
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
      assert(finder.find_dir?(dir))
    end

    def test_is_find_dir_matches_in_pattern
      settings = get_settings
      settings.add_pattern('plfind', settings.in_dirpatterns)
      finder = Finder.new(settings)
      dir = 'plfind'
      assert(finder.find_dir?(dir))
    end

    def test_is_find_dir_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('plfind', settings.in_dirpatterns)
      finder = Finder.new(settings)
      dir = 'pyfind'
      assert(!finder.find_dir?(dir))
    end

    def test_is_find_dir_matches_out_pattern
      settings = get_settings
      settings.add_pattern('pyfind', settings.out_dirpatterns)
      finder = Finder.new(settings)
      dir = 'pyfind'
      assert(!finder.find_dir?(dir))
    end

    def test_is_find_dir_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('pyfind', settings.out_dirpatterns)
      finder = Finder.new(settings)
      dir = 'plfind'
      assert(finder.find_dir?(dir))
    end

    def test_is_find_dir_single_dot
      settings = get_settings
      finder = Finder.new(settings)
      dir = '.'
      assert(finder.find_dir?(dir))
    end

    def test_is_find_dir_double_dot
      settings = get_settings
      finder = Finder.new(settings)
      dir = '..'
      assert(finder.find_dir?(dir))
    end

    def test_is_find_dir_hidden_dir
      settings = get_settings
      finder = Finder.new(settings)
      dir = '.git'
      assert(!finder.find_dir?(dir))
    end

    def test_is_find_dir_hidden_dir_include_hidden
      settings = get_settings
      settings.excludehidden = false
      finder = Finder.new(settings)
      dir = '.git'
      assert(finder.find_dir?(dir))
    end

    ################################################################################
    # is_find_file tests
    ################################################################################
    def test_is_find_file_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.find_file?(f))
    end

    def test_is_find_file_matches_in_extension
      settings = get_settings
      settings.add_exts('rb', settings.in_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.find_file?(f))
    end

    def test_is_find_file_no_match_in_extension
      settings = get_settings
      settings.add_exts('py', settings.in_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(!finder.find_file?(f))
    end

    def test_is_find_file_matches_out_extension
      settings = get_settings
      settings.add_exts('rb', settings.out_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(!finder.find_file?(f))
    end

    def test_is_find_file_no_match_out_extension
      settings = get_settings
      settings.add_exts('py', settings.out_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.find_file?(f))
    end

    def test_is_find_file_matches_in_pattern
      settings = get_settings
      settings.add_pattern('find', settings.in_filepatterns)
      finder = Finder.new(settings)
      f = 'finder.rb'
      assert(finder.find_file?(f))
    end

    def test_is_find_file_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('find', settings.in_filepatterns)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(!finder.find_file?(f))
    end

    def test_is_find_file_matches_out_pattern
      settings = get_settings
      settings.add_pattern('find', settings.out_filepatterns)
      finder = Finder.new(settings)
      f = 'finder.rb'
      assert(!finder.find_file?(f))
    end

    def test_is_find_file_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('find', settings.out_filepatterns)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.find_file?(f))
    end

    ################################################################################
    # is__archive_find_file tests
    ################################################################################
    def test_is_archive_find_file_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.archive_find_file?(f))
    end

    def test_is_archive_find_file_matches_in_extension
      settings = get_settings
      settings.add_exts('zip', settings.in_archiveextensions)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.archive_find_file?(f))
    end

    def test_is_archive_find_file_no_match_in_extension
      settings = get_settings
      settings.add_exts('gz', settings.in_archiveextensions)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.archive_find_file?(f))
    end

    def test_is_archive_find_file_matches_out_extension
      settings = get_settings
      settings.add_exts('zip', settings.out_archiveextensions)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.archive_find_file?(f))
    end

    def test_is_archive_find_file_no_match_out_extension
      settings = get_settings
      settings.add_exts('gz', settings.out_archiveextensions)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.archive_find_file?(f))
    end

    def test_is_archive_find_file_matches_in_pattern
      settings = get_settings
      settings.add_pattern('arch', settings.in_archivefilepatterns)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.archive_find_file?(f))
    end

    def test_is_archive_find_file_no_match_in_pattern
      settings = get_settings
      settings.add_pattern('archives', settings.in_archivefilepatterns)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.archive_find_file?(f))
    end

    def test_is_archive_find_file_matches_out_pattern
      settings = get_settings
      settings.add_pattern('arch', settings.out_archivefilepatterns)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.archive_find_file?(f))
    end

    def test_is_archive_find_file_no_match_out_pattern
      settings = get_settings
      settings.add_pattern('archives', settings.out_archivefilepatterns)
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.archive_find_file?(f))
    end

    ################################################################################
    # filter_file tests
    ################################################################################
    def test_filter_file_matches_by_default
      settings = get_settings
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.filter_file?(f))
    end

    def test_filter_file_is_find_file
      settings = get_settings
      settings.add_exts('rb', settings.in_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(finder.filter_file?(f))
    end

    def test_filter_file_not_is_find_file
      settings = get_settings
      settings.add_exts('pl', settings.in_extensions)
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(!finder.filter_file?(f))
    end

    def test_filter_file_is_hidden_file
      settings = get_settings
      finder = Finder.new(settings)
      f = '.gitignore'
      assert(!finder.filter_file?(f))
    end

    def test_filter_file_hidden_includehidden
      settings = get_settings
      settings.excludehidden = false
      finder = Finder.new(settings)
      f = '.gitignore'
      assert(finder.filter_file?(f))
    end

    def test_filter_file_archive_no_findarchives
      settings = get_settings
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(!finder.filter_file?(f))
    end

    def test_filter_file_archive_findarchives
      settings = get_settings
      settings.findarchives = 1
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.filter_file?(f))
    end

    def test_filter_file_archive_archivesonly
      settings = get_settings
      settings.archivesonly = true
      settings.findarchives = true
      finder = Finder.new(settings)
      f = 'archive.zip'
      assert(finder.filter_file?(f))
    end

    def test_filter_file_nonarchive_archivesonly
      settings = get_settings
      settings.archivesonly = true
      settings.findarchives = true
      finder = Finder.new(settings)
      f = 'fileutil.rb'
      assert(!finder.filter_file?(f))
    end

    ################################################################################
    # find_lines tests
    ################################################################################
    def test_find_lines
      settings = get_settings
      finder = Finder.new(settings)
      testfile = get_test_file
      fo = File.open(testfile, mode: 'r:ISO-8859-1')
      contents = fo.read
      results = finder.find_multiline_string(contents)
      assert_equal(results.size, 2)

      first_result = results[0]
      assert_equal(first_result.linenum, 29)
      assert_equal(first_result.match_start_index, 3)
      assert_equal(first_result.match_end_index, 11)

      second_result = results[1]
      assert_equal(second_result.linenum, 35)
      assert_equal(second_result.match_start_index, 24)
      assert_equal(second_result.match_end_index, 32)
    ensure
      fo&.close
    end

    ################################################################################
    # find_multiline_string tests
    ################################################################################
    def test_find_multiline_string
      settings = get_settings
      finder = Finder.new(settings)
      testfile = get_test_file
      fo = File.open(testfile, mode: 'r:ISO-8859-1')
      line_iterator = fo.each_line
      results = finder.find_line_iterator(line_iterator)
      assert_equal(results.size, 2)

      first_result = results[0]
      assert_equal(first_result.linenum, 29)
      assert_equal(first_result.match_start_index, 3)
      assert_equal(first_result.match_end_index, 11)

      second_result = results[1]
      assert_equal(second_result.linenum, 35)
      assert_equal(second_result.match_start_index, 24)
      assert_equal(second_result.match_end_index, 32)
    ensure
      fo&.close
    end
  end
end
