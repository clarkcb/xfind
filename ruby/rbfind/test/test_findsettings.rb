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

  class FindSettingsTest < Minitest::Test
    def setup
      @settings = FindSettings.new
    end

    def test_default_settings
      assert_equal(false, @settings.archives_only)
      assert_equal(false, @settings.debug)
      assert_equal(false, @settings.include_archives)
      assert_equal(false, @settings.include_hidden)
      assert_equal(false, @settings.print_dirs)
      assert_equal(false, @settings.print_files)
      assert_equal(false, @settings.print_usage)
      assert_equal(false, @settings.print_version)
      assert_equal(true, @settings.recursive)
      assert_equal(false, @settings.verbose)
      assert(@settings.in_archive_extensions.empty?)
      assert(@settings.in_archive_file_patterns.empty?)
      assert(@settings.in_dir_patterns.empty?)
      assert(@settings.in_file_patterns.empty?)
      assert(@settings.out_archive_extensions.empty?)
      assert(@settings.out_archive_file_patterns.empty?)
      assert(@settings.out_dir_patterns.empty?)
      assert(@settings.out_file_patterns.empty?)
      assert(@settings.paths.empty?)
    end

    def test_set_properties
      @settings.archives_only = true
      @settings.debug = true
      assert_equal(true, @settings.archives_only)
      assert_equal(true, @settings.include_archives)
      assert_equal(true, @settings.debug)
      assert_equal(true, @settings.verbose)
    end

    def test_add_single_extension
      @settings.add_exts('rb', @settings.in_extensions)
      self.assert_equal(1, @settings.in_extensions.length)
      self.assert(@settings.in_extensions.include?('rb'))
    end

    def test_add_comma_delimited_extensions
      @settings.add_exts('py,rb', @settings.in_extensions)
      assert_equal(2, @settings.in_extensions.length)
      assert(@settings.in_extensions.include?('py'))
      assert(@settings.in_extensions.include?('rb'))
    end

    def test_add_extensions_as_array
      @settings.add_exts(%w[py rb], @settings.in_extensions)
      assert_equal(2, @settings.in_extensions.length)
      assert(@settings.in_extensions.include?('py'))
      assert(@settings.in_extensions.include?('rb'))
    end

    def test_add_pattern
      @settings.add_pattern('Find', @settings.in_file_patterns)
      assert_equal(1, @settings.in_file_patterns.length)
      assert_equal(@settings.in_file_patterns.first.source, 'Find')
    end

    def test_add_patterns_as_array
      @settings.add_patterns(%w[Find FileTypes], @settings.in_file_patterns)
      assert_equal(2, @settings.in_file_patterns.length)
      assert_equal(@settings.in_file_patterns.first.source, 'Find')
      assert_equal(@settings.in_file_patterns[1].source, 'FileTypes')
    end
  end
end
