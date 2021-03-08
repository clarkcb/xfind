################################################################################
#
# filetypes_test.rb
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
      assert_equal(false, @settings.archivesonly)
      assert_equal(false, @settings.debug)
      assert_equal(true, @settings.excludehidden)
      assert_equal(false, @settings.includearchives)
      assert_equal(false, @settings.listdirs)
      assert_equal(false, @settings.listfiles)
      assert_equal(false, @settings.printusage)
      assert_equal(false, @settings.printversion)
      assert_equal(true, @settings.recursive)
      assert_equal(false, @settings.verbose)
      assert(@settings.in_archiveextensions.empty?)
      assert(@settings.in_archivefilepatterns.empty?)
      assert(@settings.in_dirpatterns.empty?)
      assert(@settings.in_filepatterns.empty?)
      assert(@settings.out_archiveextensions.empty?)
      assert(@settings.out_archivefilepatterns.empty?)
      assert(@settings.out_dirpatterns.empty?)
      assert(@settings.out_filepatterns.empty?)
      assert(@settings.paths.empty?)
    end

    def test_set_properties
      @settings.archivesonly = true
      @settings.debug = true
      assert_equal(true, @settings.archivesonly)
      assert_equal(true, @settings.includearchives)
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
      @settings.add_pattern('Find', @settings.in_filepatterns)
      assert_equal(1, @settings.in_filepatterns.length)
      assert_equal(@settings.in_filepatterns.first.source, 'Find')
    end

    def test_add_patterns_as_array
      @settings.add_patterns(%w[Find FileTypes], @settings.in_filepatterns)
      assert_equal(2, @settings.in_filepatterns.length)
      assert_equal(@settings.in_filepatterns.first.source, 'Find')
      assert_equal(@settings.in_filepatterns[1].source, 'FileTypes')
    end
  end
end
