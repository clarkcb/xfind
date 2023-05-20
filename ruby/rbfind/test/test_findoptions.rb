################################################################################
#
# test_findoptions.rb
#
# Test the FindOptions class
#
################################################################################

require_relative '../lib/rbfind'
require 'minitest/autorun'

module RbFind

  class FindOptionsTest < Minitest::Test
    def setup
      @findoptions = RbFind::FindOptions.new
    end

    def test_no_args
      settings = @findoptions.find_settings_from_args([])
      assert_equal(false, settings.archives_only)
      assert_equal(false, settings.debug)
      assert_equal(true, settings.exclude_hidden)
      assert_equal(false, settings.include_archives)
      assert_equal(false, settings.list_dirs)
      assert_equal(true, settings.list_files)
      assert_equal(false, settings.print_usage)
      assert_equal(false, settings.print_version)
      assert_equal(true, settings.recursive)
      assert_equal(false, settings.verbose)
      assert(settings.in_archive_extensions.empty?)
      assert(settings.in_archive_file_patterns.empty?)
      assert(settings.in_dir_patterns.empty?)
      assert(settings.in_file_patterns.empty?)
      assert(settings.out_archive_extensions.empty?)
      assert(settings.out_archive_file_patterns.empty?)
      assert(settings.out_dir_patterns.empty?)
      assert(settings.out_file_patterns.empty?)
      assert(settings.paths.empty?)
    end

    def test_valid_args
      args = %w[-x py,rb .]
      settings = @findoptions.find_settings_from_args(args)
      assert_equal(1, settings.paths.length)
      assert_equal('.', settings.paths.first)
      assert_equal(2, settings.in_extensions.length)
      assert(settings.in_extensions.include?('py'))
      assert(settings.in_extensions.include?('rb'))
    end

    def test_archives_only_arg
      args = ['--archivesonly']
      settings = @findoptions.find_settings_from_args(args)
      assert(settings.archives_only)
      assert(settings.include_archives)
    end

    def test_debug_arg
      args = ['--debug']
      settings = @findoptions.find_settings_from_args(args)
      assert(settings.debug)
      assert(settings.verbose)
    end

    def test_missing_arg
      args = %w[-x py,rb . -D]
      assert_raises(FindError) { _settings = @findoptions.find_settings_from_args(args) }
    end

    def test_invalid_arg
      args = %w[-x py,rb . -Q]
      assert_raises(FindError) { _settings = @findoptions.find_settings_from_args(args) }
    end

    def test_settings_from_json
      settings = FindSettings.new
      json = <<~JSON
      {
        "path": "~/src/xfind/",
        "in-ext": ["js","ts"],
        "out-dirpattern": "node_module",
        "out-filepattern": ["temp"],
        "debug": true,
        "includehidden": true
      }
      JSON
      @findoptions.settings_from_json(json, settings)
      assert_equal(1, settings.paths.length)
      assert_equal('~/src/xfind/', settings.paths.first)
      assert_equal(2, settings.in_extensions.length)
      assert(settings.in_extensions.include?('js'))
      assert(settings.in_extensions.include?('ts'))
      assert_equal(1, settings.out_dir_patterns.length)
      assert_equal('node_module', settings.out_dir_patterns.first.source)
      assert_equal(1, settings.out_file_patterns.length)
      assert_equal('temp', settings.out_file_patterns.first.source)
      assert(settings.debug)
      assert(settings.verbose)
    end
  end
end
