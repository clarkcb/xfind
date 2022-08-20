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
      assert_equal(false, settings.archivesonly)
      assert_equal(false, settings.debug)
      assert_equal(true, settings.excludehidden)
      assert_equal(false, settings.includearchives)
      assert_equal(false, settings.listdirs)
      assert_equal(true, settings.listfiles)
      assert_equal(false, settings.printusage)
      assert_equal(false, settings.printversion)
      assert_equal(true, settings.recursive)
      assert_equal(false, settings.verbose)
      assert(settings.in_archiveextensions.empty?)
      assert(settings.in_archivefilepatterns.empty?)
      assert(settings.in_dirpatterns.empty?)
      assert(settings.in_filepatterns.empty?)
      assert(settings.out_archiveextensions.empty?)
      assert(settings.out_archivefilepatterns.empty?)
      assert(settings.out_dirpatterns.empty?)
      assert(settings.out_filepatterns.empty?)
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

    def test_archivesonly_arg
      args = ['--archivesonly']
      settings = @findoptions.find_settings_from_args(args)
      assert(settings.archivesonly)
      assert(settings.includearchives)
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
      assert_equal(1, settings.out_dirpatterns.length)
      assert_equal('node_module', settings.out_dirpatterns.first.source)
      assert_equal(1, settings.out_filepatterns.length)
      assert_equal('temp', settings.out_filepatterns.first.source)
      assert(settings.debug)
      assert(settings.verbose)
    end
  end
end
