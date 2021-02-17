################################################################################
#
# findoptions_test.rb
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
      assert_equal(false, settings.firstmatch)
      assert_equal(0, settings.linesafter)
      assert_equal(0, settings.linesbefore)
      assert_equal(false, settings.listdirs)
      assert_equal(false, settings.listfiles)
      assert_equal(false, settings.listlines)
      assert_equal(150, settings.maxlinelength)
      assert_equal(false, settings.multilineoption-REMOVE)
      assert_equal(true, settings.printresults)
      assert_equal(false, settings.printusage)
      assert_equal(false, settings.printversion)
      assert_equal(true, settings.recursive)
      assert_equal(false, settings.findarchives)
      assert_equal(nil, settings.startpath)
      assert_equal(false, settings.uniquelines)
      assert_equal(false, settings.verbose)
      assert(settings.in_archiveextensions.empty?)
      assert(settings.in_archivefilepatterns.empty?)
      assert(settings.in_dirpatterns.empty?)
      assert(settings.in_filepatterns.empty?)
      assert(settings.in_linesafterpatterns.empty?)
      assert(settings.in_linesbeforepatterns.empty?)
      assert(settings.linesaftertopatterns.empty?)
      assert(settings.linesafteruntilpatterns.empty?)
      assert(settings.out_archiveextensions.empty?)
      assert(settings.out_archivefilepatterns.empty?)
      assert(settings.out_dirpatterns.empty?)
      assert(settings.out_filepatterns.empty?)
      assert(settings.out_linesafterpatterns.empty?)
      assert(settings.out_linesbeforepatterns.empty?)
      assert(settings.findpatterns.empty?)
    end

    def test_valid_args
      args = %w[-x py,rb -s Find .]
      settings = @findoptions.find_settings_from_args(args)
      assert_equal('.', settings.startpath)
      assert_equal(2, settings.in_extensions.length)
      assert(settings.in_extensions.include?('py'))
      assert(settings.in_extensions.include?('rb'))
      assert_equal(1, settings.findpatterns.length)
      assert_equal('Find', settings.findpatterns.first.source)
    end

    def test_archivesonly_arg
      args = ['--archivesonly']
      settings = @findoptions.find_settings_from_args(args)
      assert(settings.archivesonly)
      assert(settings.findarchives)
    end

    def test_debug_arg
      args = ['--debug']
      settings = @findoptions.find_settings_from_args(args)
      assert(settings.debug)
      assert(settings.verbose)
    end

    def test_missing_arg
      args = %w[-x py,rb -s Find . -D]
      assert_raises(FindError) { _settings = @findoptions.find_settings_from_args(args) }
    end

    def test_invalid_arg
      args = %w[-x py,rb -s Find . -Q]
      assert_raises(FindError) { _settings = @findoptions.find_settings_from_args(args) }
    end

    def test_settings_from_json
      settings = FindSettings.new
      json = <<~JSON
      {
        "startpath": "~/src/xfind/",
        "in-ext": ["js","ts"],
        "out-dirpattern": "node_module",
        "out-filepattern": ["temp"],
        "findpattern": "Finder",
        "linesbefore": 2,
        "linesafter": 2,
        "debug": true,
        "allmatches": false,
        "includehidden": true
      }
      JSON
      @findoptions.settings_from_json(json, settings)
      assert(settings.debug)
      assert(settings.verbose)
    end
  end
end
