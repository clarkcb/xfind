#!/usr/bin/env ruby
# frozen_string_literal: true

################################################################################
#
# rbfind.rb
#
# A ruby implementation of xfind
#
################################################################################

require_relative 'rbfind/common'
require_relative 'rbfind/color'
require_relative 'rbfind/console_color'
require_relative 'rbfind/fileresult'
require_relative 'rbfind/fileresultformatter'
require_relative 'rbfind/fileresultsorter'
require_relative 'rbfind/filetypes'
require_relative 'rbfind/fileutil'
require_relative 'rbfind/findconfig'
require_relative 'rbfind/finder'
require_relative 'rbfind/findoption'
require_relative 'rbfind/findoptions'
require_relative 'rbfind/findsettings'

def find_main
  config = RbFind::FindConfig.new
  options = RbFind::FindOptions.new(config)

  settings =
    begin
      options.find_settings_from_args(ARGV)
    rescue RbFind::FindError => e
      handle_find_error(e, true, options)
    end

  RbFind::log("settings: #{settings}") if settings.debug

  if settings.print_usage
    RbFind::log("\n")
    options.usage
  end

  if settings.print_version
    RbFind::log("Version: #{RbFind::VERSION}")
    abort
  end

  find(config, options, settings)
end

def handle_find_error(err, colorize, options)
  RbFind::log('')
  RbFind::log_err("#{err.message}\n", colorize)
  options.usage
end

def find(config, options, settings)
  finder =
    begin
      RbFind::Finder.new(config, settings)
    rescue RbFind::FindError => e
      handle_find_error(e, settings.colorize, options)
    rescue => e
      handle_find_error(e, settings.colorize, options)
    end
  file_results = finder.find
  formatter = RbFind::FileResultFormatter.new(settings)

  if settings.print_dirs
    finder.print_dir_results(file_results, formatter)
  end

  if settings.print_files
    finder.print_file_results(file_results, formatter)
  end

# rescue RbFind::FindError => e
#   handle_find_error(e, options)

rescue RuntimeError => e
  handle_find_error(e, settings.colorize, options)
end
