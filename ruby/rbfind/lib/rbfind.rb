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
require_relative 'rbfind/filetypes'
require_relative 'rbfind/fileutil'
require_relative 'rbfind/finder'
require_relative 'rbfind/findfile'
require_relative 'rbfind/findoption'
require_relative 'rbfind/findoptions'
require_relative 'rbfind/findresult'
require_relative 'rbfind/findsettings'

def main
  options = RbFind::FindOptions.new

  settings =
    begin
      options.find_settings_from_args(ARGV)
    rescue RbFind::FindError => e
      handle_error(e, options)
    end

  RbFind::log("settings: #{settings}") if settings.debug

  if settings.printusage
    RbFind::log("\n")
    options.usage
  end

  if settings.printversion
    RbFind::log("Version: #{RbFind::VERSION}")
    abort
  end

  find(options, settings)
end

def handle_error(err, options)
  RbFind::log("\nERROR: #{err.message}\n\n")
  options.usage
end

def find(options, settings)
  finder = RbFind::Finder.new(settings)
  finder.find

  # print the results
  if settings.printresults
    RbFind::log("\n")
    finder.print_results
  end

  if settings.listdirs
    RbFind::log("\n")
    dirs = finder.get_matching_dirs
    RbFind::log("Directories with matches (#{dirs.size}):")
    dirs.each do |d|
      RbFind::log("#{d}\n")
    end
  end

  if settings.listfiles
    RbFind::log("\n")
    files = finder.get_matching_files
    RbFind::log("Files with matches (#{files.size}):")
    files.each do |f|
      RbFind::log("#{f}\n")
    end
  end

  if settings.listlines
    RbFind::log("\n")
    lines = finder.get_matching_lines
    hdr_text =
      if settings.uniquelines
        'Unique lines with matches'
      else
        'Lines with matches'
      end
    RbFind::log("#{hdr_text} (#{lines.size}):")
    lines.each do |line|
      RbFind::log("#{line}\n")
    end
  end

rescue RbFind::FindError => e
  handle_error(e, options)

rescue RuntimeError => e
  handle_error(e, options)
end
