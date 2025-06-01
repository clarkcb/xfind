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
require_relative 'rbfind/fileresult'
require_relative 'rbfind/fileresultformatter'
require_relative 'rbfind/filetypes'
require_relative 'rbfind/fileutil'
require_relative 'rbfind/finder'
require_relative 'rbfind/findoption'
require_relative 'rbfind/findoptions'
require_relative 'rbfind/findsettings'

def find_main
  options = RbFind::FindOptions.new

  settings =
    begin
      options.find_settings_from_args(ARGV)
    rescue RbFind::FindError => e
      handle_find_error(e, options)
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

  find(options, settings)
end

def handle_find_error(err, options)
  RbFind::log('')
  RbFind::log_err("#{err.message}\n\n")
  options.usage
end

def find(options, settings)
  finder = 
    begin
      RbFind::Finder.new(settings)
    rescue RbFind::FindError => e
      handle_find_error(e, options)
    rescue => e
      handle_find_error(e, options)
    end
  file_results = finder.find
  formatter = RbFind::FileResultFormatter.new(settings)

  if settings.print_dirs
    find_dirs = file_results.map {|fr| fr.path.dirname}.uniq.sort
    if find_dirs.empty?
      RbFind::log("\nMatching directories: 0")
    else
      RbFind::log("\nMatching directories (#{find_dirs.size}):")
      find_dirs.each do |d|
        RbFind::log("#{formatter.format_dir_path(d)}\n")
      end
    end
  end

  if settings.print_files
    if file_results.empty?
      RbFind::log("\nMatching files: 0")
    else
      RbFind::log("\nMatching files (#{file_results.size}):")
      file_results.each do |fr|
        RbFind::log("#{formatter.format_file_result(fr)}\n")
      end
    end
  end

# rescue RbFind::FindError => e
#   handle_find_error(e, options)

rescue RuntimeError => e
  handle_find_error(e, options)
end
