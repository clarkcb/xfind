# frozen_string_literal: true

require 'find'
require 'pathname'
require 'set'
require_relative 'common'
require_relative 'filetypes'
require_relative 'fileutil'
require_relative 'finderror'
require_relative 'findfile'

module RbFind

  # Finder - finds files to find and findes them according to settings
  class Finder
    attr_reader :settings
    attr_reader :results

    def initialize(settings)
      @settings = settings
      validate_settings
      @filetypes = FileTypes.new
      @results = []
    end

    def find_dir?(dirname)
      path_elems = dirname.split(File::SEPARATOR) - FileUtil.dot_dirs
      if @settings.excludehidden && path_elems.any? { |p| FileUtil.hidden?(p) }
        return false
      end
      if !@settings.in_dirpatterns.empty? &&
        !any_matches_any_pattern(path_elems, @settings.in_dirpatterns)
        return false
      end
      if !@settings.out_dirpatterns.empty? &&
        any_matches_any_pattern(path_elems, @settings.out_dirpatterns)
        return false
      end
      true
    end

    def find_file?(filename)
      ext = FileUtil.get_extension(filename)
      if !@settings.in_extensions.empty? &&
        !@settings.in_extensions.include?(ext)
        return false
      end
      if !@settings.out_extensions.empty? &&
        @settings.out_extensions.include?(ext)
        return false
      end
      if !@settings.in_filepatterns.empty? &&
        !matches_any_pattern(filename, @settings.in_filepatterns)
        return false
      end
      if !@settings.out_filepatterns.empty? &&
        matches_any_pattern(filename, @settings.out_filepatterns)
        return false
      end
      filetype = @filetypes.get_filetype(filename)
      if !@settings.in_filetypes.empty? &&
        !@settings.in_filetypes.include?(filetype)
        return false
      end
      if !@settings.out_filetypes.empty? &&
        @settings.out_filetypes.include?(filetype)
        return false
      end
      true
    end

    def archive_find_file?(filepath)
      filename = File.basename(filepath)
      ext = FileUtil.get_extension(filename)
      if !@settings.in_archiveextensions.empty? &&
        !@settings.in_archiveextensions.include?(ext)
        return false
      end
      if !@settings.out_archiveextensions.empty? &&
        @settings.out_archiveextensions.include?(ext)
        return false
      end
      if !@settings.in_archivefilepatterns.empty? &&
        !matches_any_pattern(filename, @settings.in_archivefilepatterns)
        return false
      end
      if !@settings.out_archivefilepatterns.empty? &&
        matches_any_pattern(filename, @settings.out_archivefilepatterns)
        return false
      end
      true
    end

    def filter_file?(filepath)
      filename = File.basename(filepath)
      if @settings.excludehidden && FileUtil.hidden?(filename)
        return false
      end
      if @filetypes.archive_file?(filename)
        return @settings.includearchives && archive_find_file?(filename)
      end
      !@settings.archivesonly && find_file?(filename)
    end

    def find
      findfiles = []
      @settings.paths.each do |p|
        findfiles = findfiles.concat(get_find_files(p))
      end
      findfiles.sort_by(&:relativepath)
      findfiles
    end

    private

    def validate_settings
      raise FindError, 'Startpath not defined' if @settings.paths.empty?
      @settings.paths.each do |p|
        raise FindError, 'Startpath not found' unless Pathname.new(p).exist?
        raise FindError, 'Startpath not readable' unless File.readable?(p)
      end
    end

    def matches_any_pattern(str, pattern_set)
      pattern_set.any? { |p| p.match(str) }
    end

    def any_matches_any_pattern(str_list, pattern_set)
      str_list.each do |s|
        return true if matches_any_pattern(s, pattern_set)
      end
      false
    end

    def file_to_findfile(f)
      d = File.dirname(f) || '.'
      filename = File.basename(f)
      filetype = @filetypes.get_filetype(filename)
      FindFile.new(d, filename, filetype)
    end

    def get_find_files(filepath)
      findfiles = []
      if FileTest.directory?(filepath)
        if @settings.recursive
          Find.find(filepath) do |f|
            if FileTest.directory?(f)
              Find.prune unless find_dir?(f)
            elsif FileTest.file?(f)
              if filter_file?(f)
                findfile = file_to_findfile(f)
                findfiles.push(findfile)
              end
            end
          end
        else
          Find.find(filepath) do |f|
            if FileTest.directory?(f)
              Find.prune
            elsif filter_file?(f)
              findfile = file_to_findfile(f)
              findfiles.push(findfile)
            end
          end
        end
      elsif FileTest.file?(filepath)
        findfile = file_to_findfile(filepath)
        findfiles.push(findfile)
      end
      findfiles
    end

  end
end
