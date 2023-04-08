# frozen_string_literal: true

require 'find'
require 'pathname'
require 'set'
require_relative 'common'
require_relative 'fileresult'
require_relative 'filetypes'
require_relative 'fileutil'
require_relative 'finderror'

module RbFind

  # Finder - finds files according to settings
  class Finder
    attr_reader :settings
    attr_reader :results

    def initialize(settings)
      @settings = settings
      validate_settings
      @filetypes = FileTypes.new
      @results = []
    end

    def matching_dir?(dirname)
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

    def matching_file?(filepath)
      matching_fileresult?(filepath_to_fileresult(filepath))
    end

    def matching_fileresult?(fileresult)
      if !@settings.in_extensions.empty? || !@settings.out_extensions.empty?
        ext = FileUtil.get_extension(fileresult.filename)
        if !@settings.in_extensions.empty? &&
          !@settings.in_extensions.include?(ext)
          return false
        end
        if !@settings.out_extensions.empty? &&
          @settings.out_extensions.include?(ext)
          return false
        end
      end
      if !@settings.in_filepatterns.empty? &&
        !matches_any_pattern(fileresult.filename, @settings.in_filepatterns)
        return false
      end
      if !@settings.out_filepatterns.empty? &&
        matches_any_pattern(fileresult.filename, @settings.out_filepatterns)
        return false
      end
      if !@settings.in_filetypes.empty? &&
        !@settings.in_filetypes.include?(fileresult.filetype)
        return false
      end
      if !@settings.out_filetypes.empty? &&
        @settings.out_filetypes.include?(fileresult.filetype)
        return false
      end
      if @settings.maxlastmod && fileresult.stat.mtime > @settings.maxlastmod.to_time
        return false
      end
      if @settings.maxsize > 0 && fileresult.stat.size > @settings.maxsize
        return false
      end
      if @settings.minlastmod && fileresult.stat.mtime < @settings.minlastmod.to_time
        return false
      end
      if @settings.minsize > 0 && fileresult.stat.size < @settings.minsize
        return false
      end
      true
    end

    def matching_archive_file?(filepath)
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

    def filter_to_fileresult(filepath)
      filename = File.basename(filepath)
      if @settings.excludehidden && FileUtil.hidden?(filename)
        return nil
      end
      fileresult = filepath_to_fileresult(filepath)
      if fileresult.filetype == FileType::ARCHIVE
        if @settings.includearchives && matching_archive_file?(filename)
          return fileresult
        end
        return nil
      end
      if !@settings.archivesonly && matching_fileresult?(fileresult)
        return fileresult
      end
      nil
    end

    def find
      fileresults = []
      @settings.paths.each do |p|
        fileresults = fileresults.concat(get_file_results(p))
      end
      fileresults = sort_file_results(fileresults)
      if @settings.sort_descending
        fileresults.reverse!
      end
      fileresults
    end

    private

    def sort_file_results(file_results)
      if @settings.sort_caseinsensitive
        if @settings.sortby == SortBy::FILENAME
          file_results.sort_by {|r| [r.filename.downcase, r.path.downcase]}
        elsif @settings.sortby == SortBy::FILESIZE
          file_results.sort_by {|r| [r.stat.size, r.path.downcase, r.filename.downcase]}
        elsif @settings.sortby == SortBy::FILETYPE
          file_results.sort_by {|r| [r.filetype, r.path.downcase, r.filename.downcase]}
        elsif @settings.sortby == SortBy::LASTMOD
          file_results.sort_by {|r| [r.stat.mtime, r.path.downcase, r.filename.downcase]}
        else
          file_results.sort_by {|r| [r.path.downcase, r.filename.downcase]}
        end
      else
        if @settings.sortby == SortBy::FILENAME
          file_results.sort_by {|r| [r.filename, r.path]}
        elsif @settings.sortby == SortBy::FILESIZE
          file_results.sort_by {|r| [r.stat.size, r.path, r.filename]}
        elsif @settings.sortby == SortBy::FILETYPE
          file_results.sort_by {|r| [r.filetype, r.path, r.filename]}
        elsif @settings.sortby == SortBy::LASTMOD
          file_results.sort_by {|r| [r.stat.mtime, r.path, r.filename]}
        else
          file_results.sort_by {|r| [r.path, r.filename]}
        end
      end

    end

    def validate_settings
      raise FindError, 'Startpath not defined' if @settings.paths.empty?
      @settings.paths.each do |p|
        raise FindError, 'Startpath not found' unless Pathname.new(p).exist?
        raise FindError, 'Startpath not readable' unless File.readable?(p)
      end
      if @settings.maxlastmod && @settings.minlastmod && @settings.maxlastmod <= @settings.minlastmod
        raise FindError, 'Invalid range for minlastmod and maxlastmod'
      end
      if @settings.maxsize > 0 && @settings.minsize > 0 && @settings.maxsize <= @settings.minsize
        raise FindError, 'Invalid range for minsize and maxsize'
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

    def filepath_to_fileresult(filepath)
      d = File.dirname(filepath) || '.'
      filename = File.basename(filepath)
      filetype = @filetypes.get_filetype(filename)
      stat = nil
      if @settings.need_stat?
        stat = File.stat(filepath)
      end
      FileResult.new(d, filename, filetype, stat)
    end

    def get_file_results(filepath)
      fileresults = []
      if FileTest.directory?(filepath)
        if @settings.recursive
          Find.find(filepath) do |f|
            if FileTest.directory?(f)
              Find.prune unless matching_dir?(f)
            elsif FileTest.file?(f)
              fileresult = filter_to_fileresult(f)
              if fileresult != nil
                fileresults.push(fileresult)
              end
            end
          end
        else
          Find.find(filepath) do |f|
            if FileTest.directory?(f)
              Find.prune
            else
              fileresult = filter_to_fileresult(f)
              if fileresult != nil
                fileresults.push(fileresult)
              end
            end
          end
        end
      elsif FileTest.file?(filepath)
        fileresult = filter_to_fileresult(f)
        if fileresult != nil
          fileresults.push(fileresult)
        end
      end
      fileresults
    end

  end
end
