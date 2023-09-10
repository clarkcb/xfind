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
      @file_types = FileTypes.new
      @results = []
    end

    def matching_dir?(dirname)
      path_elems = dirname.split(File::SEPARATOR) - FileUtil.dot_dirs
      if @settings.exclude_hidden && path_elems.any? { |p| FileUtil.hidden?(p) }
        return false
      end
      if !@settings.in_dir_patterns.empty? &&
        !any_matches_any_pattern(path_elems, @settings.in_dir_patterns)
        return false
      end
      if !@settings.out_dir_patterns.empty? &&
        any_matches_any_pattern(path_elems, @settings.out_dir_patterns)
        return false
      end
      true
    end

    def matching_file?(file_path)
      matching_file_result?(file_path_to_file_result(file_path))
    end

    def matching_file_result?(file_result)
      if !@settings.in_extensions.empty? || !@settings.out_extensions.empty?
        ext = FileUtil.get_extension(file_result.file_name)
        if !@settings.in_extensions.empty? &&
          !@settings.in_extensions.include?(ext)
          return false
        end
        if !@settings.out_extensions.empty? &&
          @settings.out_extensions.include?(ext)
          return false
        end
      end
      if !@settings.in_file_patterns.empty? &&
        !matches_any_pattern(file_result.file_name, @settings.in_file_patterns)
        return false
      end
      if !@settings.out_file_patterns.empty? &&
        matches_any_pattern(file_result.file_name, @settings.out_file_patterns)
        return false
      end
      if !@settings.in_file_types.empty? &&
        !@settings.in_file_types.include?(file_result.file_type)
        return false
      end
      if !@settings.out_file_types.empty? &&
        @settings.out_file_types.include?(file_result.file_type)
        return false
      end
      if @settings.max_last_mod && file_result.stat.mtime > @settings.max_last_mod.to_time
        return false
      end
      if @settings.max_size > 0 && file_result.stat.size > @settings.max_size
        return false
      end
      if @settings.min_last_mod && file_result.stat.mtime < @settings.min_last_mod.to_time
        return false
      end
      if @settings.min_size > 0 && file_result.stat.size < @settings.min_size
        return false
      end
      true
    end

    def matching_archive_file?(file_path)
      filename = File.basename(file_path)
      ext = FileUtil.get_extension(filename)
      if !@settings.in_archive_extensions.empty? &&
        !@settings.in_archive_extensions.include?(ext)
        return false
      end
      if !@settings.out_archive_extensions.empty? &&
        @settings.out_archive_extensions.include?(ext)
        return false
      end
      if !@settings.in_archive_file_patterns.empty? &&
        !matches_any_pattern(filename, @settings.in_archive_file_patterns)
        return false
      end
      if !@settings.out_archive_file_patterns.empty? &&
        matches_any_pattern(filename, @settings.out_archive_file_patterns)
        return false
      end
      true
    end

    def filter_to_file_result(file_path)
      filename = File.basename(file_path)
      if @settings.exclude_hidden && FileUtil.hidden?(filename)
        return nil
      end
      file_result = file_path_to_file_result(file_path)
      if file_result.file_type == FileType::ARCHIVE
        if @settings.include_archives && matching_archive_file?(filename)
          return file_result
        end
        return nil
      end
      if !@settings.archives_only && matching_file_result?(file_result)
        return file_result
      end
      nil
    end

    def find
      file_results = []
      @settings.paths.each do |p|
        file_results = file_results.concat(get_file_results(p))
      end
      file_results = sort_file_results(file_results)
      if @settings.sort_descending
        file_results.reverse!
      end
      file_results
    end

    private

    def sort_file_results(file_results)
      if @settings.sort_case_insensitive
        if @settings.sort_by == SortBy::FILENAME
          file_results.sort_by {|r| [r.file_name.downcase, r.path.downcase]}
        elsif @settings.sort_by == SortBy::FILESIZE
          file_results.sort_by {|r| [r.stat.size, r.path.downcase, r.file_name.downcase]}
        elsif @settings.sort_by == SortBy::FILETYPE
          file_results.sort_by {|r| [r.file_type, r.path.downcase, r.file_name.downcase]}
        elsif @settings.sort_by == SortBy::LASTMOD
          file_results.sort_by {|r| [r.stat.mtime, r.path.downcase, r.file_name.downcase]}
        else
          file_results.sort_by {|r| [r.path.downcase, r.file_name.downcase]}
        end
      else
        if @settings.sort_by == SortBy::FILENAME
          file_results.sort_by {|r| [r.file_name, r.path]}
        elsif @settings.sort_by == SortBy::FILESIZE
          file_results.sort_by {|r| [r.stat.size, r.path, r.file_name]}
        elsif @settings.sort_by == SortBy::FILETYPE
          file_results.sort_by {|r| [r.file_type, r.path, r.file_name]}
        elsif @settings.sort_by == SortBy::LASTMOD
          file_results.sort_by {|r| [r.stat.mtime, r.path, r.file_name]}
        else
          file_results.sort_by {|r| [r.path, r.file_name]}
        end
      end

    end

    def validate_settings
      raise FindError, 'Startpath not defined' if @settings.paths.empty?
      @settings.paths.each do |p|
        raise FindError, 'Startpath not found' unless Pathname.new(p).exist?
        raise FindError, 'Startpath not readable' unless File.readable?(p)
      end
      if @settings.max_depth > -1 && @settings.min_depth > -1 && @settings.max_depth < @settings.min_depth
        raise FindError, 'Invalid range for mindepth and maxdepth'
      end
      if @settings.max_last_mod && @settings.min_last_mod && @settings.max_last_mod <= @settings.min_last_mod
        raise FindError, 'Invalid range for minlastmod and maxlastmod'
      end
      if @settings.max_size > 0 && @settings.min_size > 0 && @settings.max_size <= @settings.min_size
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

    def file_path_to_file_result(file_path)
      d = File.dirname(file_path) || '.'
      filename = File.basename(file_path)
      file_type = @file_types.get_file_type(filename)
      stat = nil
      if @settings.need_stat?
        stat = File.stat(file_path)
      end
      FileResult.new(d, filename, file_type, stat)
    end

    def get_file_results(file_path)
      file_results = []
      if FileTest.directory?(file_path)
        # if max_depth is zero, we can skip since a directory cannot be a result
        if @settings.max_depth == 0
          return []
        end
        if @settings.recursive
          # TODO: get depth of file_path, and get depth of every f below
          file_path_sep_count = FileUtil.sep_count(file_path)
          Find.find(file_path) do |f|
            f_sep_count = FileUtil.sep_count(f)
            if FileTest.directory?(f)
              # The +1 is for files under the directory
              depth = f_sep_count - file_path_sep_count + 1
              if depth > @settings.max_depth || !matching_dir?(f)
                Find.prune
              end
            elsif FileTest.file?(f)
              depth = f_sep_count - file_path_sep_count
              if depth > @settings.max_depth || depth < @settings.min_depth
                Find.prune
              else
                file_result = filter_to_file_result(f)
                if file_result != nil
                  file_results.push(file_result)
                end
              end
            end
          end
        else
          Find.find(file_path) do |f|
            if FileTest.directory?(f)
              Find.prune
            else
              file_result = filter_to_file_result(f)
              if file_result != nil
                file_results.push(file_result)
              end
            end
          end
        end
      elsif FileTest.file?(file_path)
        # if min_depth > zero, we can skip since the file is at depth zero
        if @settings.min_depth > 0
          return []
        end
        file_result = filter_to_file_result(file_path)
        if file_result != nil
          file_results.push(file_result)
        end
      end
      file_results
    end

  end
end
