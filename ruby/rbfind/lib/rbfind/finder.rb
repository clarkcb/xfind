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

    def initialize(settings)
      @settings = settings
      validate_settings
      @file_types = FileTypes.new
    end

    def matching_dir?(dir_path)
      if !@settings.include_hidden && FileUtil.hidden?(dir_path)
        return false
      end
      path_elems = FileUtil.path_elems(dir_path)
      if !@settings.in_dir_patterns.empty? &&
        !any_matches_any_pattern?(path_elems, @settings.in_dir_patterns)
        return false
      end
      if !@settings.out_dir_patterns.empty? &&
        any_matches_any_pattern?(path_elems, @settings.out_dir_patterns)
        return false
      end
      true
    end

    def has_matching_archive_ext?(file_result)
      if !@settings.in_archive_extensions.empty? || !@settings.out_archive_extensions.empty?
        ext = FileUtil.get_extension(file_result.path)
        return ((@settings.in_archive_extensions.empty? ||
          @settings.in_archive_extensions.include?(ext)) and
          (@settings.out_archive_extensions.empty? ||
            !@settings.out_archive_extensions.include?(ext)))
      end
      true
    end

    def has_matching_ext?(file_result)
      if !@settings.in_extensions.empty? || !@settings.out_extensions.empty?
        ext = FileUtil.get_extension(file_result.path)
        return ((@settings.in_extensions.empty? ||
          @settings.in_extensions.include?(ext)) and
          (@settings.out_extensions.empty? ||
            !@settings.out_extensions.include?(ext)))
      end
      true
    end

    def has_matching_archive_file_name?(file_result)
      ((@settings.in_archive_file_patterns.empty? ||
        matches_any_pattern?(file_result.file_name, @settings.in_archive_file_patterns)) and
        (@settings.out_archive_file_patterns.empty? ||
          !matches_any_pattern?(file_result.file_name, @settings.out_archive_file_patterns)))
    end

    def has_matching_file_name?(file_result)
      ((@settings.in_file_patterns.empty? ||
        matches_any_pattern?(file_result.file_name, @settings.in_file_patterns)) and
        (@settings.out_file_patterns.empty? ||
          !matches_any_pattern?(file_result.file_name, @settings.out_file_patterns)))
    end

    def has_matching_file_type?(file_result)
      ((@settings.in_file_types.empty? ||
        @settings.in_file_types.include?(file_result.file_type)) and
        (@settings.out_file_types.empty? ||
          !@settings.out_file_types.include?(file_result.file_type)))
    end

    def has_matching_file_size?(file_result)
      ((@settings.min_size == 0 ||
        file_result.file_size >= @settings.min_size) and
        (@settings.max_size == 0 ||
          file_result.file_size <= @settings.max_size))
    end

    def has_matching_last_mod?(file_result)
      ((@settings.min_last_mod.nil? ||
        file_result.last_mod >= @settings.min_last_mod.to_time) and
        (@settings.max_last_mod.nil? ||
          file_result.last_mod <= @settings.max_last_mod.to_time))
    end

    def matching_archive_file_result?(file_result)
      has_matching_archive_ext?(file_result) and
        has_matching_archive_file_name?(file_result) and
        has_matching_file_size?(file_result) and
        has_matching_last_mod?(file_result)
    end

    def matching_file_result?(file_result)
      has_matching_ext?(file_result) and
        has_matching_file_name?(file_result) and
        has_matching_file_type?(file_result) and
        has_matching_file_size?(file_result) and
        has_matching_last_mod?(file_result)
    end

    def matching_archive_file?(file_path)
      matching_archive_file_result?(file_path_to_file_result(file_path))
    end

    def matching_file?(file_path)
      matching_file_result?(file_path_to_file_result(file_path))
    end

    def filter_to_file_result(file_path)
      if !@settings.include_hidden && FileUtil.hidden?(file_path)
        return nil
      end
      file_result = file_path_to_file_result(file_path)
      if file_result.file_type == FileType::ARCHIVE
        if @settings.include_archives && matching_archive_file_result?(file_result)
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
          file_results.sort_by {|r| [r.file_name.downcase, r.dir_name.downcase]}
        elsif @settings.sort_by == SortBy::FILESIZE
          file_results.sort_by {|r| [r.file_size, r.dir_name.downcase, r.file_name.downcase]}
        elsif @settings.sort_by == SortBy::FILETYPE
          file_results.sort_by {|r| [r.file_type, r.dir_name.downcase, r.file_name.downcase]}
        elsif @settings.sort_by == SortBy::LASTMOD
          file_results.sort_by {|r| [r.last_mod, r.dir_name.downcase, r.file_name.downcase]}
        else
          file_results.sort_by {|r| [r.dir_name.downcase, r.file_name.downcase]}
        end
      else
        if @settings.sort_by == SortBy::FILENAME
          file_results.sort_by {|r| [r.file_name, r.dir_name]}
        elsif @settings.sort_by == SortBy::FILESIZE
          file_results.sort_by {|r| [r.file_size, r.dir_name, r.file_name]}
        elsif @settings.sort_by == SortBy::FILETYPE
          file_results.sort_by {|r| [r.file_type, r.dir_name, r.file_name]}
        elsif @settings.sort_by == SortBy::LASTMOD
          file_results.sort_by {|r| [r.last_mod, r.dir_name, r.file_name]}
        else
          file_results.sort_by {|r| [r.dir_name, r.file_name]}
        end
      end
    end

    def validate_settings
      raise FindError, 'Startpath not defined' if @settings.paths.empty?
      @settings.paths.each do |p|
        if p.instance_of?(Pathname)
          raise FindError, 'Startpath not found' unless p.exist?
          raise FindError, 'Startpath not readable' unless p.readable?
        else
          raise FindError, 'Startpath not a Pathname instance'
        end
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

    def matches_any_pattern?(str, pattern_set)
      pattern_set.any? { |p| p.match(str) }
    end

    def any_matches_any_pattern?(str_list, pattern_set)
      str_list.each do |s|
        return true if matches_any_pattern?(s, pattern_set)
      end
      false
    end

    def file_path_to_file_result(file_path)
      file_type = @file_types.get_file_type(file_path)
      file_size = 0
      last_mod = nil
      if @settings.need_last_mod? || @settings.need_size?
        stat = file_path.stat
        if @settings.need_last_mod?
          last_mod = stat.mtime
        end
        if @settings.need_size?
          file_size = stat.size
        end
      end
      FileResult.new(file_path, file_type, file_size, last_mod)
    end

    def get_file_results(file_path)
      file_results = []
      if file_path.directory?
        # if max_depth is zero, we can skip since a directory cannot be a result
        if @settings.max_depth == 0
          return []
        end
        if @settings.recursive
          # TODO: get depth of file_path, and get depth of every f below
          file_path_elem_count = FileUtil.elem_count(file_path)
          file_path.find do |f|
            if FileUtil.dot_dir?(f)
              next
            end
            f_elem_count = FileUtil.elem_count(f)
            if f.directory?
              # The +1 is for files under the directory
              depth = f_elem_count - file_path_elem_count + 1
              if (@settings.max_depth > 0 && depth > @settings.max_depth) || !matching_dir?(f)
                Find.prune
              end
            elsif f.file?
              depth = f_elem_count - file_path_elem_count
              if depth < @settings.min_depth || (@settings.max_depth > 0 && depth > @settings.max_depth)
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
          file_path.find do |f|
            if f.directory?
              Find.prune
            else
              file_result = filter_to_file_result(f)
              if file_result != nil
                file_results.push(file_result)
              end
            end
          end
        end
      elsif file_path.file?
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
