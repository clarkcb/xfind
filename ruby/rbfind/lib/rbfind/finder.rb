# frozen_string_literal: true

require 'find'
require 'pathname'
require 'set'
require_relative 'common'
require_relative 'fileresult'
require_relative 'fileresultsorter'
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

    def filter_dir_by_hidden?(dir_path)
      if !@settings.include_hidden && FileUtil.hidden_path?(dir_path)
        return false
      end
      true
    end

    def filter_dir_by_in_patterns?(dir_path)
      path_elems = FileUtil.path_elems(dir_path)
      if !@settings.in_dir_patterns.empty? &&
        !any_matches_any_pattern?(path_elems, @settings.in_dir_patterns)
        return false
      end
      true
    end

    def filter_dir_by_out_patterns?(dir_path)
      path_elems = FileUtil.path_elems(dir_path)
      if !@settings.out_dir_patterns.empty? &&
        any_matches_any_pattern?(path_elems, @settings.out_dir_patterns)
        return false
      end
      true
    end

    def matching_dir?(dir_path)
      filter_dir_by_hidden?(dir_path) &&
        filter_dir_by_in_patterns?(dir_path) &&
        filter_dir_by_out_patterns?(dir_path)
    end

    def has_matching_archive_ext?(file_result)
      if !@settings.in_archive_extensions.empty? || !@settings.out_archive_extensions.empty?
        ext = FileUtil.get_extension(file_result.path)
        return ((@settings.in_archive_extensions.empty? ||
          @settings.in_archive_extensions.include?(ext)) &&
          (@settings.out_archive_extensions.empty? ||
            !@settings.out_archive_extensions.include?(ext)))
      end
      true
    end

    def has_matching_ext?(file_result)
      if !@settings.in_extensions.empty? || !@settings.out_extensions.empty?
        ext = FileUtil.get_extension(file_result.path)
        return ((@settings.in_extensions.empty? ||
          @settings.in_extensions.include?(ext)) &&
          (@settings.out_extensions.empty? ||
            !@settings.out_extensions.include?(ext)))
      end
      true
    end

    def matching_archive_file_name?(file_name)
      ((@settings.in_archive_file_patterns.empty? ||
        matches_any_pattern?(file_name, @settings.in_archive_file_patterns)) &&
        (@settings.out_archive_file_patterns.empty? ||
          !matches_any_pattern?(file_name, @settings.out_archive_file_patterns)))
    end

    def has_matching_archive_file_name?(file_result)
      matching_archive_file_name?(file_result.file_name)
    end

    def matching_file_name?(file_name)
      ((@settings.in_file_patterns.empty? ||
        matches_any_pattern?(file_name, @settings.in_file_patterns)) &&
        (@settings.out_file_patterns.empty? ||
          !matches_any_pattern?(file_name, @settings.out_file_patterns)))
    end

    def has_matching_file_name?(file_result)
      matching_file_name?(file_result.file_name)
    end

    def matching_file_type?(file_type)
      ((@settings.in_file_types.empty? ||
        @settings.in_file_types.include?(file_type)) &&
        (@settings.out_file_types.empty? ||
          !@settings.out_file_types.include?(file_type)))
    end

    def has_matching_file_type?(file_result)
      matching_file_type?(file_result.file_type)
    end

    def matching_file_size?(file_size)
      ((@settings.min_size == 0 ||
        file_size >= @settings.min_size) &&
        (@settings.max_size == 0 ||
          file_size <= @settings.max_size))
    end

    def has_matching_file_size?(file_result)
      matching_file_size?(file_result.file_size)
    end

    def matching_last_mod?(last_mod)
      ((@settings.min_last_mod.nil? ||
        last_mod >= @settings.min_last_mod.to_time) &&
        (@settings.max_last_mod.nil? ||
          last_mod <= @settings.max_last_mod.to_time))
    end

    def has_matching_last_mod?(file_result)
      matching_last_mod?(file_result.last_mod)
    end

    def matching_archive_file_result?(file_result)
      has_matching_archive_ext?(file_result) &&
        matching_archive_file_name?(file_result.file_name) &&
        matching_file_size?(file_result.file_size) &&
        matching_last_mod?(file_result.last_mod)
    end

    def matching_file_result?(file_result)
      has_matching_ext?(file_result) &&
        matching_file_name?(file_result.file_name) &&
        matching_file_type?(file_result.file_type) &&
        matching_file_size?(file_result.file_size) &&
        matching_last_mod?(file_result.last_mod)
    end

    def matching_archive_file?(file_path)
      matching_archive_file_result?(file_path_to_file_result(file_path))
    end

    def matching_file?(file_path)
      matching_file_result?(file_path_to_file_result(file_path))
    end

    def filter_to_file_result(file_path)
      if !@settings.include_hidden && FileUtil.hidden_path?(file_path)
        return nil
      end
      unless matching_dir?(file_path.parent)
        return nil
      end
      file_type = @file_types.get_file_type(file_path)
      if file_type == FileType::ARCHIVE && !@settings.include_archives && !@settings.archives_only
        return nil
      end
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
      file_result = FileResult.new(file_path, file_type, file_size, last_mod)
      if file_result.file_type == FileType::ARCHIVE
        if matching_archive_file_result?(file_result)
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
        file_results = file_results.concat(get_file_results_for_path(p))
      end
      file_result_sorter = FileResultSorter.new(@settings)
      file_result_sorter.sort(file_results)
      file_results
    end

    def print_dir_results(file_results, formatter)
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

    def print_file_results(file_results, formatter)
      if file_results.empty?
        RbFind::log("\nMatching files: 0")
      else
        RbFind::log("\nMatching files (#{file_results.size}):")
        file_results.each do |fr|
          RbFind::log("#{formatter.format_file_result(fr)}\n")
        end
      end
    end

    private

    def validate_settings
      raise FindError, STARTPATH_NOT_DEFINED if @settings.paths.empty?
      @settings.paths.each do |p|
        if p.instance_of?(Pathname)
          p = p.expand_path unless p.exist?
          raise FindError, STARTPATH_NOT_FOUND unless p.exist?
          raise FindError, STARTPATH_NOT_READABLE unless p.readable?
        else
          raise FindError, STARTPATH_NOT_PATHNAME
        end
      end
      if @settings.max_depth > -1 && @settings.min_depth > -1 && @settings.max_depth < @settings.min_depth
        raise FindError, INVALID_RANGE_MINDEPTH_MAXDEPTH
      end
      if @settings.max_last_mod && @settings.min_last_mod && @settings.max_last_mod <= @settings.min_last_mod
        raise FindError, INVALID_RANGE_MINLASTMOD_MAXLASTMOD
      end
      if @settings.max_size > 0 && @settings.min_size > 0 && @settings.max_size <= @settings.min_size
        raise FindError, INVALID_RANGE_MINSIZE_MAXSIZE
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

    def rec_get_file_results_for_path(dir_path, min_depth, max_depth, current_depth)
      file_results = []
      recurse = true
      if current_depth == max_depth
        recurse = false
      elsif max_depth > -1 && current_depth > max_depth
        return file_results
      end
      dirs = []
      dir_path.each_child do |f|
        unless f.symlink? && !@settings.follow_symlinks
          if f.directory? && recurse && filter_dir_by_hidden?(f) && filter_dir_by_out_patterns?(f)
            dirs << f
          elsif f.file? && (min_depth < 0 || current_depth >= min_depth)
            file_result = filter_to_file_result(f)
            if file_result != nil
              file_results.push(file_result)
            end
          end
        end
      end
      dirs.each do |d|
        file_results = file_results.concat(rec_get_file_results_for_path(d, min_depth, max_depth, current_depth + 1))
      end
      file_results
    end

    def get_file_results_for_path(file_path)
      unless file_path.exist?
        file_path = file_path.expand_path
      end
      if file_path.directory?
        # if max_depth is zero, we can skip since a directory cannot be a result
        if @settings.max_depth == 0
          return []
        end
        if filter_dir_by_hidden?(file_path) && filter_dir_by_out_patterns?(file_path)
          max_depth = @settings.max_depth
          unless @settings.recursive
            max_depth = 1
          end
          return rec_get_file_results_for_path(file_path, @settings.min_depth, max_depth, 1)
        else
          raise FindError, STARTPATH_NOT_MATCH_SETTINGS
        end
      else
        # if min_depth > zero, we can skip since the file is at depth zero
        if @settings.min_depth > 0
          return []
        end
        file_result = filter_to_file_result(file_path)
        if file_result != nil
          return [file_result]
        else
          raise FindError, STARTPATH_NOT_MATCH_SETTINGS
        end
      end
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

  end
end
