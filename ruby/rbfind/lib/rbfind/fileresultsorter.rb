# frozen_string_literal: true

################################################################################
#
# fileresultsorter.rb
#
# Sorter for file results
#
################################################################################

module RbFind
  # FileResultSorter - formats file result instances
  class FileResultSorter

    attr_reader :settings

    def initialize(settings)
      @settings = settings
    end

    def sort(file_results)
      file_result_comparator = get_file_result_comparator
      file_results.sort!(&file_result_comparator)
    end

    private

    def get_file_path_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(fr1, fr2) { fr2.cmp_by_path_ci(fr1) }
        else
          ->(fr1, fr2) { fr2.cmp_by_path(fr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(fr1, fr2) { fr1.cmp_by_path_ci(fr2) }
      else
        ->(fr1, fr2) { fr1.cmp_by_path(fr2) }
      end
    end

    def get_file_name_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(fr1, fr2) { fr2.cmp_by_name_ci(fr1) }
        else
          ->(fr1, fr2) { fr2.cmp_by_name(fr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(fr1, fr2) { fr1.cmp_by_name_ci(fr2) }
      else
        ->(fr1, fr2) { fr1.cmp_by_name(fr2) }
      end
    end

    def get_file_size_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(fr1, fr2) { fr2.cmp_by_size_ci(fr1) }
        else
          ->(fr1, fr2) { fr2.cmp_by_size(fr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(fr1, fr2) { fr1.cmp_by_size_ci(fr2) }
      else
        ->(fr1, fr2) { fr1.cmp_by_size(fr2) }
      end
    end

    def get_file_type_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(fr1, fr2) { fr2.cmp_by_type_ci(fr1) }
        else
          ->(fr1, fr2) { fr2.cmp_by_type(fr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(fr1, fr2) { fr1.cmp_by_type_ci(fr2) }
      else
        ->(fr1, fr2) { fr1.cmp_by_type(fr2) }
      end
    end

    def get_last_mod_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(fr1, fr2) { fr2.cmp_by_last_mod_ci(fr1) }
        else
          ->(fr1, fr2) { fr2.cmp_by_last_mod(fr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(fr1, fr2) { fr1.cmp_by_last_mod_ci(fr2) }
      else
        ->(fr1, fr2) { fr1.cmp_by_last_mod(fr2) }
      end
    end

    def get_file_result_comparator
      case @settings.sort_by
      when SortBy::FILENAME
        get_file_name_comparator
      when SortBy::FILEPATH
        get_file_path_comparator
      when SortBy::FILESIZE
        get_file_size_comparator
      when SortBy::FILETYPE
        get_file_type_comparator
      when SortBy::LASTMOD
        get_last_mod_comparator
      else
        get_file_path_comparator
      end
    end

  end
end
