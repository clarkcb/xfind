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

    def get_file_result_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          case @settings.sort_by
          when SortBy::FILENAME
            ->(fr1, fr2) { fr2.cmp_by_name_ci(fr1) }
          when SortBy::FILESIZE
            ->(fr1, fr2) { fr2.cmp_by_size_ci(fr1) }
          when SortBy::FILETYPE
            ->(fr1, fr2) { fr2.cmp_by_type_ci(fr1) }
          when SortBy::LASTMOD
            ->(fr1, fr2) { fr2.cmp_by_last_mod_ci(fr1) }
          else
            ->(fr1, fr2) { fr2.cmp_by_path_ci(fr1) }
          end
        else
          case @settings.sort_by
          when SortBy::FILENAME
            ->(fr1, fr2) { fr2.cmp_by_name(fr1) }
          when SortBy::FILESIZE
            ->(fr1, fr2) { fr2.cmp_by_size(fr1) }
          when SortBy::FILETYPE
            ->(fr1, fr2) { fr2.cmp_by_type(fr1) }
          when SortBy::LASTMOD
            ->(fr1, fr2) { fr2.cmp_by_last_mod(fr1) }
          else
            ->(fr1, fr2) { fr2.cmp_by_path(fr1) }
          end
        end
      else
        if @settings.sort_case_insensitive
          case @settings.sort_by
          when SortBy::FILENAME
            ->(fr1, fr2) { fr1.cmp_by_name_ci(fr2) }
          when SortBy::FILESIZE
            ->(fr1, fr2) { fr1.cmp_by_size_ci(fr2) }
          when SortBy::FILETYPE
            ->(fr1, fr2) { fr1.cmp_by_type_ci(fr2) }
          when SortBy::LASTMOD
            ->(fr1, fr2) { fr1.cmp_by_last_mod_ci(fr2) }
          else
            ->(fr1, fr2) { fr1.cmp_by_path_ci(fr2) }
          end
        else
          case @settings.sort_by
          when SortBy::FILENAME
            ->(fr1, fr2) { fr1.cmp_by_name(fr2) }
          when SortBy::FILESIZE
            ->(fr1, fr2) { fr1.cmp_by_size(fr2) }
          when SortBy::FILETYPE
            ->(fr1, fr2) { fr1.cmp_by_type(fr2) }
          when SortBy::LASTMOD
            ->(fr1, fr2) { fr1.cmp_by_last_mod(fr2) }
          else
            ->(fr1, fr2) { fr1.cmp_by_path(fr2) }
          end
        end
      end
    end

  end
end
