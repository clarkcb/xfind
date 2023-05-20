require_relative 'filetypes'

module RbFind

  module SortBy
    FILEPATH = 0
    FILENAME = 1
    FILESIZE = 2
    FILETYPE = 3
    LASTMOD  = 4
  end

  # FindSettings - encapsulates find settings
  class FindSettings

    attr_reader :archives_only
    attr_reader :debug
    attr_accessor :exclude_hidden
    attr_accessor :in_archive_extensions
    attr_accessor :in_archive_file_patterns
    attr_accessor :in_dir_patterns
    attr_accessor :in_extensions
    attr_accessor :in_file_patterns
    attr_accessor :in_file_types
    attr_accessor :include_archives
    attr_accessor :list_dirs
    attr_accessor :list_files
    attr_accessor :max_last_mod
    attr_accessor :max_size
    attr_accessor :min_last_mod
    attr_accessor :min_size
    attr_accessor :out_archive_extensions
    attr_accessor :out_archive_file_patterns
    attr_accessor :out_dir_patterns
    attr_accessor :out_extensions
    attr_accessor :out_file_patterns
    attr_accessor :out_file_types
    attr_accessor :print_usage
    attr_accessor :print_version
    attr_accessor :recursive
    attr_accessor :paths
    attr_accessor :sort_by
    attr_accessor :sort_case_insensitive
    attr_accessor :sort_descending
    attr_accessor :verbose

    def initialize
      @archives_only = false
      @debug = false
      @exclude_hidden = true
      @include_archives = false
      @list_dirs = false
      @list_files = false
      @max_last_mod = nil
      @max_size = 0
      @min_last_mod = nil
      @min_size = 0
      @print_usage = false
      @print_version = false
      @recursive = true
      @sort_by = SortBy::FILEPATH
      @sort_case_insensitive = false
      @sort_descending = false
      @verbose = false

      @in_archive_extensions = []
      @in_archive_file_patterns = []
      @in_dir_patterns = []
      @in_extensions = []
      @in_file_patterns = []
      @in_file_types = []
      @out_archive_extensions = []
      @out_archive_file_patterns = []
      @out_dir_patterns = []
      @out_extensions = []
      @out_file_patterns = []
      @out_file_types = []
      @paths = []
    end

    def add_exts(exts, ext_set)
      if exts.instance_of? String
        exts.split(',').each do |x|
          ext_set.push(x)
        end
      elsif exts.instance_of? Array
        exts.each do |x|
          ext_set.push(x)
        end
      end
    end

    def add_patterns(patterns, pattern_set)
      if patterns.instance_of? String
        pattern_set.push(Regexp.new(patterns))
      elsif patterns.instance_of? Array
        patterns.each do |p|
          pattern_set.push(Regexp.new(p))
        end
      end
    end

    def add_pattern(pattern, pattern_set)
      pattern_set.push(Regexp.new(pattern))
    end

    def add_file_types(file_types, file_types_set)
      if file_types.instance_of? String
        file_types.split(',').each do |t|
          file_types_set.push(FileTypes.from_name(t))
        end
      elsif file_types.instance_of? Array
        file_types.each do |t|
          file_types_set.push(FileTypes.from_name(t))
        end
      end
    end

    def need_stat?
      if @sort_by == SortBy::FILESIZE || @sort_by == SortBy::LASTMOD
        return true
      end
      if @max_last_mod || @max_size > 0 || @min_last_mod || @min_size > 0
        return true
      end
      false
    end

    def set_sort_by(sort_by_name)
      sort_by_name = sort_by_name.strip.upcase
      if sort_by_name == 'NAME'
        @sort_by = SortBy::FILENAME
      elsif sort_by_name == 'SIZE'
        @sort_by = SortBy::FILESIZE
      elsif sort_by_name == 'TYPE'
        @sort_by = SortBy::FILETYPE
      elsif sort_by_name == 'LASTMOD'
        @sort_by = SortBy::LASTMOD
      else
        @sort_by = SortBy::FILEPATH
      end
    end

    def archives_only=(bool)
      @archives_only = bool
      @include_archives = bool if bool
    end

    def debug=(bool)
      @debug = bool
      @verbose = bool if bool
    end

    def to_s
      s = 'FindSettings('
      s << "archives_only: #{@archives_only}"
      s << ", debug: #{@debug}"
      s << ", exclude_hidden: #{@exclude_hidden}"
      s << ', ' + list_to_s('in_archive_extensions', @in_archive_extensions)
      s << ', ' + list_to_s('in_archive_file_patterns', @in_archive_file_patterns)
      s << ', ' + list_to_s('in_dir_patterns', @in_dir_patterns)
      s << ', ' + list_to_s('in_extensions', @in_extensions)
      s << ', ' + list_to_s('in_file_patterns', @in_file_patterns)
      s << ', ' + file_types_to_s('in_file_types', @in_file_types)
      s << ", include_archives: #{@include_archives}"
      s << ", list_dirs: #{@list_dirs}"
      s << ", list_files: #{@list_files}"
      s << ", max_last_mod: #{@max_last_mod}"
      s << ", max_size: #{@max_size}"
      s << ", min_last_mod: #{@min_last_mod}"
      s << ", min_size: #{@min_size}"
      s << ', ' + list_to_s('out_archive_extensions', @out_archive_extensions)
      s << ', ' + list_to_s('out_archive_file_patterns', @out_archive_file_patterns)
      s << ', ' + list_to_s('out_dir_patterns', @out_dir_patterns)
      s << ', ' + list_to_s('out_extensions', @out_extensions)
      s << ', ' + list_to_s('out_file_patterns', @out_file_patterns)
      s << ', ' + file_types_to_s('out_file_types', @out_file_types)
      s << ', ' + list_to_s('paths', @paths)
      s << ", print_usage: #{@print_usage}"
      s << ", print_version: #{@print_version}"
      s << ", recursive: #{@recursive}"
      s << ", sort_by: #{@sort_by}"
      s << ", sort_case_insensitive: #{@sort_case_insensitive}"
      s << ", sort_descending: #{@sort_descending}"
      s << ", verbose: #{@verbose}"
      s << ')'
      s
    end

    private

    def list_to_s(name, lst)
      "#{name}=[\"#{lst.join('", "')}\"]"
    end

    def file_types_to_s(name, file_types)
      s = "#{name}=["
      count = 0
      file_types.each do |ft|
        s << ', ' if count.positive?
        s << "\"#{FileTypes.to_name(ft)}\""
        count += 1
      end
      s + ']'
    end
  end
end
