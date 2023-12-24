require_relative 'filetypes'

module RbFind

  module SortBy
    FILEPATH = 0
    FILENAME = 1
    FILESIZE = 2
    FILETYPE = 3
    LASTMOD  = 4

    NAMES = %w[filepath filename filesize filetype lastmod].freeze

    module_function
    def from_name(name)
      idx = NAMES.index(name.downcase)
      idx.nil? ? 0 : idx
    end

    def to_name(sort_by)
      sort_by < NAMES.size ? NAMES[sort_by] : NAMES[0]
    end
  end

  # FindSettings - encapsulates find settings
  class FindSettings

    attr_reader :archives_only
    attr_reader :debug
    attr_accessor :in_archive_extensions
    attr_accessor :in_archive_file_patterns
    attr_accessor :in_dir_patterns
    attr_accessor :in_extensions
    attr_accessor :in_file_patterns
    attr_accessor :in_file_types
    attr_accessor :include_archives
    attr_accessor :include_hidden
    attr_accessor :list_dirs
    attr_accessor :list_files
    attr_accessor :max_depth
    attr_accessor :max_last_mod
    attr_accessor :max_size
    attr_accessor :min_depth
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
      @include_archives = false
      @include_hidden = false
      @list_dirs = false
      @list_files = false
      @max_depth = -1
      @max_last_mod = nil
      @max_size = 0
      @min_depth = -1
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
          file_types_set.push(FileType.from_name(t))
        end
      elsif file_types.instance_of? Array
        file_types.each do |t|
          file_types_set.push(FileType.from_name(t))
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
      sort_by_name = sort_by_name.strip.downcase
      if sort_by_name == 'name'
        @sort_by = SortBy::FILENAME
      elsif sort_by_name == 'size'
        @sort_by = SortBy::FILESIZE
      elsif sort_by_name == 'type'
        @sort_by = SortBy::FILETYPE
      elsif sort_by_name == 'lastmod'
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

    def last_mod_to_s(last_mod)
      if last_mod.nil?
        "0"
      else
        "\"#{last_mod.to_s}\""
      end
    end

    def to_s
      'FindSettings(' +
        "archives_only=#{@archives_only}" +
        ", debug=#{@debug}" +
        ', ' + list_to_s('in_archive_extensions', @in_archive_extensions) +
        ', ' + list_to_s('in_archive_file_patterns', @in_archive_file_patterns.map { |p| p.source }) +
        ', ' + list_to_s('in_dir_patterns', @in_dir_patterns.map { |p| p.source }) +
        ', ' + list_to_s('in_extensions', @in_extensions) +
        ', ' + list_to_s('in_file_patterns', @in_file_patterns.map { |p| p.source }) +
        ', ' + file_types_to_s('in_file_types', @in_file_types) +
        ", include_archives=#{@include_archives}" +
        ", include_hidden=#{@include_hidden}" +
        ", list_dirs=#{@list_dirs}" +
        ", list_files=#{@list_files}" +
        ", max_depth=#{@max_depth}" +
        ", max_last_mod=#{last_mod_to_s(@max_last_mod)}" +
        ", max_size=#{@max_size}" +
        ", min_depth=#{@min_depth}" +
        ", min_last_mod=#{last_mod_to_s(@min_last_mod)}" +
        ", min_size=#{@min_size}" +
        ', ' + list_to_s('out_archive_extensions', @out_archive_extensions) +
        ', ' + list_to_s('out_archive_file_patterns', @out_archive_file_patterns.map { |p| p.source }) +
        ', ' + list_to_s('out_dir_patterns', @out_dir_patterns.map { |p| p.source }) +
        ', ' + list_to_s('out_extensions', @out_extensions) +
        ', ' + list_to_s('out_file_patterns', @out_file_patterns.map { |p| p.source }) +
        ', ' + file_types_to_s('out_file_types', @out_file_types) +
        ', ' + list_to_s('paths', @paths) +
        ", print_usage=#{@print_usage}" +
        ", print_version=#{@print_version}" +
        ", recursive=#{@recursive}" +
        ", settings_only=#{@settings_only}" +
        ", sort_by=" + SortBy::to_name(@sort_by) +
        ", sort_case_insensitive=#{@sort_case_insensitive}" +
        ", sort_descending=#{@sort_descending}" +
        ", verbose=#{@verbose}" +
        ')'
    end

    private

    def list_to_s(name, lst)
      if lst.empty?
        "#{name}=[]"
      else
        "#{name}=[\"#{lst.join('", "')}\"]"
      end
    end

    def file_types_to_s(name, file_types)
      s = "#{name}=["
      count = 0
      file_types.each do |ft|
        s << ', ' if count.positive?
        s << FileType.to_name(ft)
        count += 1
      end
      s + ']'
    end
  end
end
