require_relative 'filetypes'

module RbFind

  module SortBy
    FILEPATH = 0
    FILENAME = 1
    FILESIZE = 2
    FILETYPE = 3
    LASTMOD  = 4

    NAMES = Array[:filepath, :filename, :filesize, :filetype, :lastmod].freeze

    module_function
    def from_name(name)
      idx = NAMES.index(name.downcase.to_sym)
      idx.nil? ? 0 : idx
    end

    def from_sym(sym)
      idx = NAMES.index(sym)
      idx.nil? ? 0 : idx
    end

    def to_name(sort_by)
      sort_by < NAMES.size ? NAMES[sort_by].to_s : NAMES[0].to_s
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
    attr_accessor :paths
    attr_accessor :print_dirs
    attr_accessor :print_files
    attr_accessor :print_usage
    attr_accessor :print_version
    attr_accessor :recursive
    attr_accessor :sort_by
    attr_accessor :sort_case_insensitive
    attr_accessor :sort_descending
    attr_accessor :verbose

    def initialize
      @archives_only = false
      @debug = false
      @in_archive_extensions = Set::new
      @in_archive_file_patterns = Set::new
      @in_dir_patterns = Set::new
      @in_extensions = Set::new
      @in_file_patterns = Set::new
      @in_file_types = Set::new
      @include_archives = false
      @include_hidden = false
      @max_depth = -1
      @max_last_mod = nil
      @max_size = 0
      @min_depth = -1
      @min_last_mod = nil
      @min_size = 0
      @out_archive_extensions = Set::new
      @out_archive_file_patterns = Set::new
      @out_dir_patterns = Set::new
      @out_extensions = Set::new
      @out_file_patterns = Set::new
      @out_file_types = Set::new
      @paths = Set::new
      @print_dirs = false
      @print_files = false
      @print_usage = false
      @print_version = false
      @recursive = true
      @sort_by = SortBy::FILEPATH
      @sort_case_insensitive = false
      @sort_descending = false
      @verbose = false
    end

    def add_exts(exts, ext_set)
      if exts.instance_of?(String)
        exts.split(',').each do |x|
          ext_set.add(x)
        end
      elsif exts.instance_of?(Array) || exts.instance_of?(Enumerable)
        exts.each do |x|
          ext_set.add(x)
        end
      end
    end

    def add_patterns(patterns, pattern_set)
      if patterns.instance_of?(String)
        pattern_set.add(Regexp.new(patterns))
      elsif patterns.instance_of?(Array) ||  patterns.instance_of?(Enumerable)
        patterns.each do |p|
          pattern_set.add(Regexp.new(p))
        end
      end
    end

    def add_pattern(pattern, pattern_set)
      pattern_set.add(Regexp.new(pattern))
    end

    def add_file_types(file_types, file_types_set)
      if file_types.instance_of?(String)
        file_types.split(',').each do |t|
          file_types_set.add(FileType.from_name(t))
        end
      elsif file_types.instance_of?(Array) || file_types.instance_of?(Enumerable)
        file_types.each do |t|
          file_types_set.add(FileType.from_name(t))
        end
      end
    end

    def need_last_mod?
      @sort_by == SortBy::LASTMOD || !@max_last_mod.nil? || !@min_last_mod.nil?
    end

    def need_size?
      @sort_by == SortBy::FILESIZE || @max_size > 0 || @min_size > 0
    end

    def set_sort_by_for_name(sort_by_name)
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

    def archives_only=(archives_only)
      @archives_only = archives_only
      @include_archives = archives_only if archives_only
    end

    def debug=(debug)
      @debug = debug
      @verbose = debug if debug
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
        ', ' + set_to_s('in_archive_extensions', @in_archive_extensions) +
        ', ' + array_to_s('in_archive_file_patterns', @in_archive_file_patterns.map { |p| p.source }) +
        ', ' + array_to_s('in_dir_patterns', @in_dir_patterns.map { |p| p.source }) +
        ', ' + set_to_s('in_extensions', @in_extensions) +
        ', ' + array_to_s('in_file_patterns', @in_file_patterns.map { |p| p.source }) +
        ', ' + file_types_to_s('in_file_types', @in_file_types) +
        ", include_archives=#{@include_archives}" +
        ", include_hidden=#{@include_hidden}" +
        ", max_depth=#{@max_depth}" +
        ", max_last_mod=#{last_mod_to_s(@max_last_mod)}" +
        ", max_size=#{@max_size}" +
        ", min_depth=#{@min_depth}" +
        ", min_last_mod=#{last_mod_to_s(@min_last_mod)}" +
        ", min_size=#{@min_size}" +
        ', ' + set_to_s('out_archive_extensions', @out_archive_extensions) +
        ', ' + array_to_s('out_archive_file_patterns', @out_archive_file_patterns.map { |p| p.source }) +
        ', ' + array_to_s('out_dir_patterns', @out_dir_patterns.map { |p| p.source }) +
        ', ' + set_to_s('out_extensions', @out_extensions) +
        ', ' + array_to_s('out_file_patterns', @out_file_patterns.map { |p| p.source }) +
        ', ' + file_types_to_s('out_file_types', @out_file_types) +
        ', ' + paths_to_s('paths', @paths) +
        ", print_dirs=#{@print_dirs}" +
        ", print_files=#{@print_files}" +
        ", print_usage=#{@print_usage}" +
        ", print_version=#{@print_version}" +
        ", recursive=#{@recursive}" +
        # ", settings_only=#{@settings_only}" +
        ", sort_by=" + SortBy::to_name(@sort_by) +
        ", sort_case_insensitive=#{@sort_case_insensitive}" +
        ", sort_descending=#{@sort_descending}" +
        ", verbose=#{@verbose}" +
        ')'
    end

    private

    def array_to_s(name, arr)
      if arr.empty?
        "#{name}=[]"
      else
        "#{name}=[\"#{arr.join('", "')}\"]"
      end
    end

    def set_to_s(name, set)
      if set.empty?
        "#{name}=[]"
      else
        arr = set.to_a
        arr.sort!
        "#{name}=[\"#{arr.join('", "')}\"]"
      end
    end

    def file_types_to_s(name, file_type_set)
      s = "#{name}=["
      count = 0
      file_types = file_type_set.to_a
      file_types.sort!
      file_types.each do |ft|
        s << ', ' if count.positive?
        s << FileType.to_name(ft)
        count += 1
      end
      s + ']'
    end

    def paths_to_s(name, path_set)
      s = "#{name}=["
      count = 0
      paths = path_set.to_a
      paths.sort!
      paths.each do |p|
        s << ', ' if count.positive?
        s << p.to_s
        count += 1
      end
      s + ']'
    end
  end
end
