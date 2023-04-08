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

    attr_reader :archivesonly
    attr_reader :debug
    attr_accessor :excludehidden
    attr_accessor :in_archiveextensions
    attr_accessor :in_archivefilepatterns
    attr_accessor :in_dirpatterns
    attr_accessor :in_extensions
    attr_accessor :in_filepatterns
    attr_accessor :in_filetypes
    attr_accessor :includearchives
    attr_accessor :listdirs
    attr_accessor :listfiles
    attr_accessor :maxlastmod
    attr_accessor :maxsize
    attr_accessor :minlastmod
    attr_accessor :minsize
    attr_accessor :out_archiveextensions
    attr_accessor :out_archivefilepatterns
    attr_accessor :out_dirpatterns
    attr_accessor :out_extensions
    attr_accessor :out_filepatterns
    attr_accessor :out_filetypes
    attr_accessor :printusage
    attr_accessor :printversion
    attr_accessor :recursive
    attr_accessor :paths
    attr_accessor :sortby
    attr_accessor :sort_caseinsensitive
    attr_accessor :sort_descending
    attr_accessor :verbose

    def initialize
      @archivesonly = false
      @debug = false
      @excludehidden = true
      @includearchives = false
      @listdirs = false
      @listfiles = false
      @listfiles = false
      @maxlastmod = nil
      @maxsize = 0
      @minlastmod = nil
      @minsize = 0
      @printusage = false
      @printversion = false
      @recursive = true
      @sortby = SortBy::FILEPATH
      @sort_caseinsensitive = false
      @sort_descending = false
      @verbose = false

      @in_archiveextensions = []
      @in_archivefilepatterns = []
      @in_dirpatterns = []
      @in_extensions = []
      @in_filepatterns = []
      @in_filetypes = []
      @out_archiveextensions = []
      @out_archivefilepatterns = []
      @out_dirpatterns = []
      @out_extensions = []
      @out_filepatterns = []
      @out_filetypes = []
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

    def add_filetypes(filetypes, filetypes_set)
      if filetypes.instance_of? String
        filetypes.split(',').each do |t|
          filetypes_set.push(FileTypes.from_name(t))
        end
      elsif filetypes.instance_of? Array
        filetypes.each do |t|
          filetypes_set.push(FileTypes.from_name(t))
        end
      end
    end

    def need_stat?
      if @sortby == SortBy::FILESIZE || @sortby == SortBy::LASTMOD
        return true
      end
      if @maxlastmod || @maxsize > 0 || @minlastmod || @minsize > 0
        return true
      end
      false
    end

    def set_sort_by(sort_by_name)
      sort_by_name = sort_by_name.strip.upcase
      if sort_by_name == 'NAME'
        @sortby = SortBy::FILENAME
      elsif sort_by_name == 'SIZE'
        @sortby = SortBy::FILESIZE
      elsif sort_by_name == 'TYPE'
        @sortby = SortBy::FILETYPE
      elsif sort_by_name == 'LASTMOD'
        @sortby = SortBy::LASTMOD
      else
        @sortby = SortBy::FILEPATH
      end
    end

    def archivesonly=(bool)
      @archivesonly = bool
      @includearchives = bool if bool
    end

    def debug=(bool)
      @debug = bool
      @verbose = bool if bool
    end

    def to_s
      s = 'FindSettings('
      s << "archivesonly: #{@archivesonly}"
      s << ", debug: #{@debug}"
      s << ", excludehidden: #{@excludehidden}"
      s << ', ' + list_to_s('in_archiveextensions', @in_archiveextensions)
      s << ', ' + list_to_s('in_archivefilepatterns', @in_archivefilepatterns)
      s << ', ' + list_to_s('in_dirpatterns', @in_dirpatterns)
      s << ', ' + list_to_s('in_extensions', @in_extensions)
      s << ', ' + list_to_s('in_filepatterns', @in_filepatterns)
      s << ', ' + filetypes_to_s('in_filetypes', @in_filetypes)
      s << ", includearchives: #{@includearchives}"
      s << ", listdirs: #{@listdirs}"
      s << ", listfiles: #{@listfiles}"
      s << ", maxlastmod: #{@maxlastmod}"
      s << ", maxsize: #{@maxsize}"
      s << ", minlastmod: #{@minlastmod}"
      s << ", minsize: #{@minsize}"
      s << ', ' + list_to_s('out_archiveextensions', @out_archiveextensions)
      s << ', ' + list_to_s('out_archivefilepatterns', @out_archivefilepatterns)
      s << ', ' + list_to_s('out_dirpatterns', @out_dirpatterns)
      s << ', ' + list_to_s('out_extensions', @out_extensions)
      s << ', ' + list_to_s('out_filepatterns', @out_filepatterns)
      s << ', ' + filetypes_to_s('out_filetypes', @out_filetypes)
      s << ', ' + list_to_s('paths', @paths)
      s << ", printusage: #{@printusage}"
      s << ", printversion: #{@printversion}"
      s << ", recursive: #{@recursive}"
      s << ", sortby: #{@sortby}"
      s << ", sort_caseinsensitive: #{@sort_caseinsensitive}"
      s << ", sort_descending: #{@sort_descending}"
      s << ", verbose: #{@verbose}"
      s << ')'
      s
    end

    private

    def list_to_s(name, lst)
      "#{name}=[\"#{lst.join('", "')}\"]"
    end

    def filetypes_to_s(name, filetypes)
      s = "#{name}=["
      count = 0
      filetypes.each do |ft|
        s << ', ' if count.positive?
        s << "\"#{FileTypes.to_name(ft)}\""
        count += 1
      end
      s + ']'
    end
  end
end
