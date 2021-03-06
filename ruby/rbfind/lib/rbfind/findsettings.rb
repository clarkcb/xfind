require_relative 'filetypes'

module RbFind

  # FindSettings - encapsulates find settings
  class FindSettings

    attr_reader :archivesonly
    attr_accessor :colorize
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
    attr_accessor :verbose

    def initialize
      @archivesonly = false
      @colorize = true
      @debug = false
      @excludehidden = true
      @includearchives = false
      @listdirs = false
      @listfiles = false
      @printusage = false
      @printversion = false
      @recursive = true
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
