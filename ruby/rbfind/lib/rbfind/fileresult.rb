# frozen_string_literal: true

module RbFind
  # FindFile - encapsulates a file to be found
  class FileResult

    attr_accessor :containers
    attr_reader :path
    attr_reader :filename
    attr_reader :filetype
    attr_reader :stat

    CONTAINER_SEPARATOR = '!'

    def initialize(path, filename, filetype, stat)
      @containers = []
      @path = path
      @filename = filename
      @filetype = filetype # FileType
      @stat = stat
    end

    def relativepath
      return '.' + File::SEPARATOR + @filename if @path == '.' || @path == './'

      Pathname.new(@path).join(@filename).to_s
    end

    def to_s
      s = ''
      unless @containers.empty?
        s += @containers.join(CONTAINER_SEPARATOR) + CONTAINER_SEPARATOR
      end
      s + relativepath
    end
  end
end
