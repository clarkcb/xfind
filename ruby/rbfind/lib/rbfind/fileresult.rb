# frozen_string_literal: true

module RbFind
  # FindFile - encapsulates a file to be found
  class FileResult

    attr_accessor :containers
    attr_reader :path
    attr_reader :file_name
    attr_reader :file_type
    attr_reader :file_size
    attr_reader :last_mod

    CONTAINER_SEPARATOR = '!'

    def initialize(path, file_name, file_type, file_size, last_mod)
      @containers = []
      @path = path
      @file_name = file_name
      @file_type = file_type # FileType
      @file_size = file_size
      @last_mod = last_mod
    end

    def relative_path
      return '.' + File::SEPARATOR + @file_name if @path == '.' || @path == './'

      Pathname.new(@path).join(@file_name).to_s
    end

    def to_s
      s = ''
      unless @containers.empty?
        s += @containers.join(CONTAINER_SEPARATOR) + CONTAINER_SEPARATOR
      end
      s + relative_path
    end
  end
end
