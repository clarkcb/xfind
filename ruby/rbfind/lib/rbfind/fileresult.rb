# frozen_string_literal: true

require 'pathname'

module RbFind
  # FindFile - encapsulates a file to be found
  class FileResult

    attr_accessor :containers
    attr_reader :path
    attr_reader :file_type
    attr_reader :file_size
    attr_reader :last_mod

    CONTAINER_SEPARATOR = '!'

    def initialize(path, file_type, file_size, last_mod)
      @containers = []
      @path = path
      @file_type = file_type # FileType
      @file_size = file_size
      @last_mod = last_mod
    end

    def dir_name
      @path.dirname.to_s
    end

    def file_name
      @path.basename.to_s
    end

    def relative_path
      @path
    end

    def to_s
      s = ''
      unless @containers.empty?
        containers = @containers.map { |c| c.to_s }
        s += containers.join(CONTAINER_SEPARATOR) + CONTAINER_SEPARATOR
      end
      s + @path.to_s
    end


    def cmp_by_path(other)
      dir_cmp = dir_name <=> other.dir_name
      if dir_cmp == 0
        file_name <=> other.file_name
      else
        dir_cmp
      end
    end

    def cmp_by_path_ci(other)
      dir_cmp = dir_name.downcase <=> other.dir_name.downcase
      if dir_cmp == 0
        file_name.downcase <=> other.file_name.downcase
      else
        dir_cmp
      end
    end

    def cmp_by_name(other)
      file_cmp = file_name <=> other.file_name
      if file_cmp == 0
        dir_name <=> other.dir_name
      else
        file_cmp
      end
    end

    def cmp_by_name_ci(other)
      file_cmp = file_name.downcase <=> other.file_name.downcase
      if file_cmp == 0
        dir_name <=> other.dir_name
      else
        file_cmp
      end
    end

    def cmp_by_size(other)
      size_cmp = file_size <=> other.file_size
      if size_cmp == 0
        cmp_by_path(other)
      else
        size_cmp
      end
    end

    def cmp_by_size_ci(other)
      size_cmp = file_size <=> other.file_size
      if size_cmp == 0
        cmp_by_path_ci(other)
      else
        size_cmp
      end
    end

    def cmp_by_type(other)
      type_cmp = file_type <=> other.file_type
      if type_cmp == 0
        cmp_by_path(other)
      else
        type_cmp
      end
    end

    def cmp_by_type_ci(other)
      type_cmp = file_type <=> other.file_type
      if type_cmp == 0
        cmp_by_path_ci(other)
      else
        type_cmp
      end
    end

    def cmp_by_last_mod(other)
      last_mod_cmp = last_mod <=> other.last_mod
      if last_mod_cmp == 0
        cmp_by_path(other)
      else
        last_mod_cmp
      end
    end

    def cmp_by_last_mod_ci(other)
      last_mod_cmp = last_mod <=> other.last_mod
      if last_mod_cmp == 0
        cmp_by_path_ci(other)
      else
        last_mod_cmp
      end
    end
  end
end
