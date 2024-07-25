# frozen_string_literal: true

require 'json'
require 'sqlite3'

require_relative 'config'
require_relative 'fileutil'
require_relative 'finderror'

module RbFind

  module FileType
    UNKNOWN = 0
    ARCHIVE = 1
    AUDIO   = 2
    BINARY  = 3
    CODE    = 4
    FONT    = 5
    IMAGE   = 6
    TEXT    = 7
    VIDEO   = 8
    XML     = 9

    NAMES = Array[:unknown, :archive, :audio, :binary, :code, :font, :image, :text, :video, :xml].freeze

    module_function
    def from_name(name)
      idx = NAMES.index(name.downcase.to_sym)
      idx.nil? ? 0 : idx
    end

    def from_index(idx)
      if idx < 0 || idx > NAMES.size
        return 0
      end
      idx
    end

    def from_sym(sym)
      idx = NAMES.index(sym)
      idx.nil? ? 0 : idx
    end

    def to_name(file_type)
      file_type < NAMES.size ? NAMES[file_type].to_s : NAMES[0].to_s
    end

    def to_sym(file_type)
      file_type < NAMES.size ? NAMES[file_type] : NAMES[0]
    end
  end

  # FileTypes - provides basic file type information
  class FileTypes

    def initialize
      @db = SQLite3::Database.open XFINDDB, readonly: true, results_as_hash: true
      @ext_type_cache = {}
    end

    def get_file_type_for_file_name(file_name)
      results = @db.query "SELECT file_type_id FROM file_name WHERE name=?", file_name
      result = results.next
      if result
        file_type_id = result['file_type_id'] - 1
        return FileType.from_index(file_type_id)
      end
      FileType::UNKNOWN
    end

    def get_file_type_for_file_extension(file_ext)
      if @ext_type_cache.has_key?(file_ext)
        return @ext_type_cache[file_ext]
      end
      results = @db.query "SELECT file_type_id FROM file_extension WHERE extension=?", file_ext
      result = results.next
      if result
        file_type_id = result['file_type_id'] - 1
        file_type = FileType.from_index(file_type_id)
        @ext_type_cache[file_ext] = file_type
        return file_type
      end
      FileType::UNKNOWN
    end

    def get_file_type(file_path)
      file_type_for_file_name = get_file_type_for_file_name(file_path.basename.to_s)
      if file_type_for_file_name != FileType::UNKNOWN
        return file_type_for_file_name
      end
      get_file_type_for_file_extension(FileUtil.get_extension(file_path))
    end

    def archive_file?(file_path)
      get_file_type(file_path) == FileType::ARCHIVE
    end

    def audio_file?(file_path)
      get_file_type(file_path) == FileType::AUDIO
    end

    def binary_file?(file_path)
      get_file_type(file_path) == FileType::BINARY
    end

    def code_file?(file_path)
      get_file_type(file_path) == FileType::CODE
    end

    def font_file?(file_path)
      get_file_type(file_path) == FileType::FONT
    end

    def image_file?(file_path)
      get_file_type(file_path) == FileType::IMAGE
    end

    def text_file?(file_path)
      file_type = get_file_type(file_path)
      file_type == FileType::TEXT || file_type == FileType::CODE || file_type == FileType::XML
    end

    def video_file?(file_path)
      get_file_type(file_path) == FileType::VIDEO
    end

    def xml_file?(file_path)
      get_file_type(file_path) == FileType::XML
    end
  end
end
