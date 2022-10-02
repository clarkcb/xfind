# frozen_string_literal: true

require 'json'
require_relative 'fileutil'
require_relative 'finderror'

module RbFind

  module FileType
    UNKNOWN = 0
    ARCHIVE = 1
    BINARY  = 2
    CODE    = 3
    TEXT    = 4
    XML     = 5
  end

  # FileTypes - provides basic file type information
  class FileTypes
    FILE_TYPE_NAMES = %w[UNKNOWN ARCHIVE BINARY CODE TEXT XML].freeze

    def initialize
      set_filetype_maps_from_json
    end

    def self.from_name(name)
      idx = FILE_TYPE_NAMES.index(name.upcase)
      idx.nil? ? 0 : idx
    end

    def self.to_name(filetype)
      filetype < FILE_TYPE_NAMES.size ? FILE_TYPE_NAMES[filetype] : 0
    end

    def set_filetype_maps_from_json
      @file_type_ext_map = {}
      @file_type_name_map = {}
      filetypes_json_path = File.join(File.dirname(__FILE__), "../../data/filetypes.json")
      f = File.open(filetypes_json_path, mode: 'r')
      json = f.read
      json_hash = JSON.parse(json)
      json_hash['filetypes'].each do |ft|
        typename = ft['type']
        exts = ft['extensions'].to_set
        @file_type_ext_map[typename] = exts
        names = ft['names'].to_set
        @file_type_name_map[typename] = names
      end
      @file_type_ext_map['text'] = @file_type_ext_map['text'] + @file_type_ext_map['code'] +
        @file_type_ext_map['xml']
      @file_type_name_map['text'] = @file_type_name_map['text'] + @file_type_name_map['code'] +
        @file_type_name_map['xml']
    rescue StandardError => e
      raise FindError, "#{e}"
    ensure
      f&.close
    end

    def get_filetype(filename)
      if code_file?(filename)
        FileType::CODE
      elsif xml_file?(filename)
        FileType::XML
      elsif text_file?(filename)
        FileType::TEXT
      elsif binary_file?(filename)
        FileType::BINARY
      elsif archive_file?(filename)
        FileType::ARCHIVE
      else
        FileType::UNKNOWN
      end
    end

    def archive_file?(filename)
      @file_type_name_map['archive'].include?(filename) ||
      @file_type_ext_map['archive'].include?(FileUtil.get_extension(filename))
    end

    def binary_file?(filename)
      @file_type_name_map['binary'].include?(filename) ||
      @file_type_ext_map['binary'].include?(FileUtil.get_extension(filename))
    end

    def code_file?(filename)
      @file_type_name_map['code'].include?(filename) ||
      @file_type_ext_map['code'].include?(FileUtil.get_extension(filename))
    end

    def text_file?(filename)
      @file_type_name_map['text'].include?(filename) ||
      @file_type_ext_map['text'].include?(FileUtil.get_extension(filename))
    end

    def xml_file?(filename)
      @file_type_name_map['xml'].include?(filename) ||
      @file_type_ext_map['xml'].include?(FileUtil.get_extension(filename))
    end
  end
end
