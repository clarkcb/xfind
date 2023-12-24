# frozen_string_literal: true

require 'json'
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

    NAMES = %w[unknown archive audio binary code font image text video xml].freeze

    module_function
    def from_name(name)
      idx = NAMES.index(name.downcase)
      idx.nil? ? 0 : idx
    end

    def to_name(file_type)
      file_type < NAMES.size ? NAMES[file_type] : NAMES[0]
    end
  end

  # FileTypes - provides basic file type information
  class FileTypes

    def initialize
      set_file_type_maps_from_json
    end

    def set_file_type_maps_from_json
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

    def get_file_type(file_name)
      # more specific first
      if code_file?(file_name)
        FileType::CODE
      elsif archive_file?(file_name)
        FileType::ARCHIVE
      elsif audio_file?(file_name)
        FileType::AUDIO
      elsif font_file?(file_name)
        FileType::FONT
      elsif image_file?(file_name)
        FileType::IMAGE
      elsif video_file?(file_name)
        FileType::VIDEO

      # more general last
      elsif xml_file?(file_name)
        FileType::XML
      elsif text_file?(file_name)
        FileType::TEXT
      elsif binary_file?(file_name)
        FileType::BINARY
      else
        FileType::UNKNOWN
      end
    end

    def archive_file?(file_name)
      @file_type_name_map['archive'].include?(file_name) ||
      @file_type_ext_map['archive'].include?(FileUtil.get_extension(file_name))
    end

    def audio_file?(file_name)
      @file_type_name_map['audio'].include?(file_name) ||
      @file_type_ext_map['audio'].include?(FileUtil.get_extension(file_name))
    end

    def binary_file?(file_name)
      @file_type_name_map['binary'].include?(file_name) ||
      @file_type_ext_map['binary'].include?(FileUtil.get_extension(file_name))
    end

    def code_file?(file_name)
      @file_type_name_map['code'].include?(file_name) ||
      @file_type_ext_map['code'].include?(FileUtil.get_extension(file_name))
    end

    def font_file?(file_name)
      @file_type_name_map['font'].include?(file_name) ||
      @file_type_ext_map['font'].include?(FileUtil.get_extension(file_name))
    end

    def image_file?(file_name)
      @file_type_name_map['image'].include?(file_name) ||
      @file_type_ext_map['image'].include?(FileUtil.get_extension(file_name))
    end

    def text_file?(file_name)
      @file_type_name_map['text'].include?(file_name) ||
      @file_type_ext_map['text'].include?(FileUtil.get_extension(file_name))
    end

    def video_file?(file_name)
      @file_type_name_map['video'].include?(file_name) ||
      @file_type_ext_map['video'].include?(FileUtil.get_extension(file_name))
    end

    def xml_file?(file_name)
      @file_type_name_map['xml'].include?(file_name) ||
      @file_type_ext_map['xml'].include?(FileUtil.get_extension(file_name))
    end
  end
end
