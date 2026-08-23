# frozen_string_literal: true

module RbFind

  # FindConfig - basic config info
  class FindConfig
    attr_reader :file_types_path
    attr_reader :find_options_path
    attr_reader :default_find_settings_path

    def initialize
      @file_types_path = File.realpath(File.join(File.dirname(__FILE__), "../../data/filetypes.json"))
      @find_options_path = File.realpath(File.join(File.dirname(__FILE__), "../../data/findoptions.json"))
      @default_find_settings_path = File.join(ENV['HOME'], '.config', 'xfind', 'settings.json')
    end
  end
end
