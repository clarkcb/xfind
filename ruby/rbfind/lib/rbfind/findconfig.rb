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
      default_xfind_config_dir = File.join(ENV['HOME'], '.config', 'xfind')
      xfind_config_dir = ENV.fetch('XFIND_CONFIG_DIR', default_xfind_config_dir)
      @default_find_settings_path = File.join(xfind_config_dir, 'settings.json')
    end
  end
end
