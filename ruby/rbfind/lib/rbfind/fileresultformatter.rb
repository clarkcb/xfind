# frozen_string_literal: true

################################################################################
#
# fileresultformatter.rb
#
# Formatter for file results
#
################################################################################

require 'pathname'

require_relative 'color'

module RbFind
  # FileResultFormatter - formats file result instances
  class FileResultFormatter

    attr_reader :settings

    def initialize(settings)
      @settings = settings
      if settings.colorize
        if settings.in_dir_patterns
          # override format_dir_path method in instance with colorizing method
          define_singleton_method(:format_dir_path) { |dir_path| format_dir_path_with_color(dir_path) }
        end
        if settings.in_extensions || settings.in_file_patterns
          # override format_file_name method in instance with colorizing method
          define_singleton_method(:format_file_name) { |file_name| format_file_name_with_color(file_name) }
        end
      end
    end

    def colorize(str, match_start_index, match_end_index)
      prefix = ''
      if match_start_index > 0
        prefix = str[0..(match_start_index - 1)]
      end
      colorized = str[match_start_index..(match_end_index - 1)].green
      suffix = ''
      if match_end_index < str.length
        suffix = str[match_end_index..str.length]
      end
      prefix + colorized + suffix
    end

    def format_dir_path(dir_path)
      dir_path.to_s
    end

    def format_file_name(file_name)
      file_name.to_s
    end

    def format_path(path)
      parent = '.'
      unless path.parent.nil?
        parent = format_dir_path(path.parent)
      end
      file_name = format_file_name(path.basename)
      Pathname(parent).join(file_name).to_s
    end

    def format_file_result(file_result)
      format_path(file_result.path)
    end

    private

    def format_dir_path_with_color(dir_path)
      formatted_dir = dir_path.to_s
      @settings.in_dir_patterns.each do |p|
        m = p.match(formatted_dir)
        if m
          formatted_dir = colorize(formatted_dir, m.begin(0), m.end(0))
          break
        end
      end
      formatted_dir
    end

    def format_file_name_with_color(file_name)
      formatted_file_name = file_name.to_s
      @settings.in_file_patterns.each do |p|
        m = p.match(formatted_file_name)
        if m
          formatted_file_name = colorize(formatted_file_name, m.begin(0), m.end(0))
          break
        end
      end
      unless @settings.in_extensions.empty? or file_name.extname.empty?
        idx = formatted_file_name.rindex('.')
        if idx > 0 && idx < formatted_file_name.length - 1
          formatted_file_name = colorize(formatted_file_name, idx + 1, formatted_file_name.length)
        end
      end
      formatted_file_name
    end

  end
end
