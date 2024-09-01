# FindOptions - generate help, create settings from CLI args
require 'date'
require 'json'
require 'pathname'

require_relative 'common'
require_relative 'config'
require_relative 'finderror'
require_relative 'findoption'
require_relative 'findsettings'

module RbFind

  # FindOptions - parses CLI args into settings, generates usage string
  class FindOptions
    attr_reader :options

    def initialize
      @options = []
      @arg_action_dict = {}
      @bool_flag_action_dict = {}
      @long_arg_dict = {}
      set_actions
      set_options_from_json
      # set_options_from_xml
      @options.sort! { |a, b| a.sort_arg <=> b.sort_arg }
    end

    def find_settings_from_args(args)
      settings = FindSettings.new
      # default print_files to true since running as cli
      settings.print_files = true
      until args.empty?
        arg = args.shift
        if arg.start_with?('-')
          arg = arg[1..arg.length] while arg && arg.start_with?('-')
          long_arg = @long_arg_dict[arg]
          if @arg_action_dict.key?(long_arg)
            raise FindError, "Missing value for option #{arg}" if args.empty?
            arg_val = args.shift
            @arg_action_dict[long_arg].call(arg_val, settings)
          elsif @bool_flag_action_dict.key?(long_arg)
            @bool_flag_action_dict[long_arg].call(true, settings)
            return settings if %w[help version].include?(long_arg)
          else
            raise FindError, "Invalid option: #{arg}"
          end
        else
          settings.add_path(arg)
        end
      end
      settings
    end

    def settings_from_file(file_path, settings)
      f = File.open(file_path, mode: 'r')
      json = f.read
      settings_from_json(json, settings)
    rescue IOError => e
      raise FindError, "#{e} (file: #{file_path})"
    rescue ArgumentError => e
      raise FindError, "#{e} (file: #{file_path})"
    rescue FindError => e
      raise FindError, "#{e} (file: #{file_path})"
    ensure
      f&.close
    end

    def settings_from_json(json, settings)
      json_hash = JSON.parse(json)
      json_hash.each_key do |arg|
        arg_sym = arg.to_sym
        if @arg_action_dict.key?(arg_sym)
          @arg_action_dict[arg_sym].call(json_hash[arg], settings)
        elsif @bool_flag_action_dict.key?(arg_sym)
          @bool_flag_action_dict[arg_sym].call(json_hash[arg], settings)
          return if %w[h help V version].include?(arg)
        elsif arg == 'path'
          settings.paths.push(Pathname.new(arg))
        else
          raise FindError, "Invalid option: #{arg}"
        end
      end
    end

    def usage
      puts "#{get_usage_string}\n"
      abort
    end

    def get_usage_string
      usage = "Usage:\n"
      usage << " rbfind [options] <path> [<path> ...]\n\n"
      usage << "Options:\n"
      opt_strings = []
      opt_descs = []
      longest = 0
      @options.each do |opt|
        opt_string = ''
        opt_string << "-#{opt.short_arg}," unless opt.short_arg.empty?
        opt_string << "--#{opt.long_arg}"
        longest = opt_string.length > longest ? opt_string.length : longest
        opt_strings.push(opt_string)
        opt_descs.push(opt.desc)
      end
      format_string = " %-#{longest}s  %s\n"
      i = 0
      while i < opt_strings.size
        usage << format(format_string, opt_strings[i], opt_descs[i])
        i += 1
      end
      usage
    end

    private

    def set_actions
      @arg_action_dict = {
        'in-archiveext': lambda { |x, settings|
          settings.add_exts(x, settings.in_archive_extensions)
        },
        'in-archivefilepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_archive_file_patterns)
        },
        'in-dirpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_dir_patterns)
        },
        'in-ext': lambda { |x, settings|
          settings.add_exts(x, settings.in_extensions)
        },
        'in-filetype': lambda { |x, settings|
          settings.add_file_types(x, settings.in_file_types)
        },
        'in-filepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_file_patterns)
        },
        'maxdepth': lambda { |x, settings|
          settings.max_depth = x.to_i
        },
        'maxlastmod': lambda { |x, settings|
          settings.max_last_mod = DateTime.parse(x)
        },
        'maxsize': lambda { |x, settings|
          settings.max_size = x.to_i
        },
        'mindepth': lambda { |x, settings|
          settings.min_depth = x.to_i
        },
        'minlastmod': lambda { |x, settings|
          settings.min_last_mod = DateTime.parse(x)
        },
        'minsize': lambda { |x, settings|
          settings.min_size = x.to_i
        },
        'out-archiveext': lambda { |x, settings|
          settings.add_exts(x, settings.out_archive_extensions)
        },
        'out-archivefilepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_archive_file_patterns)
        },
        'out-dirpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_dir_patterns)
        },
        'out-ext': lambda { |x, settings|
          settings.add_exts(x, settings.out_extensions)
        },
        'out-filepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_file_patterns)
        },
        'out-filetype': lambda { |x, settings|
          settings.add_file_types(x, settings.out_file_types)
        },
        'path': lambda { |x, settings|
          settings.add_path(x)
        },
        'settings-file': lambda { |x, settings|
          settings_from_file(x, settings)
        },
        'sort-by': lambda { |x, settings|
          settings.set_sort_by_for_name(x)
        }
      }
      @bool_flag_action_dict = {
        archivesonly: ->(b, settings) { settings.archives_only = b },
        caseinsensitive: ->(b, settings) { settings.sort_case_insensitive = b },
        casesensitive: ->(b, settings) { settings.sort_case_insensitive = !b },
        debug: ->(b, settings) { settings.debug = b },
        excludearchives: ->(b, settings) { settings.include_archives = !b },
        excludehidden: ->(b, settings) { settings.include_hidden = !b },
        help: ->(b, settings) { settings.print_usage = b },
        includearchives: ->(b, settings) { settings.include_archives = b },
        includehidden: ->(b, settings) { settings.include_hidden = b },
        noprintdirs: ->(b, settings) { settings.print_dirs = !b },
        noprintfiles: ->(b, settings) { settings.print_files = !b },
        norecursive: ->(b, settings) { settings.recursive = !b },
        printdirs: ->(b, settings) { settings.print_dirs = b },
        printfiles: ->(b, settings) { settings.print_files = b },
        recursive: ->(b, settings) { settings.recursive = b },
        'sort-ascending': ->(b, settings) { settings.sort_descending = !b },
        'sort-caseinsensitive': ->(b, settings) { settings.sort_case_insensitive = b },
        'sort-casesensitive': ->(b, settings) { settings.sort_case_insensitive = !b },
        'sort-descending': ->(b, settings) { settings.sort_descending = b },
        verbose: ->(b, settings) { settings.verbose = b },
        version: ->(b, settings) { settings.print_version = b }
      }
      @long_arg_dict = {}
    end

    def set_options_from_json
      find_options_json_path = File.join(File.dirname(__FILE__), "../../data/findoptions.json")
      f = File.open(find_options_json_path, mode: 'r')
      json = f.read
      json_hash = JSON.parse(json)
      json_hash['findoptions'].each do |so|
        long = so['long']
        short =
          if so.key?('short')
            so['short']
          else
            ''
          end
        desc = so['desc']
        long_sym = long.to_sym
        @options.push(FindOption.new(short, long, desc))
        @long_arg_dict[long] = long_sym
        @long_arg_dict[short] = long_sym if short
      end
    rescue StandardError => e
      raise FindError, "#{e} (file: #{find_options_json_path})"
    ensure
      f&.close
    end
  end
end
