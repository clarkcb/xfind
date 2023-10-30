# FindOptions - generate help, create settings from CLI args
require 'date'
require 'json'

require_relative 'common'
require_relative 'finderror'
require_relative 'findoption'
require_relative 'findsettings'

module RbFind

  # FindOptions - parses CLI args into settings, generates usage string
  class FindOptions

    def initialize
      @options = []
      @arg_action_dict = {}
      @bool_flag_action_dict = {}
      @longarg_dict = {}
      set_actions
      set_options_from_json
      # set_options_from_xml
      @options.sort! { |a, b| a.sortarg <=> b.sortarg }
    end

    def find_settings_from_args(args)
      settings = FindSettings.new
      # default list_files to true since running as cli
      settings.list_files = true
      until args.empty?
        arg = args.shift
        if arg.start_with?('-')
          arg = arg[1..arg.length] while arg && arg.start_with?('-')
          longarg = @longarg_dict[arg]
          if @arg_action_dict.key?(longarg)
            raise FindError, "Missing value for option #{arg}" if args.empty?
            argval = args.shift
            @arg_action_dict[longarg].call(argval, settings)
          elsif @bool_flag_action_dict.key?(longarg)
            @bool_flag_action_dict[longarg].call(true, settings)
            return settings if %w[help version].include?(longarg)
          else
            raise FindError, "Invalid option: #{arg}"
          end
        else
          settings.paths.push(arg)
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
          settings.paths.push(arg)
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
        opt_string << "-#{opt.shortarg}," unless opt.shortarg.empty?
        opt_string << "--#{opt.longarg}"
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
          settings.paths.push(x)
        },
        'settings-file': lambda { |x, settings|
          settings_from_file(x, settings)
        },
        'sort-by': lambda { |x, settings|
          settings.set_sort_by(x)
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
        listdirs: ->(b, settings) { settings.list_dirs = b },
        listfiles: ->(b, settings) { settings.list_files = b },
        norecursive: ->(b, settings) { settings.recursive = !b },
        recursive: ->(b, settings) { settings.recursive = b },
        'sort-ascending': ->(b, settings) { settings.sort_descending = !b },
        'sort-caseinsensitive': ->(b, settings) { settings.sort_case_insensitive = b },
        'sort-casesensitive': ->(b, settings) { settings.sort_case_insensitive = !b },
        'sort-descending': ->(b, settings) { settings.sort_descending = b },
        verbose: ->(b, settings) { settings.verbose = b },
        version: ->(b, settings) { settings.print_version = b }
      }
      @longarg_dict = {}
    end

    def set_options_from_json
      findoptions_json_path = File.join(File.dirname(__FILE__), "../../data/findoptions.json")
      f = File.open(findoptions_json_path, mode: 'r')
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
        func =
          if @arg_action_dict.key?(long_sym)
            @arg_action_dict[long_sym]
          elsif @bool_flag_action_dict.key?(long_sym)
            @bool_flag_action_dict[long_sym]
          else
            raise FindError, "Unknown find option: #{long}"
          end
        @options.push(FindOption.new(short, long, desc, func))
        @longarg_dict[long] = long_sym
        @longarg_dict[short] = long_sym if short
      end
    rescue StandardError => e
      raise FindError, "#{e} (file: #{findoptions_json_path})"
    ensure
      f&.close
    end
  end
end
