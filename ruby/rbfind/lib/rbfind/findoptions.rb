# FindOptions - generate help, create settings from CLI args
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
      # default listfiles to true since running as cli
      settings.listfiles = true
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

    def settings_from_file(filepath, settings)
      f = File.open(filepath, mode: 'r')
      json = f.read
      settings_from_json(json, settings)
    rescue IOError => e
      raise FindError, "#{e} (file: #{filepath})"
    rescue ArgumentError => e
      raise FindError, "#{e} (file: #{filepath})"
    rescue FindError => e
      raise FindError, "#{e} (file: #{filepath})"
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
          settings.add_exts(x, settings.in_archiveextensions)
        },
        'in-archivefilepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_archivefilepatterns)
        },
        'in-dirpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_dirpatterns)
        },
        'in-ext': lambda { |x, settings|
          settings.add_exts(x, settings.in_extensions)
        },
        'in-filetype': lambda { |x, settings|
          settings.add_filetypes(x, settings.in_filetypes)
        },
        'in-filepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.in_filepatterns)
        },
        'maxlastmod': lambda { |x, settings|
          # TODO: add maxlastmod to settings as date/time
        },
        'maxsize': lambda { |x, settings|
          # TODO: add maxsize to settings as int
        },
        'minlastmod': lambda { |x, settings|
          # TODO: add minlastmod to settings as date/time
        },
        'minsize': lambda { |x, settings|
          # TODO: add minsize to settings as int
        },
        'out-archiveext': lambda { |x, settings|
          settings.add_exts(x, settings.out_archiveextensions)
        },
        'out-archivefilepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_archivefilepatterns)
        },
        'out-dirpattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_dirpatterns)
        },
        'out-ext': lambda { |x, settings|
          settings.add_exts(x, settings.out_extensions)
        },
        'out-filepattern': lambda { |x, settings|
          settings.add_patterns(x, settings.out_filepatterns)
        },
        'out-filetype': lambda { |x, settings|
          settings.add_filetypes(x, settings.out_filetypes)
        },
        'path': lambda { |x, settings|
          settings.paths.push(x)
        },
        'settings-file': lambda { |x, settings|
          settings_from_file(x, settings)
        },
        'sort-by': lambda { |x, settings|
          settings_from_file(x, settings)
        }
      }
      @bool_flag_action_dict = {
        archivesonly: ->(b, settings) { settings.archivesonly = b },
        caseinsensitive: ->(b, settings) { settings.casesensitive = !b },
        casesensitive: ->(b, settings) { settings.casesensitive = b },
        debug: ->(b, settings) { settings.debug = b },
        excludearchives: ->(b, settings) { settings.includearchives = !b },
        excludehidden: ->(b, settings) { settings.excludehidden = b },
        help: ->(b, settings) { settings.printusage = b },
        includearchives: ->(b, settings) { settings.includearchives = b },
        includehidden: ->(b, settings) { settings.excludehidden = !b },
        listdirs: ->(b, settings) { settings.listdirs = b },
        listfiles: ->(b, settings) { settings.listfiles = b },
        norecursive: ->(b, settings) { settings.recursive = !b },
        recursive: ->(b, settings) { settings.recursive = b },
        'sort-descending': ->(b, settings) { settings.sort_descending = b },
        verbose: ->(b, settings) { settings.verbose = b },
        version: ->(b, settings) { settings.printversion = b }
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
