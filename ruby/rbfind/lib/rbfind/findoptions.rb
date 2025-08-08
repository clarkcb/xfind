# FindOptions - generate help, create settings from CLI args
require 'date'
require 'json'
require 'pathname'

require_relative 'common'
require_relative 'finderror'
require_relative 'findoption'
require_relative 'findsettings'

module RbFind

  # FindOptions - parses CLI args into settings, generates usage string
  class FindOptions
    attr_reader :options

    def initialize
      @options = []
      @bool_action_dict = {}
      @str_action_dict = {}
      @int_action_dict = {}
      @long_arg_dict = {}
      set_actions
      set_options_from_json
      @options.sort! { |a, b| a.sort_arg <=> b.sort_arg }
    end

    def arg_hash_from_args(args)
      arg_hash = {}
      until args.empty?
        arg = args.shift
        if arg.start_with?('-')
          arg_names = []
          if arg.start_with?('--')
            if arg.length > 2
              arg_name = arg.slice(2..arg.length)
              if arg_name.include?('=')
                # TODO: split the string and save the 2nd elem back to the front of the args list
                kv = arg_name.split('=')
                arg_name = kv[0]
                args.unshift(kv[1])
              end
              arg_names << arg_name.to_sym
            else
              raise FindError, "Invalid option: #{arg}"
            end
          elsif arg.length > 1
            arg_name = arg.slice(1..arg.length)
            arg_name.each_char do |c|
              if @long_arg_dict.key?(c)
                arg_names << @long_arg_dict[c].to_sym
              else
                raise FindError, "Invalid option: #{c}"
              end
            end
          else
            raise FindError, "Invalid option: #{arg}"
          end

          arg_names.each do |arg_name|
            if @bool_action_dict.key?(arg_name)
              arg_hash[arg_name] = true
            elsif @str_action_dict.key?(arg_name) || @int_action_dict.key?(arg_name)
              raise FindError, "Missing value for option #{arg_name}" if args.empty?
              arg_val = args.shift
              if @str_action_dict.key?(arg_name)
                arg_hash[arg_name] = [] unless arg_hash.key?(arg_name)
                arg_hash[arg_name] << arg_val
              else
                arg_hash[arg_name] = arg_val.to_i
              end
            else
              raise FindError, "Invalid option: #{arg_name}"
            end
          end
        else
          arg_hash[:path] = [] unless arg_hash.key?(arg_name)
          arg_hash[:path] << arg
        end
      end
      arg_hash
    end

    def update_settings_from_arg_hash(settings, args_hash)
      # keys are sorted so that output is consistent across all versions
      arg_names = args_hash.keys.sort
      arg_names.each do |arg_name|
        if @bool_action_dict.key?(arg_name)
          if args_hash[arg_name] == true || args_hash[arg_name] == false
            @bool_action_dict[arg_name].call(args_hash[arg_name], settings)
            return if [:help, :version].include?(arg_name)
          else
            raise FindError, "Invalid value for option: #{arg_name}"
          end
        elsif @str_action_dict.key?(arg_name)
          if args_hash[arg_name].is_a?(String)
            @str_action_dict[arg_name].call(args_hash[arg_name], settings)
          elsif args_hash[arg_name].is_a?(Array)
            args_hash[arg_name].each do |v|
              if v.is_a?(String)
                @str_action_dict[arg_name].call(v, settings)
              else
                raise FindError, "Invalid value for option: #{arg_name}"
              end
            end
          else
            raise FindError, "Invalid value for option: #{arg_name}"
          end
        elsif @int_action_dict.key?(arg_name)
          if args_hash[arg_name].is_a?(Numeric)
            @int_action_dict[arg_name].call(args_hash[arg_name], settings)
          else
            raise FindError, "Invalid value for option: #{arg_name}"
          end
        else
          raise FindError, "Invalid option: #{arg_name}"
        end
      end
    end

    def update_settings_from_json(settings, json)
      json_hash = JSON.parse(json).transform_keys(&:to_sym)
      update_settings_from_arg_hash(settings, json_hash)
    end

    def update_settings_from_file(settings, file_path)
      expanded_path = Pathname.new(file_path).expand_path
      unless expanded_path.exist?
        raise FindError, "Settings file not found: #{file_path}"
      end
      unless expanded_path.extname == '.json'
        raise FindError, "Invalid settings file (must be JSON): #{file_path}"
      end
      f = File.open(expanded_path.to_s, mode: 'r')
      json = f.read
      update_settings_from_json(settings, json)
    rescue IOError => e
      raise FindError, e.to_s
    rescue ArgumentError => e
      raise FindError, e.to_s
    rescue FindError => e
      raise FindError, e.to_s
    rescue JSON::ParserError => e
      raise FindError, "Unable to parse JSON in settings file: #{file_path}"
    ensure
      f&.close
    end

    def update_settings_from_args(settings, args)
      # default print_files to true since running as cli
      settings.print_files = true
      args_hash = arg_hash_from_args(args)
      update_settings_from_arg_hash(settings, args_hash)
    end

    def find_settings_from_args(args)
      settings = FindSettings.new
      update_settings_from_args(settings, args)
      settings
    end

    def get_usage_string
      usage = ["Usage:\n", " rbfind [options] <path> [<path> ...]\n\n", "Options:\n"]
      opt_strings = []
      opt_descs = []
      longest = 0
      @options.each do |opt|
        if opt.short_arg.empty?
          opt_string = "--#{opt.long_arg}"
        else
          opt_string = "-#{opt.short_arg},--#{opt.long_arg}"
        end
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
      usage.join
    end

    def usage
      puts "#{get_usage_string}\n"
      abort
    end

    private

    def set_actions
      @bool_action_dict = {
        archivesonly: ->(b, settings) { settings.archives_only = b },
        caseinsensitive: ->(b, settings) { settings.sort_case_insensitive = b },
        casesensitive: ->(b, settings) { settings.sort_case_insensitive = !b },
        colorize: ->(b, settings) { settings.colorize = b },
        debug: ->(b, settings) { settings.debug = b },
        excludearchives: ->(b, settings) { settings.include_archives = !b },
        excludehidden: ->(b, settings) { settings.include_hidden = !b },
        followsymlinks: ->(b, settings) { settings.follow_symlinks = b },
        help: ->(b, settings) { settings.print_usage = b },
        includearchives: ->(b, settings) { settings.include_archives = b },
        includehidden: ->(b, settings) { settings.include_hidden = b },
        nocolorize: ->(b, settings) { settings.colorize = !b },
        nofollowsymlinks: ->(b, settings) { settings.follow_symlinks = !b },
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
      @str_action_dict = {
        'in-archiveext': ->(s, settings) { settings.add_exts(s, settings.in_archive_extensions) },
        'in-archivefilepattern': ->(s, settings) { settings.add_patterns(s, settings.in_archive_file_patterns) },
        'in-dirpattern': ->(s, settings) { settings.add_patterns(s, settings.in_dir_patterns) },
        'in-ext': ->(s, settings) { settings.add_exts(s, settings.in_extensions) },
        'in-filetype': ->(s, settings) { settings.add_file_types(s, settings.in_file_types) },
        'in-filepattern': ->(s, settings) { settings.add_patterns(s, settings.in_file_patterns) },
        maxlastmod: ->(s, settings) { settings.max_last_mod = DateTime.parse(s) },
        minlastmod: ->(s, settings) { settings.min_last_mod = DateTime.parse(s) },
        'out-archiveext': ->(s, settings) { settings.add_exts(s, settings.out_archive_extensions) },
        'out-archivefilepattern': ->(s, settings) { settings.add_patterns(s, settings.out_archive_file_patterns) },
        'out-dirpattern': ->(s, settings) { settings.add_patterns(s, settings.out_dir_patterns) },
        'out-ext': ->(s, settings) { settings.add_exts(s, settings.out_extensions) },
        'out-filepattern': ->(s, settings) { settings.add_patterns(s, settings.out_file_patterns) },
        'out-filetype': ->(s, settings) { settings.add_file_types(s, settings.out_file_types) },
        path: ->(s, settings) { settings.add_path(s) },
        'settings-file': ->(s, settings) { update_settings_from_file(settings, s) },
        'sort-by': ->(s, settings) { settings.set_sort_by_for_name(s) }
      }
      @int_action_dict = {
        maxdepth: ->(i, settings) { settings.max_depth = i },
        maxsize: ->(i, settings) { settings.max_size = i },
        mindepth: ->(i, settings) { settings.min_depth = i },
        minsize: ->(i, settings) { settings.min_size = i },
      }
      @long_arg_dict = {"path" => :path}
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
