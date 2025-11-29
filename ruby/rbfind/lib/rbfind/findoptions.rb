# FindOptions - generate help, create settings from CLI args
require 'date'
require 'json'
require 'pathname'

require_relative 'arg_tokenizer'
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
      set_actions
      set_options_from_json
      @arg_tokenizer = ArgTokenizer.new(@options)
    end

    def update_settings_from_json(settings, json)
      arg_tokens = @arg_tokenizer.tokenize_json(json)
      update_settings_from_arg_tokens(settings, arg_tokens)
    end

    def update_settings_from_file(settings, file_path)
      arg_tokens = @arg_tokenizer.tokenize_file(file_path)
      update_settings_from_arg_tokens(settings, arg_tokens)
    end

    def update_settings_from_args(settings, args)
      arg_tokens = @arg_tokenizer.tokenize_args(args)
      update_settings_from_arg_tokens(settings, arg_tokens)
    end

    def find_settings_from_args(args)
      settings = FindSettings.new
      # default print_files to true since running as cli
      settings.print_files = true
      update_settings_from_args(settings, args)
      settings
    end

    def usage
      puts "#{get_usage_string}\n"
      abort
    end

    private

    def get_usage_string
      usage = ["Usage:\n", " rbfind [options] <path> [<path> ...]\n\n", "Options:\n"]
      opt_strings = []
      opt_descs = []
      longest = 0
      @options.sort! { |a, b| a.sort_arg <=> b.sort_arg }
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
        arg_type = ArgTokenType::UNKNOWN
        long_sym = long.to_sym
        if @bool_action_dict.key?(long_sym)
          arg_type = ArgTokenType::BOOL
        elsif @str_action_dict.key?(long_sym)
          arg_type = ArgTokenType::STR
        elsif @int_action_dict.key?(long_sym)
          arg_type = ArgTokenType::INT
        end
        @options.push(FindOption.new(short, long, desc, arg_type))
      end
    rescue StandardError => e
      raise FindError, "#{e} (file: #{find_options_json_path})"
    ensure
      f&.close
    end

    def update_settings_from_arg_tokens(settings, arg_tokens)
      arg_tokens.each do |arg_token|
        if arg_token.type == ArgTokenType::BOOL
          if arg_token.value == true || arg_token.value == false
            @bool_action_dict[arg_token.name].call(arg_token.value, settings)
            return if [:help, :version].include?(arg_token.name)
          else
            raise FindError, "Invalid value for option: #{arg_token.name}"
          end
        elsif arg_token.type == ArgTokenType::STR
          if arg_token.value.is_a?(String)
            @str_action_dict[arg_token.name].call(arg_token.value, settings)
          elsif arg_token.value.is_a?(Array)
            arg_token.value.each do |v|
              if v.is_a?(String)
                @str_action_dict[arg_token.name].call(v, settings)
              else
                raise FindError, "Invalid value for option: #{arg_token.name}"
              end
            end
          else
            raise FindError, "Invalid value for option: #{arg_token.name}"
          end
        elsif arg_token.type == ArgTokenType::INT
          if arg_token.value.is_a?(Numeric)
            @int_action_dict[arg_token.name].call(arg_token.value, settings)
          else
            raise FindError, "Invalid value for option: #{arg_token.name}"
          end
        else
          raise FindError, "Invalid option: #{arg_token.name}"
        end
      end
    end
  end
end
