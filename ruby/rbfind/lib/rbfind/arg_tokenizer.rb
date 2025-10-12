# frozen_string_literal: true

module RbFind

  module ArgTokenType
    UNKNOWN = 0
    BOOL    = 1
    STR     = 2
    INT     = 3
  end

  class ArgToken
    attr_reader :name
    attr_reader :type
    attr_reader :value

    def initialize(name, type, value)
      @name = name
      @type = type
      @value = value
    end
  end

  class ArgTokenizer

    def initialize(options)
      @bool_dict = {}
      @str_dict = {}
      @int_dict = {}
      options.each do |o|
        long_sym = o.long_arg.to_sym
        if o.arg_type == ArgTokenType::BOOL
          @bool_dict[long_sym] = long_sym
          @bool_dict[o.short_arg.to_sym] = long_sym if o.short_arg
        elsif o.arg_type == ArgTokenType::STR
          @str_dict[long_sym] = long_sym
          @str_dict[o.short_arg.to_sym] = long_sym if o.short_arg
        elsif o.arg_type == ArgTokenType::INT
          @int_dict[long_sym] = long_sym
          @int_dict[o.short_arg.to_sym] = long_sym if o.short_arg
        end
      end
    end

    def tokenize_args(args)
      arg_tokens = []
      until args.empty?
        arg = args.shift
        if arg.start_with?('-')
          arg_names = []
          if arg.start_with?('--')
            if arg.length > 2
              arg_name = arg.slice(2..arg.length)
              if arg_name.include?('=')
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
              c_sym = c.to_sym
              if @bool_dict.key?(c_sym)
                arg_names << @bool_dict[c_sym]
              elsif @str_dict.key?(c_sym)
                arg_names << @str_dict[c_sym]
              elsif @int_dict.key?(c_sym)
                arg_names << @int_dict[c_sym]
              else
                raise FindError, "Invalid option: #{c}"
              end
            end
          else
            raise FindError, "Invalid option: #{arg}"
          end

          arg_names.each do |arg_name|
            if @bool_dict.key?(arg_name)
              arg_tokens << ArgToken.new(arg_name, ArgTokenType::BOOL, true)
            elsif @str_dict.key?(arg_name) || @int_dict.key?(arg_name)
              raise FindError, "Missing value for option #{arg_name}" if args.empty?
              arg_val = args.shift
              if @str_dict.key?(arg_name)
                arg_tokens << ArgToken.new(arg_name, ArgTokenType::STR, arg_val)
              else
                arg_tokens << ArgToken.new(arg_name, ArgTokenType::INT, arg_val.to_i)
              end
            else
              raise FindError, "Invalid option: #{arg_name}"
            end
          end
        else
          arg_tokens << ArgToken.new(:path, ArgTokenType::STR, arg)
        end
      end
      arg_tokens
    end

    def tokenize_arg_hash(arg_hash)
      arg_tokens = []
      # keys are sorted so that output is consistent across all versions
      arg_names = arg_hash.keys.sort
      arg_names.each do |arg_name|
        arg_value = arg_hash[arg_name]
        if @bool_dict.key?(arg_name)
          if arg_value == true || arg_value == false
            arg_tokens << ArgToken.new(arg_name, ArgTokenType::BOOL, arg_value)
            return arg_tokens if [:help, :version].include?(arg_name)
          else
            raise FindError, "Invalid value for option: #{arg_name}"
          end
        elsif @str_dict.key?(arg_name)
          if arg_value.is_a?(String)
            arg_tokens << ArgToken.new(arg_name, ArgTokenType::STR, arg_value)
          elsif arg_value.is_a?(Array)
            arg_value.each do |v|
              if v.is_a?(String)
                arg_tokens << ArgToken.new(arg_name, ArgTokenType::STR, v)
              else
                raise FindError, "Invalid value for option: #{arg_name}"
              end
            end
          else
            raise FindError, "Invalid value for option: #{arg_name}"
          end
        elsif @int_dict.key?(arg_name)
          if arg_value.is_a?(Numeric)
            arg_tokens << ArgToken.new(arg_name, ArgTokenType::INT, arg_value)
          elsif arg_value.is_a?(String)
            arg_tokens << ArgToken.new(arg_name, ArgTokenType::INT, arg_value.to_i)
          else
            raise FindError, "Invalid value for option: #{arg_name}"
          end
        else
          raise FindError, "Invalid option: #{arg_name}"
        end
      end
      arg_tokens
    end

    def tokenize_json(json)
      json_hash = JSON.parse(json).transform_keys(&:to_sym)
      tokenize_arg_hash(json_hash)
    end

    def tokenize_file(file_path)
      expanded_path = Pathname.new(file_path).expand_path
      unless expanded_path.exist?
        raise FindError, "Settings file not found: #{file_path}"
      end
      unless expanded_path.extname == '.json'
        raise FindError, "Invalid settings file (must be JSON): #{file_path}"
      end
      f = File.open(expanded_path.to_s, mode: 'r')
      json = f.read
      tokenize_json(json)
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

  end

end
