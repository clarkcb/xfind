# frozen_string_literal: true

module RbFind

  # FindOption - encapsulates a CLI find option
  class FindOption
    attr_reader :short_arg
    attr_reader :long_arg
    attr_reader :desc
    attr_reader :func

    def initialize(short_arg, long_arg, desc, func)
      @short_arg = short_arg
      @long_arg = long_arg
      @desc = desc
      @func = func
    end

    def sortarg
      if !@short_arg.nil? && !@short_arg.empty?
        @short_arg.downcase + 'a' + @long_arg.downcase
      else
        @long_arg.downcase
      end
    end
  end
end
