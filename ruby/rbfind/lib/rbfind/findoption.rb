# frozen_string_literal: true

module RbFind

  # FindOption - encapsulates a CLI find option
  class FindOption
    attr_reader :shortarg
    attr_reader :longarg
    attr_reader :desc
    attr_reader :func

    def initialize(shortarg, longarg, desc, func)
      @shortarg = shortarg
      @longarg = longarg
      @desc = desc
      @func = func
    end

    def sortarg
      if !@shortarg.nil? && !@shortarg.empty?
        @shortarg.downcase + 'a' + @longarg.downcase
      else
        @longarg.downcase
      end
    end
  end
end
