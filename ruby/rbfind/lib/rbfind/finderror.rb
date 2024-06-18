# frozen_string_literal: true

module RbFind

  # FindError - custom exception class
  class FindError < StandardError
    def initialize(msg = 'FindError occurred')
      super(msg)
    end
  end
end
