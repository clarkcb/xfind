# frozen_string_literal: true

################################################################################
#
# common.rb
#
# Common methods and settings
#
################################################################################

module RbFind
  module_function
  def log(message)
    STDOUT.puts(message)
  end

  def log_err(message)
    STDERR.puts("ERROR: #{message}")
  end
end
