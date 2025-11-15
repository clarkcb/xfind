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

  def log_err(message, colorize = true)
    err = "ERROR: #{message}"
    if colorize
      err = err.bold_red
    end
    STDERR.puts(err)
  end
end
