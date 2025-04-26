# frozen_string_literal: true

module RbFind

  INVALID_RANGE_MINDEPTH_MAXDEPTH = 'Invalid range for mindepth and maxdepth'
  INVALID_RANGE_MINLASTMOD_MAXLASTMOD = 'Invalid range for minlastmod and maxlastmod'
  INVALID_RANGE_MINSIZE_MAXSIZE = 'Invalid range for minsize and maxsize'
  STARTPATH_NOT_DEFINED = 'Startpath not defined'
  STARTPATH_NOT_FOUND = 'Startpath not found'
  STARTPATH_NOT_MATCH_SETTINGS = 'Startpath does not match find settings'
  STARTPATH_NOT_PATHNAME = 'Startpath not a Pathname instance'
  STARTPATH_NOT_READABLE = 'Startpath not readable'

  # FindError - custom exception class
  class FindError < StandardError
    def initialize(msg = 'FindError occurred')
      super(msg)
    end
  end
end
