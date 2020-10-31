# frozen_string_literal: true
require_relative 'color.rb'

# SearchResult - encapsulates a search result
class SearchResult
  attr_accessor :pattern
  attr_accessor :file
  attr_accessor :linenum
  attr_accessor :match_start_index
  attr_accessor :match_end_index
  attr_accessor :line
  attr_accessor :lines_before
  attr_accessor :lines_after

  SEPARATOR_LEN = 80

  def initialize(pattern, file, linenum=0, match_start_index=0, match_end_index=0, line='', lines_before=[],
                 lines_after=[])
    @pattern = pattern
    @file = file
    @linenum = linenum
    @match_start_index = match_start_index
    @match_end_index = match_end_index
    @line = line
    @lines_before = lines_before
    @lines_after = lines_after
  end
end
