################################################################################
#
# searcher.rb
#
# class Searcher: executes a file search
#
################################################################################

require 'find'
require 'pathname'
require 'set'
require 'common.rb'
require 'filetypes.rb'
require 'fileutil.rb'
require 'searchresult.rb'

class Searcher
  attr_accessor :results
  attr_accessor :settings

  def initialize(settings)
    @settings = settings
    validate_settings
    @filetypes = FileTypes.new
    @results = []
    @timers = {}
    @totalElapsed = 0
  end

  def validate_settings
    raise 'Startpath not defined' unless @settings.startpath
    raise 'Startpath not found' unless Pathname.new(@settings.startpath).exist?
    raise 'No search patterns defined' unless @settings.searchpatterns.count > 0
  end

  def matches_any_pattern(s, pattern_set)
    pattern_set.any? {|p| p.match(s)}
  end

  def any_matches_any_pattern(slist, pattern_set)
    slist.each do |s|
      if matches_any_pattern(s, pattern_set)
        return true
      end
    end
    return false
  end

  def is_search_dir(d)
    path_elems = d.split(File::SEPARATOR) - ['.', '..']
    if @settings.excludehidden && path_elems.any? {|p| FileUtil::is_hidden?(p)}
      return false
    end
    if @settings.in_dirpatterns.count > 0 &&
      ! any_matches_any_pattern(path_elems, @settings.in_dirpatterns)
      return false
    end
    if @settings.out_dirpatterns.count > 0 &&
      any_matches_any_pattern(path_elems, @settings.out_dirpatterns)
      return false
    end
    true
  end

  def is_search_file(f)
    if FileUtil::is_hidden?(f) && @settings.excludehidden
      return false
    end
    ext = FileUtil::get_extension(f)
    if @settings.in_extensions.count > 0 &&
      ! @settings.in_extensions.include?(ext)
      return false
    end
    if @settings.out_extensions.count > 0 &&
      @settings.out_extensions.include?(ext)
      return false
    end
    filename = Pathname.new(f).basename.to_s
    if @settings.in_filepatterns.count > 0 &&
      ! matches_any_pattern(filename, @settings.in_filepatterns)
      return false
    end
    if @settings.out_filepatterns.count > 0 &&
      matches_any_pattern(filename, @settings.out_filepatterns)
      return false
    end
    true
  end

  def is_archive_search_file(f)
    if FileUtil::is_hidden?(f) && @settings.excludehidden
      return false
    end
    ext = FileUtil::get_extension(f)
    if @settings.in_archiveextensions.count > 0 &&
      ! @settings.in_archiveextensions.include?(ext)
      return false
    end
    if @settings.out_archiveextensions.count > 0 &&
      @settings.out_archiveextensions.include?(ext)
      return false
    end
    filename = Pathname.new(f).basename.to_s
    if @settings.in_archivefilepatterns.count > 0 &&
      ! matches_any_pattern(filename, @settings.in_archivefilepatterns)
      return false
    end
    if @settings.out_archivefilepatterns.count > 0 &&
      matches_any_pattern(filename, @settings.out_archivefilepatterns)
      return false
    end
    true
  end

  def get_search_dirs
    searchdirs = []
    if FileTest.directory?(@settings.startpath)
      if @settings.recursive
        Find.find(@settings.startpath) do |f|
          if FileTest.directory?(f)
            searchdirs.push(f) if is_search_dir(f)
          end
        end
      else
        searchdirs.push(@settings.startpath) if is_search_dir(@settings.startpath)
      end
    elsif FileTest.file?(@settings.startpath)
      d = File.dirname(@settings.startpath)
      if ! d
        d = '.'
      end
      searchdirs.push(d) if is_search_dir(d)
    end
    searchdirs
  end

  def filter_file(f)
    if FileUtil::is_hidden?(f) && @settings.excludehidden
      return false
    end
    if @filetypes.is_archive_file(f)
      return @settings.searcharchives && is_archive_search_file(f)
    end
    return !@settings.archivesonly && is_search_file(f)
  end

  def get_search_files(searchdirs)
    searchfiles = []
    if FileTest.directory?(@settings.startpath)
      searchdirs.each do |d|
        all_dirs = Dir.entries(d)
        all_dirs.each do |f|
          unless FileTest.directory?(f)
            if filter_file(f)
              searchfiles.push(Pathname.new(d).join(f).to_s)
            end
          end
        end
      end
    elsif FileTest.file?(@settings.startpath)
      searchfiles.push(@settings.startpath) if is_search_file(@settings.startpath)
    end
    searchfiles
  end

  def add_timer(name, action)
    @timers[name+':'+action] = Time.new
  end

  def start_timer(name)
    add_timer(name, 'start')
  end

  def stop_timer(name)
    add_timer(name, 'stop')
    add_elapsed(name)
  end

  def get_elapsed(name)
    start = @timers[name+':start']
    stop = @timers[name+':stop']
    stop - start
  end

  def add_elapsed(name)
    @totalElapsed += get_elapsed(name)
  end

  def print_elapsed(name)
    elapsed = get_elapsed(name) * 1000
    log("Elapsed time for #{name}: #{elapsed} ms")
  end

  def print_total_elapsed
    log("Total elapsed time: #{@totalElapsed * 1000} ms")
  end

  def search
    # get the searchdirs
    if @settings.dotiming
      start_timer('get_search_dirs')
    end
    searchdirs = get_search_dirs
    if @settings.dotiming
      stop_timer('get_search_dirs')
      if @settings.printresults
        print_elapsed('get_search_dirs')
      end
    end
    if @settings.verbose
      log("\nDirectories to be searched (#{searchdirs.count}):")
      searchdirs.each do |d|
        log("#{d}")
      end
    end
    # get the searchfiles
    if @settings.dotiming
      start_timer('get_search_files')
    end
    searchfiles = get_search_files(searchdirs)
    if @settings.dotiming
      stop_timer('get_search_files')
      if @settings.printresults
        print_elapsed('get_search_files')
      end
    end
    if @settings.verbose
      log("\nFiles to be searched (#{searchfiles.count}):")
      searchfiles.each do |f|
        log("#{f}")
      end
      log("\n")
    end
    if @settings.dotiming
      start_timer('search_files')
    end
    searchfiles.each do |f|
      search_file(f)
    end
    if @settings.dotiming
      stop_timer('search_files')
      if @settings.printresults
        print_elapsed('search_files')
        print_total_elapsed
      end
    end
  end

  def search_file(f)
    unless @filetypes.is_searchable_file(f)
      if @settings.verbose || @settings.debug
        log("Skipping unsearchable file: #{f}")
        return 0
      end
    end
    filetype = @filetypes.get_filetype(f)
    if filetype == FileType::Text
      search_text_file(f)
    elsif filetype == FileType::Binary
      search_binary_file(f)
    end
  end

  def search_binary_file(f)
    contents = File.open(f, "rb").read
    @settings.searchpatterns.each do |p|
      if p.match(contents)
        add_search_result(SearchResult.new(p, f, 0, nil))
      end
    end
  end

  def search_text_file(f, enc = nil)
    if @settings.debug
      log("Searching text file #{f}")
    end
    if @settings.multilinesearch
      search_text_file_contents(f, enc)
    else
      search_text_file_lines(f, enc)
    end
  end

  def get_line_count(s)
    s.scan(/(\r\n|\n)/m).size
  end

  def search_text_file_contents(f, enc='ISO8859-1')
    begin
      # using ISO8859-1 instead of UTF-8 because a UTF-8 file won't break
      # on ISO8859-1 but a non-UTF-8 file will break on UTF-8
      enc='ISO8859-1'
      contents = File.open(f, mode: "r:#{enc}").read
      results = search_multiline_string(contents)
      results.each do |r|
        r.filename = f
        add_search_result(r)
      end
    rescue ArgumentError => e
      log("\nArgumentError in search_text_file_contents: #{e.message}\n\n")
    end
  end

  def get_new_line_indices(s)
    indices = []
    i = 0
    s.chars.each do |c|
      if c == "\n"
        indices.push(i)
      end
      i += 1
    end
    indices
  end

  def get_lines_at_indices(s, at_indices, start_line_indices, end_line_indices)
    if ! at_indices
      []
    end
    lines = []
    at_indices.each {|i|
      line = s[i..end_line_indices[start_line_indices.index(i)]]
      lines.push(line)
    }
    lines
  end

  def get_lines_before(s, before_start_indices, start_line_indices, end_line_indices)
    get_lines_at_indices(s, before_start_indices, start_line_indices, end_line_indices)
  end

  def get_lines_after(s, after_start_indices, start_line_indices, end_line_indices)
    get_lines_at_indices(s, after_start_indices, start_line_indices, end_line_indices)
  end

  def search_multiline_string(s)
    lines_before = []
    lines_after = []
    results = []
    new_line_indices = get_new_line_indices(s)
    start_line_indices = [0] + new_line_indices.map { |n| n+1 }
    end_line_indices = new_line_indices + [s.length - 1]
    @settings.searchpatterns.each do |p|
      m = p.match(s)
      stop = false
      while m && !stop
        m_line_start_index = 0
        m_line_end_index = s.length - 1
        before_start_indices = start_line_indices.take_while { |i| i <= m.begin(0) }
        before_line_count = 0
        if before_start_indices
          m_line_start_index = before_start_indices.last
          before_line_count = before_start_indices.count - 1
        end
        m_line_end_index = end_line_indices[start_line_indices.index(m_line_start_index)]
        after_start_indices = start_line_indices.drop_while { |i| i <= m.begin(0) }
        line = s[m_line_start_index..m_line_end_index]
        if @settings.linesbefore && before_line_count
          before_start_indices = before_start_indices[0, before_start_indices.size-1].last(@settings.linesbefore)
          lines_before = get_lines_before(s, before_start_indices,
            start_line_indices, end_line_indices)
        end
        if @settings.linesafter && after_start_indices
          after_start_indices = after_start_indices[0, @settings.linesafter]
          lines_after = get_lines_after(s, after_start_indices,
            start_line_indices, end_line_indices)
        end
        match_start_index = m.begin(0) - m_line_start_index
        match_end_index = m.end(0) - m_line_start_index
        if (lines_before.length == 0 || lines_before_match(lines_before)) &&
          (lines_after.length == 0 || lines_after_match(lines_after))
          results.push(SearchResult.new(
            p,
            '',
            before_line_count+1,
            match_start_index + 1,
            match_end_index + 1,
            line,
            lines_before,
            lines_after))
          if @settings.firstmatch
            stop = true
          end
        end
        m = p.match(s, m_line_start_index+match_end_index)
      end
    end
    results
  end

  def lines_match(lines, in_patterns, out_patterns)
    ((in_patterns.length == 0 || any_matches_any_pattern(lines, in_patterns)) &&
    (out_patterns.length == 0 || ! any_matches_any_pattern(lines, out_patterns)))
  end

  def lines_before_match(lines_before)
    lines_match(lines_before, @settings.in_linesbeforepatterns,
      @settings.out_linesbeforepatterns)
  end

  def do_lines_after_or_until
    @settings.linesaftertopatterns.length > 0 ||
    @settings.linesafteruntilpatterns.length > 0
  end

  def do_lines_after
    @settings.linesafter || do_lines_after_or_until
  end

  def lines_after_match(lines_after)
    if do_lines_after_or_until
      return true
    end
    lines_match(lines_after, @settings.in_linesafterpatterns,
      @settings.out_linesafterpatterns)
  end

  def search_text_file_lines(f, enc='ISO8859-1')
    begin
      # using ISO8859-1 instead of UTF-8 because a UTF-8 file won't break
      # on ISO8859-1 but a non-UTF-8 file will break on UTF-8
      enc = "ISO8859-1"
      fo = File.open(f, mode: "r:#{enc}")
      line_iterator = fo.each_line
      results = search_line_iterator(line_iterator)
      fo.close
      results.each do |r|
        r.filename = f
        add_search_result(r)
      end
    rescue ArgumentError => e
      log("\nArgumentError in search_text_file_lines: #{e.message}\n\n")
    end
  end

  def search_line_iterator(lines)
    linenum = 0
    lines_before = []
    lines_after = []
    pattern_matches = {}
    results = []
    while true
      line = ""
      if lines_after.length > 0
        line = lines_after.shift
      else
        begin
          line = lines.next
        rescue StopIteration
          return results
        end
      end
      linenum += 1
      if @settings.linesafter
        while lines_after.length < @settings.linesafter
          begin
            lines_after.push(lines.next)
          rescue StopIteration
            break
          end
        end
      end
      @settings.searchpatterns.each do |p|
        search_line = true
        if @settings.firstmatch && pattern_matches.include?(p)
          search_line = false
        end
        pos = 0
        while search_line && pos < line.length
          begin
            m = p.match(line, pos)
            if m
              if (lines_before.length > 0 && ! lines_before_match(lines_before)) ||
                 (lines_after.length > 0 && ! lines_after_match(lines_after))
                search_line = false
              else
                results.push(SearchResult.new(
                  p,
                  '',
                  linenum,
                  m.begin(0) + 1,
                  m.end(0) + 1,
                  line,
                  [].concat(lines_before),
                  [].concat(lines_after)))
                pos = m.end(0) + 1
                pattern_matches[p] = 1
              end
            else
              search_line = false
            end
          rescue ArgumentError => e
            log("\nArgumentError in search_line_iterator: #{e.message}\n\n")
            search_line = false
          end
        end
      end
      if @settings.linesbefore
        if lines_before.length == @settings.linesbefore
          lines_before.shift
        end
        if lines_before.length < @settings.linesbefore
          lines_before.push(line)
        end
      end
    end
  end

  def add_search_result(search_result)
    @results.push(search_result)
  end

  def print_results()
    log("Search results (#{@results.count}):")
    @results.each do |r|
      print_result(r)
    end
  end

  def print_result(search_result)
    s = ""
    if @settings.searchpatterns.count > 1
      s += "#{search_result.pattern}: "
    end
    s += search_result.to_s
    log(s)
  end

  def get_matching_dirs(patterns = [])
    if patterns.empty?
      patterns = @settings.searchpatterns
    end
    pattern_set = Set.new patterns
    dirs = Set.new
    @results.each do |r|
      if pattern_set.include? r.pattern
        dirs.add(File.dirname(r.filename))
      end
    end
    dirs = dirs.to_a
    dirs.sort
  end

  def get_matching_files(patterns = [])
    if patterns.empty?
      patterns = @settings.searchpatterns
    end
    pattern_set = Set.new patterns
    files = Set.new
    @results.each do |r|
      if pattern_set.include? r.pattern
        files.add(r.filename)
      end
    end
    files = files.to_a
    files.sort
  end

  def get_matching_lines(patterns = [])
    if patterns.empty?
      patterns = @settings.searchpatterns
    end
    pattern_set = Set.new patterns
    lines = []
    @results.each do |r|
      if pattern_set.include? r.pattern
        lines.push(r.line.strip)
      end
    end
    if @settings.uniquelines
      lines.uniq!
    end
    lines.sort {|s1, s2| s1.upcase <=> s2.upcase}
  end
end
