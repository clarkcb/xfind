# frozen_string_literal: true

require 'find'
require 'pathname'
require 'set'
require_relative 'common'
require_relative 'filetypes'
require_relative 'fileutil'
require_relative 'finderror'
require_relative 'findfile'
require_relative 'findresult'
require_relative 'findresultformatter'

module RbFind

  # Finder - finds files to find and findes them according to settings
  class Finder
    attr_reader :settings
    attr_reader :results

    def initialize(settings)
      @settings = settings
      validate_settings
      @filetypes = FileTypes.new
      @results = []
    end

    def find_dir?(dirname)
      path_elems = dirname.split(File::SEPARATOR) - FileUtil.dot_dirs
      if @settings.excludehidden && path_elems.any? { |p| FileUtil.hidden?(p) }
        return false
      end
      if !@settings.in_dirpatterns.empty? &&
        !any_matches_any_pattern(path_elems, @settings.in_dirpatterns)
        return false
      end
      if !@settings.out_dirpatterns.empty? &&
        any_matches_any_pattern(path_elems, @settings.out_dirpatterns)
        return false
      end
      true
    end

    def find_file?(filename)
      ext = FileUtil.get_extension(filename)
      if !@settings.in_extensions.empty? &&
        !@settings.in_extensions.include?(ext)
        return false
      end
      if !@settings.out_extensions.empty? &&
        @settings.out_extensions.include?(ext)
        return false
      end
      if !@settings.in_filepatterns.empty? &&
        !matches_any_pattern(filename, @settings.in_filepatterns)
        return false
      end
      if !@settings.out_filepatterns.empty? &&
        matches_any_pattern(filename, @settings.out_filepatterns)
        return false
      end
      filetype = @filetypes.get_filetype(filename)
      if !@settings.in_filetypes.empty? &&
        !@settings.in_filetypes.include?(filetype)
        return false
      end
      if !@settings.out_filetypes.empty? &&
        @settings.out_filetypes.include?(filetype)
        return false
      end
      true
    end

    def archive_find_file?(filename)
      ext = FileUtil.get_extension(filename)
      if !@settings.in_archiveextensions.empty? &&
        !@settings.in_archiveextensions.include?(ext)
        return false
      end
      if !@settings.out_archiveextensions.empty? &&
        @settings.out_archiveextensions.include?(ext)
        return false
      end
      if !@settings.in_archivefilepatterns.empty? &&
        !matches_any_pattern(filename, @settings.in_archivefilepatterns)
        return false
      end
      if !@settings.out_archivefilepatterns.empty? &&
        matches_any_pattern(filename, @settings.out_archivefilepatterns)
        return false
      end
      true
    end

    def filter_file?(filename)
      if @settings.excludehidden && FileUtil.hidden?(filename)
        return false
      end
      if @filetypes.archive_file?(filename)
        return @settings.findarchives && archive_find_file?(filename)
      end
      !@settings.archivesonly && find_file?(filename)
    end

    def find
      # get the findfiles
      findfiles = get_find_files.sort_by(&:relativepath)
      if @settings.verbose
        finddirs = findfiles.map(&:path).uniq.sort
        log("\nDirectories to be found (#{finddirs.size}):")
        finddirs.each do |d|
          log(d)
        end
        log("\nFiles to be found (#{findfiles.size}):")
        findfiles.each do |sf|
          log(sf.to_s)
        end
        log("\n")
      end
      findfiles.each do |sf|
        find_file(sf)
      end
    end

    def find_multiline_string(str)
      lines_before = []
      lines_after = []
      results = []
      new_line_indices = get_new_line_indices(str)
      start_line_indices = [0] + new_line_indices.map { |n| n + 1 }
      end_line_indices = new_line_indices + [str.length - 1]
      @settings.findpatterns.each do |p|
        m = p.match(str)
        stop = false
        while m && !stop
          m_line_start_index = 0
          m_line_end_index = str.length - 1
          before_start_indices = start_line_indices.take_while { |i| i <= m.begin(0) }
          before_line_count = 0
          if before_start_indices
            m_line_start_index = before_start_indices.last
            before_line_count = before_start_indices.size - 1
          end
          m_line_end_index = end_line_indices[start_line_indices.index(m_line_start_index)]
          after_start_indices = start_line_indices.drop_while { |i| i <= m.begin(0) }
          line = str[m_line_start_index..m_line_end_index]
          if @settings.linesbefore && before_line_count
            before_start_indices = before_start_indices[0, before_start_indices.size - 1].last(@settings.linesbefore)
            lines_before = get_lines_before(str, before_start_indices,
                                            start_line_indices, end_line_indices)
          end
          if @settings.linesafter && after_start_indices
            after_start_indices = after_start_indices[0, @settings.linesafter]
            lines_after = get_lines_after(str, after_start_indices,
                                          start_line_indices, end_line_indices)
          end
          match_start_index = m.begin(0) - m_line_start_index
          match_end_index = m.end(0) - m_line_start_index
          if (lines_before.length == 0 || lines_before_match(lines_before)) &&
            (lines_after.length == 0 || lines_after_match(lines_after))
            results.push(FindResult.new(
              p,
              nil,
              before_line_count + 1,
              match_start_index + 1,
              match_end_index + 1,
              line,
              lines_before,
              lines_after
            ))
            stop = true if @settings.firstmatch
          end
          m = p.match(str, m_line_start_index + match_end_index)
        end
      end
      results
    end

    def find_line_iterator(lines)
      linenum = 0
      lines_before = []
      lines_after = []
      pattern_matches = {}
      results = []
      loop do
        line =
          if lines_after.empty?
            begin
              lines.next
            rescue StopIteration
              return results
            end
          else
            lines_after.shift
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
        @settings.findpatterns.each do |p|
          find_line = true
          if @settings.firstmatch && pattern_matches.include?(p)
            find_line = false
          end
          pos = 0
          while find_line && pos < line.length
            begin
              m = p.match(line, pos)
              if m
                if (!lines_before.empty? && !lines_before_match(lines_before)) ||
                  (!lines_after.empty? && !lines_after_match(lines_after))
                  find_line = false
                else
                  results.push(FindResult.new(
                    p,
                    nil,
                    linenum,
                    m.begin(0) + 1,
                    m.end(0) + 1,
                    line,
                    [].concat(lines_before),
                    [].concat(lines_after)
                  ))
                  pos = m.end(0) + 1
                  pattern_matches[p] = 1
                end
              else
                find_line = false
              end
            rescue StandardError => e
              raise FindError, e
            end
          end
        end
        if @settings.linesbefore
          lines_before.shift if lines_before.size == @settings.linesbefore
          lines_before.push(line) if lines_before.size < @settings.linesbefore
        end
      end
    end

    def print_results
      formatter = FindResultFormatter.new(@settings)
      RbFind::log("Find results (#{@results.size}):")
      @results.each do |r|
        # print_result(r)
        RbFind::log(formatter.format(r))
      end
    end

    def print_result(find_result)
      s = ''
      s += "#{find_result.pattern}: " if @settings.findpatterns.size > 1
      s += find_result.to_s
      RbFind::log(s)
    end

    def get_matching_dirs
      @results.map { |r| r.file.path }.uniq.sort
    end

    def get_matching_files
      @results.map { |r| r.file.to_s }.uniq.sort
    end

    def get_matching_lines
      lines = @results.map { |r| r.line.strip }.sort { |l1, l2| l1.upcase <=> l2.upcase }
      lines.uniq! if @settings.uniquelines
      lines
    end

    private

    def validate_settings
      raise FindError, 'Startpath not defined' unless @settings.startpath
      raise FindError, 'Startpath not found' unless Pathname.new(@settings.startpath).exist?
      raise FindError, 'Startpath not readable' unless File.readable?(@settings.startpath)
      raise FindError, 'No find patterns defined' if @settings.findpatterns.empty?
      raise FindError, 'Invalid linesbefore' if @settings.linesbefore < 0
      raise FindError, 'Invalid linesafter' if @settings.linesafter < 0
      raise FindError, 'Invalid maxlinelength' if @settings.maxlinelength < 0
      begin
        _enc = Encoding.find(@settings.textfileencoding)
      rescue StandardError
        raise FindError, "Invalid encoding: #{@settings.textfileencoding}"
      end
    end

    def matches_any_pattern(str, pattern_set)
      pattern_set.any? { |p| p.match(str) }
    end

    def any_matches_any_pattern(str_list, pattern_set)
      str_list.each do |s|
        return true if matches_any_pattern(s, pattern_set)
      end
      false
    end

    def file_to_findfile(f)
      d = File.dirname(f) || '.'
      filename = File.basename(f)
      filetype = @filetypes.get_filetype(filename)
      FindFile.new(d, filename, filetype)
    end

    def get_find_files
      findfiles = []
      if FileTest.directory?(@settings.startpath)
        if @settings.recursive
          Find.find(@settings.startpath) do |f|
            if FileTest.directory?(f)
              Find.prune unless find_dir?(f)
            elsif filter_file?(f)
              findfile = file_to_findfile(f)
              findfiles.push(findfile)
            end
          end
        else
          Find.find(@settings.startpath) do |f|
            if FileTest.directory?(f)
              Find.prune
            elsif filter_file?(f)
              findfile = file_to_findfile(f)
              findfiles.push(findfile)
            end
          end
        end
      elsif FileTest.file?(@settings.startpath)
        findfile = file_to_findfile(@settings.startpath)
        findfiles.push(findfile)
      end
      findfiles
    end

    def find_file(sf)
      unless @filetypes.findable_file?(sf.filename)
        if @settings.verbose || @settings.debug
          log("Skipping unfindable file: #{sf}")
          return 0
        end
      end
      case sf.filetype
      when FileType::CODE, FileType::TEXT, FileType::XML
        find_text_file(sf)
      when FileType::BINARY
        find_binary_file(sf)
      else
        log("Finding currently unsupported for FileType #{sf.filetype}")
      end
    end

    def find_binary_file(sf)
      f = File.open(sf.relativepath, 'rb')
      contents = f.read
      results = find_binary_string(contents)
      results.each do |r|
        r.file = sf
        add_find_result(r)
      end
    rescue StandardError => e
      raise FindError, e
    ensure
      f&.close
    end

    def find_binary_string(binstr)
      results = []
      @settings.findpatterns.each do |p|
        pos = 0
        m = p.match(binstr, pos)
        stop = false
        while m && !stop
          results.push(FindResult.new(
            p,
            nil,
            0,
            m.begin(0) + 1,
            m.end(0) + 1,
            nil
          ))
          pos = m.end(0) + 1
          m = p.match(binstr, pos)
          stop = true if @settings.firstmatch
        end
      end
      results
    end

    def find_text_file(sf)
      log("Finding text file #{sf}") if @settings.debug
      if @settings.multilineoption-REMOVE
        find_text_file_contents(sf)
      else
        find_text_file_lines(sf)
      end
    end

    def get_line_count(s)
      s.scan(/(\r\n|\n)/m).size
    end

    def find_text_file_contents(sf)
      f = File.open(sf.relativepath, mode: "r:#{@settings.textfileencoding}")
      contents = f.read
      results = find_multiline_string(contents)
      results.each do |r|
        r.file = sf
        add_find_result(r)
      end
    rescue StandardError => e
      raise FindError, "#{e} (file: #{sf})"
    ensure
      f&.close
    end

    def get_new_line_indices(s)
      indices = []
      i = 0
      s.chars.each do |c|
        indices.push(i) if c == "\n"
        i += 1
      end
      indices
    end

    def get_lines_at_indices(s, at_indices, start_line_indices, end_line_indices)
      lines = []
      at_indices.each do |i|
        line = s[i..end_line_indices[start_line_indices.index(i)]]
        lines.push(line)
      end
      lines
    end

    def get_lines_before(s, before_start_indices, start_line_indices, end_line_indices)
      get_lines_at_indices(s, before_start_indices, start_line_indices, end_line_indices)
    end

    def get_lines_after(s, after_start_indices, start_line_indices, end_line_indices)
      get_lines_at_indices(s, after_start_indices, start_line_indices, end_line_indices)
    end

    def lines_match(lines, in_patterns, out_patterns)
      ((in_patterns.empty? || any_matches_any_pattern(lines, in_patterns)) &&
        (out_patterns.empty? || !any_matches_any_pattern(lines, out_patterns)))
    end

    def lines_before_match(lines_before)
      lines_match(lines_before, @settings.in_linesbeforepatterns,
                  @settings.out_linesbeforepatterns)
    end

    def do_lines_after_or_until
      !@settings.linesaftertopatterns.empty? ||
        !@settings.linesafteruntilpatterns.empty?
    end

    def do_lines_after
      @settings.linesafter || do_lines_after_or_until
    end

    def lines_after_match(lines_after)
      return true if do_lines_after_or_until
      lines_match(lines_after, @settings.in_linesafterpatterns,
                  @settings.out_linesafterpatterns)
    end

    def find_text_file_lines(sf)
      f = File.open(sf.relativepath, mode: "r:#{@settings.textfileencoding}")
      line_iterator = f.each_line
      results = find_line_iterator(line_iterator)
      results.each do |r|
        r.file = sf
        add_find_result(r)
      end
    rescue StandardError => e
      raise FindError, "#{e} (file: #{sf})"
    ensure
      f&.close
    end

    def add_find_result(find_result)
      @results.push(find_result)
    end
  end
end
