# frozen_string_literal: true

module RbFind

  # FileUtil - file utility functions
  module FileUtil
    module_function

    def dot_dirs
      %w[. .. ./ ../]
    end

    def dot_dir?(file_path)
      dot_dirs.include?(file_path.to_s)
    end

    def get_extension(file_path)
      unless file_path.instance_of?(Pathname)
        file_path = Pathname.new(file_path)
      end
      ext = file_path.extname
      if !ext.empty? && ext.start_with?('.')
        ext = ext[1..-1]
      end
      if ext != 'Z'
        ext = ext.downcase
      end
      ext
    end

    def hidden?(file_path)
      file_path.each_filename do |f|
        if f.length > 1 && f[0] == '.' && !dot_dirs.include?(f)
          return true
        end
      end
      false
    end

    def path_elems(file_path)
      elems = []
      file_path.each_filename do |f|
        elems.push(f)
      end
      elems
    end

    def elem_count(file_path)
      elem_count = 0
      file_path.each_filename do |_|
        elem_count += 1
      end
      elem_count
    end
  end
end
