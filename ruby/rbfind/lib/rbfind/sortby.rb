module RbFind

  module SortBy
    FILEPATH = 0
    FILENAME = 1
    FILESIZE = 2
    FILETYPE = 3
    LASTMOD  = 4

    NAMES = Array[:filepath, :filename, :filesize, :filetype, :lastmod].freeze

    module_function
    def from_name(name)
      from_sym(name.downcase.to_sym)
    end

    def from_sym(sym)
      idx = NAMES.index(sym)
      if idx.nil?
        if sym.equal?(:name)
          FILENAME
        elsif sym.equal?(:size)
          FILESIZE
        elsif sym.equal?(:type)
          FILETYPE
        else
          FILEPATH
        end
      else
        idx
      end
    end

    def to_name(sort_by)
      sort_by < NAMES.size ? NAMES[sort_by].to_s : NAMES[0].to_s
    end
  end
end
