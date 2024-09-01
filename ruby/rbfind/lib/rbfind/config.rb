# frozen_string_literal: true

module RbFind
  XFIND_PATH = ENV['XFIND_PATH'].nil? ? File.join(ENV['HOME'], 'src', 'xfind') : ENV['XFIND_PATH']
  SHARED_PATH = File.join(XFIND_PATH, 'shared')
  XFIND_DB = File.join(SHARED_PATH, 'xfind.db')
end
