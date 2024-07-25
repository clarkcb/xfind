# frozen_string_literal: true

module RbFind
  XFINDPATH = ENV['XFINDPATH'].nil? ? File.join(ENV['HOME'], 'src', 'xfind') : ENV['XFINDPATH']
  SHAREDPATH = File.join(XFINDPATH, 'shared')
  XFINDDB = File.join(SHAREDPATH, 'xfind.db')
end
