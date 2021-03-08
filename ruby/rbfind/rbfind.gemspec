require_relative 'lib/rbfind/version'

Gem::Specification.new do |spec|
  spec.name        = 'rbfind'
  spec.version     = '0.1.0'
  spec.authors     = ['Cary Clark']
  spec.email       = 'clarkcb@gmail.com'

  spec.summary     = 'ruby version of xfind'
  spec.homepage    = 'http://github.com/clarkcb/xfind'
  spec.license     = 'MIT'
  spec.required_ruby_version = Gem::Requirement.new('>= 2.3.3')

  spec.files       = `git ls-files`.split($/)
  spec.test_files  = spec.files.grep(%r{^(test|spec|features)/})
  spec.bindir      = 'bin'
  spec.executables = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']
end
