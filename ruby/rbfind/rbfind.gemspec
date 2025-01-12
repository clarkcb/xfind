# frozen_string_literal: true

require_relative 'lib/rbfind/version'

Gem::Specification.new do |spec|
  spec.name        = 'rbfind'
  spec.version     = '0.1.0'
  spec.authors     = ['Cary Clark']
  spec.email       = ['clarkcb@gmail.com']

  spec.summary     = 'ruby version of xfind'
  spec.homepage    = 'https://github.com/clarkcb/xfind'
  spec.license     = 'MIT'
  spec.required_ruby_version = ">= 3.3.6"

  spec.metadata['allowed_push_host'] = "TODO: Set to your gem server 'https://example.com'"

  spec.metadata['homepage_uri']    = spec.homepage
  spec.metadata['source_code_uri'] = spec.homepage
  # spec.metadata["changelog_uri"]   = "TODO: Put your gem's CHANGELOG.md URL here."

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = Dir.chdir(__dir__) do
    gitfiles = `git ls-files -z`.split("\x0").reject do |f|
      (File.expand_path(f) == __FILE__) ||
        f.start_with?(*%w[test/ spec/ features/ .git .github appveyor Gemfile])
    end
    datafiles = `find ./data -type f -name "*.json" -print0`.split("\x0")
    gitfiles.concat(datafiles)
  end
  spec.test_files = Dir.chdir(__dir__) do
    `git ls-files -z`.split("\x0").select { |f| f.start_with?('test/') } 
  end
  spec.bindir = 'bin'
  # spec.executables = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  # spec.executables << 'rbfind'
  spec.executables = %w[rbfind.sh rbfind.ps1]
  spec.require_paths = ['lib']

  # Uncomment to register a new dependency of your gem
  # spec.add_dependency "example-gem", "~> 1.0"

  # For more information and examples about making a new gem, check out our
  # guide at: https://bundler.io/guides/creating_gem.html
end
