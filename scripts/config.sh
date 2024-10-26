################################################################################
#
# config.sh
#
# The common configuration for bash scripts
#
################################################################################

########################################
# Configuration
########################################

# XFIND_PATH defaults to $HOME/src/xfind if not defined as env var
if [ -z "$XFIND_PATH" ]; then
    if [ -n "$XFINDPATH" ]; then
        XFIND_PATH=$XFINDPATH
    else
        XFIND_PATH=$HOME/src/xfind
    fi
fi
XFIND_BIN_PATH=$XFIND_PATH/bin
XFIND_SHARED_PATH=$XFIND_PATH/shared
XFIND_TEST_FILE_PATH=$XFIND_SHARED_PATH/testFiles

# Language roots
BASH_PATH=$XFIND_PATH/bash
C_PATH=$XFIND_PATH/c
CLOJURE_PATH=$XFIND_PATH/clojure
CPP_PATH=$XFIND_PATH/cpp
CSHARP_PATH=$XFIND_PATH/csharp
DART_PATH=$XFIND_PATH/dart
ELIXIR_PATH=$XFIND_PATH/elixir
FSHARP_PATH=$XFIND_PATH/fsharp
GO_PATH=$XFIND_PATH/go
GROOVY_PATH=$XFIND_PATH/groovy
HASKELL_PATH=$XFIND_PATH/haskell
JAVA_PATH=$XFIND_PATH/java
JAVASCRIPT_PATH=$XFIND_PATH/javascript
KOTLIN_PATH=$XFIND_PATH/kotlin
OBJC_PATH=$XFIND_PATH/objc
# OCAML_PATH=$XFIND_PATH/ocaml
PERL_PATH=$XFIND_PATH/perl
PHP_PATH=$XFIND_PATH/php
POWERSHELL_PATH=$XFIND_PATH/powershell
PYTHON_PATH=$XFIND_PATH/python
RUBY_PATH=$XFIND_PATH/ruby
RUST_PATH=$XFIND_PATH/rust
SCALA_PATH=$XFIND_PATH/scala
SWIFT_PATH=$XFIND_PATH/swift
TYPESCRIPT_PATH=$XFIND_PATH/typescript

# Language version roots
BASHFIND_PATH=$BASH_PATH/bashfind
CFIND_PATH=$C_PATH/cfind
CLJFIND_PATH=$CLOJURE_PATH/cljfind
CPPFIND_PATH=$CPP_PATH/cppfind
CSFIND_PATH=$CSHARP_PATH/csfind
DARTFIND_PATH=$DART_PATH/dartfind
EXFIND_PATH=$ELIXIR_PATH/exfind
FSFIND_PATH=$FSHARP_PATH/fsfind
GOFIND_PATH=$GO_PATH/gofind
GROOVYFIND_PATH=$GROOVY_PATH/groovyfind
HSFIND_PATH=$HASKELL_PATH/hsfind
JAVAFIND_PATH=$JAVA_PATH/javafind
JSFIND_PATH=$JAVASCRIPT_PATH/jsfind
KTFIND_PATH=$KOTLIN_PATH/ktfind
OBJCFIND_PATH=$OBJC_PATH/objcfind
# OCAMLFIND_PATH=$OCAML_PATH/mlfind
# MLFIND_PATH=$OCAML_PATH/mlfind
PLFIND_PATH=$PERL_PATH/plfind
PHPFIND_PATH=$PHP_PATH/phpfind
PS1FIND_PATH=$POWERSHELL_PATH/ps1find
PYFIND_PATH=$PYTHON_PATH/pyfind
RBFIND_PATH=$RUBY_PATH/rbfind
RSFIND_PATH=$RUST_PATH/rsfind
SCALAFIND_PATH=$SCALA_PATH/scalafind
SWIFTFIND_PATH=$SWIFT_PATH/swiftfind
TSFIND_PATH=$TYPESCRIPT_PATH/tsfind
