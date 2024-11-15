################################################################################
#
# config.ps1
#
# The common configuration for PowerShell scripts
#
################################################################################

########################################
# Configuration
########################################

# XFIND_PATH defaults to $HOME/src/xfind if not defined
if (-not (Test-Path env:XFIND_PATH)) { $env:XFIND_PATH = Join-Path $HOME 'src' 'xfind' }
$xfindPath = $env:XFIND_PATH
$xfindBinPath = Join-Path $xfindPath 'bin'
$xfindSharedPath = Join-Path $xfindPath 'shared'
$xfindTestFilePath = Join-Path $xfindSharedPath 'testFiles'

# Language roots
$xfindBashPath = Join-Path $xfindPath 'bash'
$xfindCPath = Join-Path $xfindPath 'c'
$xfindClojurePath = Join-Path $xfindPath 'clojure'
$xfindCppPath = Join-Path $xfindPath 'cpp'
$xfindCsharpPath = Join-Path $xfindPath 'csharp'
$xfindDartPath = Join-Path $xfindPath 'dart'
$xfindElixirPath = Join-Path $xfindPath 'elixir'
$xfindFsharpPath = Join-Path $xfindPath 'fsharp'
$xfindGoPath = Join-Path $xfindPath 'go'
$xfindGroovyPath = Join-Path $xfindPath 'groovy'
$xfindHaskellPath = Join-Path $xfindPath 'haskell'
$xfindJavaPath = Join-Path $xfindPath 'java'
$xfindJavascriptPath = Join-Path $xfindPath 'javascript'
$xfindKotlinPath = Join-Path $xfindPath 'kotlin'
$xfindObjcPath = Join-Path $xfindPath 'objc'
# $xfindOcamlPath = Join-Path $xfindPath 'ocaml'
$xfindPerlPath = Join-Path $xfindPath 'perl'
$xfindPhpPath = Join-Path $xfindPath 'php'
$xfindPowershellPath = Join-Path $xfindPath 'powershell'
$xfindPythonPath = Join-Path $xfindPath 'python'
$xfindRubyPath = Join-Path $xfindPath 'ruby'
$xfindRustPath = Join-Path $xfindPath 'rust'
$xfindScalaPath = Join-Path $xfindPath 'scala'
$xfindSwiftPath = Join-Path $xfindPath 'swift'
$xfindTypescriptPath = Join-Path $xfindPath 'typescript'

# Language version roots
$bashFindPath = Join-Path $xfindBashPath 'bashfind'
$cFindPath = Join-Path $xfindCPath 'cfind'
$cljFindPath = Join-Path $xfindClojurePath 'cljfind'
$cppFindPath = Join-Path $xfindCppPath 'cppfind'
$csFindPath = Join-Path $xfindCsharpPath 'csfind'
$dartFindPath = Join-Path $xfindDartPath 'dartfind'
$exFindPath = Join-Path $xfindElixirPath 'exfind'
$fsFindPath = Join-Path $xfindFsharpPath 'fsfind'
$goFindPath = Join-Path $xfindGoPath 'gofind'
$groovyFindPath = Join-Path $xfindGroovyPath 'groovyfind'
$hsFindPath = Join-Path $xfindHaskellPath 'hsfind'
$javaFindPath = Join-Path $xfindJavaPath 'javafind'
$jsFindPath = Join-Path $xfindJavascriptPath 'jsfind'
$ktFindPath = Join-Path $xfindKotlinPath 'ktfind'
$objcFindPath = Join-Path $xfindObjcPath 'objcfind'
# $mlFindPath = Join-Path $xfindOcamlPath 'mlfind'
$phpFindPath = Join-Path $xfindPhpPath 'phpfind'
$plFindPath = Join-Path $xfindPerlPath 'plfind'
$ps1FindPath = Join-Path $xfindPowershellPath 'ps1find'
$pyFindPath = Join-Path $xfindPythonPath 'pyfind'
$rbFindPath = Join-Path $xfindRubyPath 'rbfind'
$rsFindPath = Join-Path $xfindRustPath 'rsfind'
$scalaFindPath = Join-Path $xfindScalaPath 'scalafind'
$swiftFindPath = Join-Path $xfindSwiftPath 'swiftfind'
$tsFindPath = Join-Path $xfindTypescriptPath 'tsfind'
