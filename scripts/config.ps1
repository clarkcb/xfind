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
$xfindCPath = Join-Path $xfindPath 'c'
$xfindClojurePath = Join-Path $xfindPath 'clojure'
$xfindCppPath = Join-Path $xfindPath 'cpp'
$xfindCsharpPath = Join-Path $xfindPath 'csharp'
$xfindDartPath = Join-Path $xfindPath 'dart'
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
$cfindPath = Join-Path $xfindCPath 'cfind'
$cljfindPath = Join-Path $xfindClojurePath 'cljfind'
$cppfindPath = Join-Path $xfindCppPath 'cppfind'
$csfindPath = Join-Path $xfindCsharpPath 'csfind'
$dartfindPath = Join-Path $xfindDartPath 'dartfind'
$fsfindPath = Join-Path $xfindFsharpPath 'fsfind'
$gofindPath = Join-Path $xfindGoPath 'gofind'
$groovyfindPath = Join-Path $xfindGroovyPath 'groovyfind'
$hsfindPath = Join-Path $xfindHaskellPath 'hsfind'
$javafindPath = Join-Path $xfindJavaPath 'javafind'
$jsfindPath = Join-Path $xfindJavascriptPath 'jsfind'
$ktfindPath = Join-Path $xfindKotlinPath 'ktfind'
$objcfindPath = Join-Path $xfindObjcPath 'objcfind'
# $mlfindPath = Join-Path $xfindOcamlPath 'mlfind'
$phpfindPath = Join-Path $xfindPhpPath 'phpfind'
$plfindPath = Join-Path $xfindPerlPath 'plfind'
$ps1findPath = Join-Path $xfindPowershellPath 'ps1find'
$pyfindPath = Join-Path $xfindPythonPath 'pyfind'
$rbfindPath = Join-Path $xfindRubyPath 'rbfind'
$rsfindPath = Join-Path $xfindRustPath 'rsfind'
$scalafindPath = Join-Path $xfindScalaPath 'scalafind'
$swiftfindPath = Join-Path $xfindSwiftPath 'swiftfind'
$tsfindPath = Join-Path $xfindTypescriptPath 'tsfind'
