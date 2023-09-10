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
$binPath = Join-Path $xfindPath 'bin'
$sharedPath = Join-Path $xfindPath 'shared'
$testFilePath = Join-Path $sharedPath 'testFiles'

# Language roots
$cPath = Join-Path $xfindPath 'c'
$clojurePath = Join-Path $xfindPath 'clojure'
$cppPath = Join-Path $xfindPath 'cpp'
$csharpPath = Join-Path $xfindPath 'csharp'
$dartPath = Join-Path $xfindPath 'dart'
$fsharpPath = Join-Path $xfindPath 'fsharp'
$goPath = Join-Path $xfindPath 'go'
$haskellPath = Join-Path $xfindPath 'haskell'
$javaPath = Join-Path $xfindPath 'java'
$javascriptPath = Join-Path $xfindPath 'javascript'
$kotlinPath = Join-Path $xfindPath 'kotlin'
$objcPath = Join-Path $xfindPath 'objc'
# $ocamlPath = Join-Path $xfindPath 'ocaml'
$perlPath = Join-Path $xfindPath 'perl'
$phpPath = Join-Path $xfindPath 'php'
$powershellPath = Join-Path $xfindPath 'powershell'
$pythonPath = Join-Path $xfindPath 'python'
$rubyPath = Join-Path $xfindPath 'ruby'
$rustPath = Join-Path $xfindPath 'rust'
$scalaPath = Join-Path $xfindPath 'scala'
$swiftPath = Join-Path $xfindPath 'swift'
$typescriptPath = Join-Path $xfindPath 'typescript'

# Language version roots
$cfindPath = Join-Path $cPath 'cfind'
$cljfindPath = Join-Path $clojurePath 'cljfind'
$cppfindPath = Join-Path $cppPath 'cppfind'
$csfindPath = Join-Path $csharpPath 'csfind'
$dartfindPath = Join-Path $dartPath 'dartfind'
$fsfindPath = Join-Path $fsharpPath 'fsfind'
$gofindPath = Join-Path $goPath 'gofind'
$hsfindPath = Join-Path $haskellPath 'hsfind'
$javafindPath = Join-Path $javaPath 'javafind'
$jsfindPath = Join-Path $javascriptPath 'jsfind'
$ktfindPath = Join-Path $kotlinPath 'ktfind'
$objcfindPath = Join-Path $objcPath 'objcfind'
# $mlfindPath = Join-Path $ocamlPath 'mlfind'
$phpfindPath = Join-Path $phpPath 'phpfind'
$plfindPath = Join-Path $perlPath 'plfind'
$ps1findPath = Join-Path $powershellPath 'ps1find'
$pyfindPath = Join-Path $pythonPath 'pyfind'
$rbfindPath = Join-Path $rubyPath 'rbfind'
$rsfindPath = Join-Path $rustPath 'rsfind'
$scalafindPath = Join-Path $scalaPath 'scalafind'
$swiftfindPath = Join-Path $swiftPath 'swiftfind'
$tsfindPath = Join-Path $typescriptPath 'tsfind'
