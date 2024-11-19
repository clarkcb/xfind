# xfind

[xfind](https://github.com/clarkcb/xfind) is a command-line recursive file find utility
implemented in multiple programming languages, currently these [twenty-four](#why) (twenty-five including Ocaml, but that version is currently defunct):

| Language       | URL |
| :------------ | :---------- |
| Bash | [https://en.wikipedia.org/wiki/Bash_(Unix_shell)](https://en.wikipedia.org/wiki/Bash_(Unix_shell)) |
| C | [https://en.wikipedia.org/wiki/C_(programming_language)](https://en.wikipedia.org/wiki/C_(programming_language)) |
| Clojure | [https://clojure.org/](https://clojure.org/) |
| C# | [https://learn.microsoft.com/en-us/dotnet/csharp/](https://learn.microsoft.com/en-us/dotnet/csharp/) |
| C++ | [https://www.stroustrup.com/C++.html](https://www.stroustrup.com/C++.html) |
| Dart | [https://dart.dev/](https://dart.dev/) |
| Elixir | [https://elixir-lang.org/](https://elixir-lang.org/) |
| F# | [https://learn.microsoft.com/en-us/dotnet/fsharp/](https://learn.microsoft.com/en-us/dotnet/fsharp/) |
| Go | [https://golang.org/](https://golang.org/) |
| Groovy | [https://groovy-lang.org/](https://groovy-lang.org/) |
| Haskell | [https://www.haskell.org/](https://www.haskell.org/) |
| Java | [https://www.java.com/](https://www.java.com/) |
| JavaScript | [https://nodejs.org/](https://nodejs.org/) |
| Kotlin | [https://kotlinlang.org/](https://kotlinlang.org/) |
| Objective-C | [https://en.wikipedia.org/wiki/Objective-C](https://en.wikipedia.org/wiki/Objective-C) |
| ~~OCaml~~ | ~~[https://ocaml.org/](https://ocaml.org/)~~ |
| Perl | [https://www.perl.org/](https://www.perl.org/) |
| PHP | [https://www.php.net/](https://www.php.net/) |
| PowerShell | [https://learn.microsoft.com/en-us/powershell/](https://learn.microsoft.com/en-us/powershell/) |
| Python | [https://www.python.org/](https://www.python.org/) |
| Ruby | [https://www.ruby-lang.org/](https://www.ruby-lang.org/) |
| Rust | [https://www.rust-lang.org/](https://www.rust-lang.org/) |
| Scala | [https://www.scala-lang.org/](https://www.scala-lang.org/) |
| Swift | [https://swift.org/](https://swift.org/) |
| TypeScript | [https://www.typescriptlang.org/](https://www.typescriptlang.org/) |

[Using](#usage) any language version, you can find files based on numerous criteria, including:

* filter by file extensions
* filter directory paths by [regex](https://en.wikipedia.org/wiki/Regular_expression)
* filter file names by regex
* filter by file types
* find files at a minimum and/or maximum depth
* find files using max/min values for lastmod and size
* find under multiple separate directories
* include/exclude hidden directories/files

There are some other features being added, such as:

* filter files by user and group
* filter files using found ignore files
* find files inside archive files (zip, tar.gz, etc.)

The `xfind` repo is derived from [xsearch](https://github.com/clarkcb/xsearch).

## Why?

There are a number of "why questions" that can be asked about `xfind`, such as:

* Why create another file find/search CLI utility?
* Why write a version of the utility in X language?
* Why rewrite the utility in so many languages?!?

Those are really better questions for `xsearch`, the project that `xfind` is derived from, and I
*will* answer those questions there. However, there are a couple of questions specific to `xfind`:

### Why create `xfind` from `xsearch`?

I created `xfind` from `xsearch` after realizing that the file finding portion of the functionality
would be useful as a library dependency for other projects, such as a utility to find file duplicates
(see: [pydupes](https://github.com/clarkcb/pydupes)). I also realized that a file finding utility with
regex filtering would be useful on its own when file searching is not needed. Lastly, it occurred
to me that I could modify `xsearch` to use the file finding library as an external dependency, which
would add another dimension for inter-language comparison.

The process of creating `xfind` from `xsearch` is touched on [here](#how).

### Wasn't one multi-language project enough?

Honestly, yes. 😀 &nbsp; I have "reimplementation fatigue" from these projects, and I will probably
never do another multi-language project. That being said, I'm glad for doing them, it has been a
very educational and mostly enjoyable experience that I wouldn't trade. There's a lot more I can
say about it, and I plan to. Have a look at the [conclusions](#conclusions) section for an overview
of how I plan to tackle that.

## How?

The high-level process of creating `xfind` from `xsearch` included these steps:

1. Clone a copy of the `xsearch` repo, renaming the root directory to `xfind`
2. Write and execute a conversion script on the source under `xfind` (see _scripts/xsearch2xfind.py_)
3. Manually edit the source to finish the conversion - remove file search functionality, etc.

Looking back, there was more I could have added to the conversion script to further simplify
the manual editing process, but it provided a good start. It did also get me thinking about
programming language translation as another possible experimental project...

## Installation

The first step is to clone `xfind`. By default, it is expected to be located at `$HOME/src/xfind`,
but this can be changed by setting `XFIND_PATH` environment variable to wherever it has been cloned to.

```sh
$ git clone https://github.com/clarkcb/xfind.git
```

Once cloned, you have two installation options for `xfind`:

1. Build a Docker image and open in a container
2. Build/run on your local system, installing any needed compilers, interpreters, etc.

The Docker image option is recommended if you think you will want to build and run all or many of
the different language versions for comparison, since the setup will be easier and it won't effect
your local system.

If you are only interested in code comparisons, or if you only want to run/compare a subset of the
language versions, then building and running on your local system might make sense, especially if
you already have support for those languages installed.

### Build Docker Image

There is a _Dockerfile_ that enables building a Docker image locally in order to build and run
`xfind` in a container. This simplifies the setup and building process and is recommended if you
want to build and run all or many language versions.

There are two steps to this process: first build the image, then open a new container
instance of the image (opening in VS Code is described below).

#### Building the Docker image

To build the Docker image, you will need to have Docker installed on your system. I also
recommend enabling `experimental` in the Docker engine configuration in order to enable the
`--squash` option for building an image, which is done by putting the following JSON in
the Docker Engine config:

```json
{
  "experimental": true
}
```

Next, open a terminal in the `xfind` root directory and cd into the _.devcontainer_
subdirectory. There, run the following command (include `--squash` if you enabled
`experimental`):

```sh
$ docker build --squash -t xfind .
```

This build will take a long time, probably at least a half hour on a typical system with
a typical internet connection. To see the specifics of what is happening, have a look at
the _Dockerfile_, but in general, the necessary components to build and run (most of) the
language versions of `xfind` in a container are downloaded and installed into a base ubuntu
image. If the build is interrupted or stalls at any point, it should be possible to restart
it by issuing the same command and have it continue close to where it left off.

After the image is built, you should be able to see it listed in your images when using
the `docker images` command, it should include a line similar to this:

```sh
REPOSITORY          TAG           IMAGE ID       CREATED        SIZE
xfind               latest        ca8980e929b1   12 hours ago   5.94GB
```

Yes, the image is big, and it probably means there's something not quite right in the way
I've configured the build, even with squashing. I have a TODO to research this.

Now that you have a built image, you can run it one of several ways. I recommend opening it
inside VS Code.

#### Opening in VS Code

First, open `xfind` in VS Code as you would a project directory. VS Code should automatically
detect that the project is configured to be able to be opened inside a container and display
a popup asking if you want to do so, which you can confirm by clicking on the "Reopen in Container"
button. Otherwise, you can click in the green area in the far lower left corner on the status bar
and select the option to reopen in container from the menu.

The first time you open `xfind` in a container in VS Code, some more installations will be
triggered, and this can take some time too, although not nearly as much as builing the image.
These installations are VS Code extensions to provide extra functionality for many of the languages,
build systems, etc. I tried to pick ones that are most popular / standard for a given language in
cases where it's obvious, as well as a few that I found useful and not too intrusive. The list
is in the _devcontainer.json_ file in the `extensions` array.

The next step will be to [build](#building) the language versions of `find` and compare them.

### Installation on local machine

If you are primarily interested in specific language versions, and especially if you already
have some or all of the language support for those versions installed locally, this installation
option could make sense.

In this scenario, you will need to make sure you have the proper language support installed for
each version that you want to build/run.

In general, different OSes provide or have available systems for managing installation of multiple
software packages from a single interface:

* Linux - Ubuntu uses the `apt` command to install/manage software installs, Red Hat uses `rpm`, etc.
* OSX - a popular package manager and the one I would recommend is [Homebrew](brew.sh)
* Windows - from my limited experience, [Chocolatey](https://chocolatey.org/) seems to be a popular
package manager. If your version of Windows supports WSL (Windows Subsystem for Linux), you could
install into an active subsystem using a Linux package manager. It is also possible to install
something similar on older Windows using [Cygwin](https://www.cygwin.com/).

It is also possible to install language support separately per language. Providing in-depth
installation instructions for each individual language is beyond the scope of this README, but here
is some basic info:

* `Bash (bashfind)` - On Unix-style OSes (Linux, OSX), `bash` should be installed by default, but
`bashfind` will also run in `zsh`. On Windows, WSL or Cygwin is recommended.
* `C (cfind)` - You will need a C/C++ compiler like `gcc` or `clang`. On Linux, use a package manager
to install the recommended tools. For example, on Ubuntu, you would run `sudo apt update` followed by
`sudo apt install build-essential`. On OSX, installing XCode will install all necessary compiler
components. On Windows, Visual Studio is a recommended choice that will provide the necessary tools.
* `Clojure (cljfind)` - The [Getting Started](https://clojure.org/guides/getting_started) page on
[clojure.org](https://clojure.org) includes installation instructions.
* `C++ (cppfind)` - The instructions for `C / cfind` also apply here.
* `C# (CsFind)` - `CsFind` runs on .NET 9.0, download available [here](https://dotnet.microsoft.com/download/)
* `Dart (dartfind)` - You can download the Dart SDK from [here](https://dart.dev/get-dart)
* `Elixir (exfind)` - Instructions for installing elixir can be found [here](https://elixir-lang.org/install.html)
* `F# (FsFind)` - `FsFind` also runs on .NET 9.0, download available [here](https://dotnet.microsoft.com/download/)
* `Go (gofind)` - Download the OS-appropriate installer from [here](https://go.dev/dl/)
* `Groovy (groovyfind)` - The download page is at [here](https://groovy.apache.org/download.html)
* `Haskell (hsfind)` - Installation instructions [here](https://www.haskell.org/downloads/)
* `Java (javafind)` - Many different Java installs are available from [Oracle](https://www.oracle.com/java/technologies/downloads/)
and third parties (e.g. [Red Hat](https://developers.redhat.com/products/openjdk/overview)), but be
sure to install a JDK for at least Java version 11, preferably 17+.
* `JavaScript (jsfind)` - `jsfind` runs on Node.js, download avaible from [here](https://nodejs.org/en)
* `Kotlin (ktfind)` - To install support for Kotlin, you will need to install the IntelliJ IDEA or
Android Studio IDE from JetBrains.
* `Objective-C (objcfind)` - On OSX, an Xcode install will provide everything needed. I don't have
experience with this language on other platforms, but I believe that `gcc` and maybe `clang` have
some support for compiling it.
* `OCaml (mlfind)` - See [https://ocaml.org/(https://ocaml.org/)]
* `Perl (plfind)` - On Unix-style OSes, `perl` should be installed by default. On Windows, you can
install [Strawberry Perl](https://strawberryperl.com/) or
[ActiveState Perl](https://www.activestate.com/platform/supported-languages/perl/), or install it
via Cygwin.
* `PHP (phpfind)` - `phpfind` requires `php` verison 8.3+. On Unix-style OSes, `php` should be
installed by default, although it might be an older version. On Windows, you can
download and build `php` from source, but your best bet will probably be to install via WSL or
Cygwin.
* `PowerShell (ps1find)` - On most modern versions of Windows, PowerShell should be installed by
default. It can also be installed on
[OSX](https://learn.microsoft.com/en-us/powershell/scripting/install/installing-powershell-on-macos)
and [Linux](https://learn.microsoft.com/en-us/powershell/scripting/install/installing-powershell-on-linux)
* `Python (pyfind)` - `pyfind` requires Python 3.9 or higher. Find downloads for various OSes and
versions [here](https://www.python.org/downloads/)
* `Ruby (rbfind)` - `rbfind` requires Ruby 3.x. Find downloads for various OSes and versions
[here](https://www.ruby-lang.org/en/downloads/)
* `Rust (rsfind)` - See the [Getting started](https://www.rust-lang.org/learn/get-started) page for
installation instructions
* `Scala (scalafind)` - `scalafind` requires Scala 3.x. Downloads available from the
[Scala lang uagehome page](https://www.scala-lang.org/)
* `Swift (swiftfind)` - Though it is a language created by Apple, there are installations
available for Linux and Windows, see this [page](https://www.swift.org/install/macos/)
* `TypeScript (tsfind)` - `tsfind` also runs on Node.js, download avaible from [here](https://nodejs.org/en)



Several of the language versions require additional software to build/manage dependencies:

* `Clojure (cljfind)` - the [leiningen](https://leiningen.org/) tool is used for package management and building
* `Haskell (hsfind)` - this version requires the [stack](https://docs.haskellstack.org/en/stable/README/)
utility (instead of just `cabal`)
* `Java (javafind)` - the [gradle](https://gradle.org/) tool is used to build `javafind`
* `Kotlin (ktfind)` - the [gradle](https://gradle.org/) tool is also used to build `ktfind`
* `PHP (phpfind)` - the [composer](https://getcomposer.org/) utility is used for dependency management
* `Scala (scalafind)` - the [sbt](https://scala-sbt.org/) tool is also used to build `scalafind`

You will also need to set an environment variable called `$XFIND_PATH` to the path that you cloned
`xfind` to. For example, on my OSX machine is it set to this:

```sh
XFIND_PATH=$HOME/src/xfind
```

If undefined, `$XFIND_PATH` defaults to `$HOME/src/xfind`, so if you clone `xfind` to that
location you will have reasonably good success in running various versions and tools
without setting `$XFIND_PATH`, but setting it is strongly recommended nonetheless.

Finally, note that there are some useful utilities in the _scripts_ folder. Most require
`bash`, although some of those also have powershell versions you can use instead. There are
also several written in `python`, most notably _benchmark.py_ (see [Comparison](#comparison));
you will need `python3` to run those.

## Building

There is a build script provided to build any/all language versions, and you will definitely
need use it at least initially, because all language versions, regardless of whether
the language is compiled or interpreted, have some necessary build steps to put the version
into a runnable state. For example, many of the language builds copy resource files from a
shared location to the language version's local resource location.

The build script is under _scripts_ and named _build.sh_. If you are on Windows, or if you just
prefer powershell, you can also use _build.ps1_. To run the build for a specific language,
run the script on the command line with the name of the language (or the language's extension
that the language version name is derived from) as the argument. For example, you can build
the TypeScript version using either of these commands:

```sh
$ ./scripts/build.sh typescript
# -or-
$ ./scripts/build.sh ts
```

You can specify more than one language version to build:

```sh
$ ./scripts/build.sh c cpp go hs objc rs swift
```

You can build all language versions together by passing `all`:

```sh
$ ./scripts/build.sh all
```

If you try to build any language version without the necessary software installed, the build
script will simply point out what is missing and move on.

For each language version successfully built, a softlink to the executable is created under
`$XFIND_PATH/bin` (`gofind` and `hsfind` binaries are installed there directly), so after building
you can try running any version from there, either by changing to that directory or by adding it to
your path:

```sh
PATH=$PATH:$XFIND_PATH/bin
```

For compiled languages that differentiate between debug and release builds, you can 
include `--debug` and/or `--release` to target those specific builds (they will be ignored for
languages that don't differentiate). If neither is specified, debug-only will be assumed. If both
are specified, both builds will run, but the softlink will be created for the release version.
Examples:

```sh
$ ./scripts/build.sh --debug swift
# -or-
$ ./scripts/build.sh --release swift
# -or-
$ ./scripts/build.sh --debug --release swift
```

Specific to Python, there is also a `--venv` option that indicates that a virtual environment should
be used, either using an existing one if found or creating a new one, when building and running
`pyfind`. I recommend using this, because it isolates dependencies to the given virtual environment.

When I want to build all language versions, say after a new feature has been added, I will usually
do a `clean`, then a `build` and finally a `unittest`:

```sh
$ ./scripts/clean.sh all
# . . .
$ ./scripts/build.sh --debug --release --venv all
# . . .
$ ./scripts/unittest.sh all
```


## Usage

This section concerns usage of the `xfind` tool by running any individual language version.
For information on running comparatively, see the [Comparison](#comparison) section.

Assuming you have `$XFIND_PATH/bin` in your path or that you are in that directory, you
can run any version with the `-h` to get the help/usage. Here's an example for the
python version:

```sh
$ pyfind -h

Usage:
 pyfind [options] <path> [<path> ...]

Options:
 --archivesonly            Find only archive files
 -d,--in-dirpattern        Specify name pattern for directories to include in find
 -D,--out-dirpattern       Specify name pattern for directories to exclude from find
 --debug                   Set output mode to debug
 --excludehidden           Exclude hidden files and directories*
 -f,--in-filepattern       Specify name pattern for files to include in find
 -F,--out-filepattern      Specify name pattern for files to exclude from find
 --followsymlinks          Follow symlinks to their linked files or directories
 -h,--help                 Print this usage and exit
 --in-archiveext           Specify extension for archive files to include in find
 --in-archivefilepattern   Specify name pattern for archive files to include in find
 --includehidden           Include hidden files and directories
 --maxdepth                Find files at most maxdepth levels below startpath
 --maxlastmod              Find files with lastmod less than or equal to maxlastmod
 --maxsize                 Find files with size <= maxsize
 --mindepth                Find files at least mindepth levels below startpath
 --minlastmod              Find files with lastmod greater than or equal to minlastmod
 --minsize                 Find files with size >= minsize
 --nofollowsymlinks        Do not follow symlinks to their linked files or directories
 --noprintdirs             Do not print matching directories after finding*
 --noprintfiles            Do not print matching files after finding
 --out-archiveext          Specify extension for archive files to exclude from find
 --out-archivefilepattern  Specify name pattern for archive files to exclude from find
 --printdirs               Print matching directories after finding
 --printfiles              Print matching files after finding*
 -R,--norecursive          Do not find recursively (no subdirectories)
 -r,--recursive            Find recursively through subdirectories*
 --settings-file           Path to JSON file with specified find settings
 --sort-ascending          Sort results in ascending order*
 --sort-by                 Sort by: PATH, NAME, TYPE, SIZE, LASTMOD
 --sort-caseinsensitive    Sort results case-insensitive
 --sort-casesensitive      Sort results case-sensitive*
 --sort-descending         Sort results in descending order
 -t,--in-filetype          File type to find (text, binary)
 -T,--out-filetype         File type not to find (text, binary)
 -v,--verbose              Set output mode to verbose
 -V,--version              Print version and exit
 -x,--in-ext               Specify extension for files to include in find
 -X,--out-ext              Specify extension for files to exclude from find
 -Z,--excludearchives      Exclude archive files (bz2, gz, tar, zip)*
 -z,--includearchives      Include archive files (bz2, gz, tar, zip)

```

Now try running it to find specific files under `$XFIND_PATH`, using the following
criteria:

* Find files with `js` or `ts` extension
* Skip directories that match `node_module` or `dist`
* Find files that have `find` in the name
* Look for files under `$XFIND_PATH/javascript` and `$XFIND_PATH/typescript`

Here's what that looks like (using the `rust` version):

```sh
$ cd $XFIND_PATH
$ rsfind -x js,ts -D node_module -D dist -f find ./javascript ./typescript

Matching files (22):
./javascript/jsfind/src/finder.js
./javascript/jsfind/src/finderror.js
./javascript/jsfind/src/findfile.js
./javascript/jsfind/src/findoption.js
./javascript/jsfind/src/findoptions.js
./javascript/jsfind/src/findsettings.js
./javascript/jsfind/src/jsfind.js
./javascript/jsfind/tests/finder.test.js
./javascript/jsfind/tests/findfile.test.js
./javascript/jsfind/tests/findoptions.test.js
./javascript/jsfind/tests/findsettings.test.js
./typescript/tsfind/src/finder.ts
./typescript/tsfind/src/finderror.ts
./typescript/tsfind/src/findfile.ts
./typescript/tsfind/src/findoption.ts
./typescript/tsfind/src/findoptions.ts
./typescript/tsfind/src/findsettings.ts
./typescript/tsfind/src/tsfind.ts
./typescript/tsfind/tests/finder.test.ts
./typescript/tsfind/tests/findfile.test.ts
./typescript/tsfind/tests/findoptions.test.ts
./typescript/tsfind/tests/findsettings.test.ts
```

Now change the command to *skip* files that have `find` in the name
(and use the `go` version this time):

```sh
$ gofind -x js,ts -D node_module -D dist -F find ./javascript ./typescript

Matching files (16):
javascript/jsfind/jest.config.js
javascript/jsfind/src/common.js
javascript/jsfind/src/config.js
javascript/jsfind/src/filetype.js
javascript/jsfind/src/filetypes.js
javascript/jsfind/src/fileutil.js
javascript/jsfind/tests/filetypes.test.js
javascript/jsfind/tests/fileutil.test.js
typescript/tsfind/jest.config.js
typescript/tsfind/src/common.ts
typescript/tsfind/src/config.ts
typescript/tsfind/src/filetype.ts
typescript/tsfind/src/filetypes.ts
typescript/tsfind/src/fileutil.ts
typescript/tsfind/tests/filetypes.test.ts
typescript/tsfind/tests/fileutil.test.ts
```

## Comparison

There are several scripts in the _scripts_ directory to help with comparing the language
versions in various ways, but the one that will likely be of primary interest is the
python script _benchmark.py_, an *unscientific* tool for comparing performance and
functionality (i.e. ensuring matching output of all versions).

By default, the _benchmark.py_ script will run and compare all language versions, but
this can be customized one of two ways:

1. pass a comma-separated language/ext code argument, e.g. `-l c,cpp,go,hs,objc,rs,swift`
2. modify the `lang_dict` dictionary in _xfind.py_

The _benchmark.py_ script executes a series of "scenarios" for each configured language
version, and outputs whether the results of all versions match with a table of ranked
performance. At the end, the performances values from all scenarios are summed and
averaged and a final summary table is presented. 

The scenarios can be read from a json file, and the included _scenarios.json_ will be
the default if no file is specified.

Here's what the help looks like for benchmark.py:


```sh
$ python3 ./scripts/benchmark.py -h
usage: benchmark.py [-h] [-g [GROUP ...]] [-s [SCENARIO ...]] [-l LANGS] [-L NOLANGS] [-r RUNS] [-b] [-f SCENARIOS_FILE] [--debug]

Run xfind benchmark

options:
  -h, --help            show this help message and exit
  -g [GROUP ...], --group [GROUP ...]
                        Name of scenario group to run
  -s [SCENARIO ...], --scenario [SCENARIO ...]
                        Name of scenario to run
  -l LANGS, --langs LANGS
                        Comma-separated list of languages to include in benchmark
  -L NOLANGS, --nolangs NOLANGS
                        Comma-separated list of languages to exclude from benchmark
  -r RUNS, --runs RUNS  Number of runs for each scenario
  -b, --exit-on-diff    Exit on first output difference
  -f SCENARIOS_FILE, --scenarios-file SCENARIOS_FILE
                        A scenarios json file
  --debug               Print debug output
```


Here's an example of the final output from a run that was executed on 2024-11-14:

```sh
$ python3 ./scripts/benchmark.py

 . . .

Outputs of all versions in all scenarios match

Total results for 45 out of 45 scenarios with 450 out of 450 total runs

Date/time:  2024-11-18 16:46:08.988141
Git branch: "develop" (a11e9281af1bd490ef37f02d0b06aa1536720c95)

              total         avg    rank
----------  -------  ----------  ------
gofind         2.41  0.00535556       1
cfind          4.38  0.00973333       2
cppfind        4.52  0.0100444        3
rsfind        15.26  0.0339111        4
dartfind      33.68  0.0748444        5
objcfind      39.53  0.0878444        6
hsfind        41.65  0.0925556        7
swiftfind     55.06  0.122356         8
phpfind      132.17  0.293711         9
jsfind       207.09  0.4602          10
tsfind       211.11  0.469133        11
pyfind       241.21  0.536022        12
csfind       257.62  0.572489        13
fsfind       328.93  0.730956        14
javafind     411.36  0.914133        15
plfind       445.78  0.990622        16
exfind       518.84  1.15298         17
ktfind       592.61  1.31691         18
scalafind    850.37  1.88971         19
rbfind      1083.55  2.40789         20
ps1find     1583.9   3.51978         21
bashfind    1934.13  4.29807         22
groovyfind  2972.74  6.60609         23
cljfind     3340.5   7.42333         24
```

Notice the line above the table that says "Output of all versions in all scenarios match". It is
important to see this and similar messages on all scenario runs, otherwise one of the language
versions isn't working properly and the results will be invalid. An obvious example of this would
be attempting to run language versions that aren't built.

## Conclusions

In this section I will write about the experience of developing these projects, writing the
different language versions, and what personal conclusions I drew from it. For now I will just
outline the approach I will use.

Here's a list of criteria to evaluate each language by:

* documentation / resources
* learning curve
* readability
* core library
* building/running
* managing dependencies
* speed of development
* efficiency/performance
* platform agnosticity

The conclusions from these are helpful in determining which languages are most and least suited for
given requirements:

* one-off utilities / scripting
* high-performance
* cross-platform
* rich core and/or third-party dependencies
* specific platform (e.g. iOS or Android)
* specific framework (e.g. JVM or CLR)

I will give summaries of the experience of developing each of the language versions, and then
rank them by criteria and requirements.

## TODOs

* Add mime type support - detection, filtering, wildcards. This is nearly complete.
* Determine how archive file support should work, two options:
  1. Provide option to find files inside archives - in this case should change `archivesonly` and `includearchives` options to `inarchivesonly` and `findinarchives`, respectively
  2. Find archives the same as other files (without option to look inside them) - in this case should consider removing `archivesonly` and `includearchives` options
* Add documentation about the what/why/how of `xfind`
* Add `stats` option to get a json object with various stats, such as unique extensions / extension counts, etc.
* Resolve OCaml issues
* Research Docker best practices to determine if there are ways to reduce the image size
* Add other language versions (in alphabetical order and subject to change)
  * [Common Lisp](https://lisp-lang.org/) - I want to see how it compares to Clojure and learn more about macros
  * [Julia](https://julialang.org/) - Julia is described as a high-performance scripting language, so I'm interested to see how it compares to existing implementations
  * [Lua](http://www.lua.org/) - another language that I would like to compare with existing implementations
  * [Racket](https://racket-lang.org/) - this might be an alternate choice to Common Lisp, or another comparison point

## License

This project is licensed under the MIT license. See the [LICENSE](LICENSE) file for more info.
