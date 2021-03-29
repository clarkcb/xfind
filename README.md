# xfind

[xfind](https://github.com/clarkcb/xfind) is a command-line recursive file find utility
implemented in multiple programming languages, currently these [twenty](#why):

| Language       | URL |
| :------------ | :---------- |
| Clojure | [https://clojure.org/](https://clojure.org/) |
| C# | [https://docs.microsoft.com/en-us/dotnet/csharp/](https://docs.microsoft.com/en-us/dotnet/csharp/) |
| C++ | [https://www.stroustrup.com/C++.html](https://www.stroustrup.com/C++.html) |
| Dart | [https://dart.dev/](https://dart.dev/) |
| F# | [https://docs.microsoft.com/en-us/dotnet/fsharp/](https://docs.microsoft.com/en-us/dotnet/fsharp/) |
| Go | [https://golang.org/](https://golang.org/) |
| Haskell | [https://www.haskell.org/](https://www.haskell.org/) |
| Java | [https://www.java.com/](https://www.java.com/) |
| JavaScript | [https://nodejs.org/](https://nodejs.org/) |
| Kotlin | [https://kotlinlang.org/](https://kotlinlang.org/) |
| Objective-C | [https://en.wikipedia.org/wiki/Objective-C](https://en.wikipedia.org/wiki/Objective-C) |
| OCaml | [https://ocaml.org/](https://ocaml.org/) |
| Perl | [https://www.perl.org/](https://www.perl.org/) |
| PHP | [https://www.php.net/](https://www.php.net/) |
| Python | [https://www.python.org/](https://www.python.org/) |
| Ruby | [https://www.ruby-lang.org/](https://www.ruby-lang.org/) |
| Rust | [https://www.rust-lang.org/](https://www.rust-lang.org/) |
| Scala | [https://www.scala-lang.org/](https://www.scala-lang.org/) |
| Swift | [https://swift.org/](https://swift.org/) |
| TypeScript | [https://www.typescriptlang.org/](https://www.typescriptlang.org/) |


[Using](#usage) any language version, you can find files for numerous criteria, including:

* filter in/out by extensions
* filter in/out directory paths by regex
* filter in/out file names by regex
* filter in/out by file types
* find under multiple separate directories
* include/exclude hidden directories/files

There are some other features being added, such as:

* find files before and/or after lastmod date/time
* find files smaller and/or larger than a given size


The `xfind` repo is derived from [xsearch](https://github.com/clarkcb/xsearch). More specifically,
I created `xfind` by extracting the file finding functionality from `xsearch`, and also applying
some changes/additions. I ended up writing a python script to assist with creating `xfind` from
`xsearch`. It's called _xsearch2xfind.py_ and I added it in the _scripts_ directory.


## Why?

There are a number of "why questions" that can be asked about `xfind`, such as:

* Why create another file find CLI utility?
* Why write a version of the utility in X language?
* Why rewrite the utility in so many languages?!?

Those are really questions for `xsearch`, the project that `xfind` is derived from, and I *will*
answer those questions there. However, there are a couple of questions specific to `xfind`:

### Why create `xfind` from `xsearch`?

I created `xfind` from `xsearch` after I realized that I would like to be able to use just the file
finding functionality as a library dependency for another utility that I was working on to find
file duplicates (see: [pydupes](https://github.com/clarkcb/pydupes)). I also realized that a file
finding utility is useful on its own without the file searching part. Lastly, I realized that by
creating `xfind`, I could then modify `xsearch` to use `xfind` as a library dependency to provide
the file finding functionality, which would add another dimension for inter-language comparison.

By the way, the process of creating `xfind` from `xsearch` was a pretty interesting in and of
itself, have a look at the _scripts/xsearch2xfind.py_ script I created to help with that.

### Wasn't one multi-language project enough?

Honestly, yes. 😀 &nbsp; I have "reimplementation fatigue" from these projects, and I will probably
never do another multi-language project. That being said, I'm glad for doing them, it has been a
very educational and mostly enjoyable process, and it has provided me with a lot of knowledge and
experience that I wouldn't trade. There's a lot more I can say about it, and I hope to at some point.
For now the key takeaway that I would offer is that the experience of doing these projects left me
feeling well-equipped to answer questions like this:

* What languages would you most recommend for a given future project, considering the project's
  needs (rapid development/prototyping, high-performance, integration into existing environment,
  etc.)?


## Installation

There are three installation options for `xfind`:

1. Clone the repo only - if you are sure you will only want to compare language versions
   in an editor, then you can just clone the repo and go from there.
2. Build a Docker image and open in a container - this is recommended because it's much less
   effort and won't affect your base system, choose this if you think you will want to build
   and run different language versions for comparison.
3. Install different language compilers, interpreters, etc. locally - this is obviously more
   effort, but it might make sense it in cases where you're particularly interested in a small
   number of language versions and possibly even have some support for them installed already.


### Build Docker Image (recommended)

There is a _Dockerfile_ that enables building a Docker image locally in order to build and run
`xfind` in a container. This greatly simplifies the setup and building process and is highly
recommended.

There are actually two steps to this process: first build the image, then open a new container
instance of the image (opening in VS Code described below).

#### Building the Docker image

To build the Docker image, first make sure you have Docker installed on your system. I also
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

```
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

```
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

If you are interested in performance [comparisons](#comparison), and/or you want to try
[running](#usage) `xfind` in one or more languages, you will need to install the language
support for each language version you want to run, unless already installed on the system.
In many cases, the latest standard install for that language will work fine, but listed
below are some special cases/considerations:

* `clojure/cljfind` - the [leiningen](https://leiningen.org/) tool is used for package management and building
* `cpp/cppfind` - C++11 is required, but C++17 is recommended
* `csharp/CsFind` - [dotnet 5.0](https://dotnet.microsoft.com/download/dotnet/5.0)
* `fsharp/FsFind` - [dotnet 5.0](https://dotnet.microsoft.com/download/dotnet/5.0)
* `go/gofind` - the go version needs to support go modules (1.13+), but 1.16+ is recommended because `gofind` will be making use of the new [embed](https://golang.org/pkg/embed/) feature soon
* `haskell/hsfind` - this version requires the [stack](https://docs.haskellstack.org/en/stable/README/) utility (instead of `cabal`)
* `ocaml/mlfind` - this version uses [opam](https://opam.ocaml.org/) and [core](https://opam.ocaml.org/packages/core/), but I'm currently having problems building it on OSX, adding to TODOs to investigate
* `php/phpfind` - the [composer](https://getcomposer.org/) utility is used for dependency management, also need to use a version of PHP that supports classes and namespaces (7+?)
* `python/pyfind` - this version runs via the `asyncio` module, which requires python 3.7+


Another thing you will need to do is set an environment variable called `$XFIND_PATH` to
the path that you cloned `xfind` to. For example, on my OSX machine is it set to
this:

```
$ XFIND_PATH=$HOME/src/xfind
```

If undefined, `$XFIND_PATH` defaults to `$HOME/src/xfind`, so if you clone `xfind` to that
location you will have reasonably good success in running various versions and tools
without setting `$XFIND_PATH`, but setting it is strongly recommended regardless.

Finally, note that there are some useful utilities in the _scripts_ folder. Most require
`bash`, although some of those also have powershell versions you can use instead. There are
also several written in `python`, most notably _benchmark.py_ (see [Comparison](#comparison));
you will need `python3` to run those.


## Building

There is a build script provided to build any/all language versions, and you will definitely
want use it at least initially, because all language versions, regardless of whether
the language is compiled or interpreted, have some necessary build steps to put the version
into a runnable state.

The build script is under _scripts_ and named _build.sh_. If you are on Windows, or if you just
prefer powershell, you can also use _build.ps1_. To run the build for a specific language,
run the script on the command line with the name of the language (or the language's extension
that the language version name is derived from) as the argument. For example, you can build
the TypeScript version using either of these commands:

```
$ ./scripts/build.sh typescript
# -or-
$ ./scripts/build.sh ts
```

You can build all language versions together by not passing an argument or passing
`'all'`:

```
$ ./scripts/build.sh
# -or-
$ ./scripts/build.sh all
```

You can use the latter approach even if you don't have all necessary software installed to
build/run all language versions; the build script will simply point out what is missing
and move on.

For compiled languages that differentiate between debug and release builds, you can 
include `--debug` and/or `--release` to target those specific builds. If neither is
specified, debug-only will be assumed. Example:

```
$ ./scripts/build.sh --debug swift
# -or-
$ ./scripts/build.sh --release swift
# -or-
$ ./scripts/build.sh --debug --release swift
```

For each language version built, a softlink is created under `$XFIND_PATH/bin` (`go` and
`haskell` binaries are installed there directly), so after building you can try
running any version from there, either by changing to that directory or by adding it
to your path:

```
$ PATH=$PATH:$XFIND_PATH/bin
```


## Usage

This section concerns usage of the `xfind` tool by running any individual language version.
For information on running comparatively, see the [Comparison](#comparison) section.

Assuming you have `$XFIND_PATH/bin` in your path or that you are in that directory, you
can run any version with the `-h` to get the help/usage. Here's an example for the
python version:

```
$ pyfind -h

Usage:
 pyfind [options] <path> [<path> ...]

Options:
 --archivesonly            Find only archive files
 -c,--colorize             Colorize console output*
 -C,--nocolorize           Do not colorize console output
 -d,--in-dirpattern        Specify name pattern for directories to include in find
 -D,--out-dirpattern       Specify name pattern for directories to exclude from find
 --debug                   Set output mode to debug
 --excludehidden           Exclude hidden files and directories*
 -f,--in-filepattern       Specify name pattern for files to include in find
 -F,--out-filepattern      Specify name pattern for files to exclude from find
 -h,--help                 Print this usage and exit
 --in-archiveext           Specify extension for archive files to include in find
 --in-archivefilepattern   Specify name pattern for archive files to include in find
 --includehidden           Include hidden files and directories
 --listdirs                Generate a list of the matching directories after finding
 --listfiles               Generate a list of the matching files after finding
 --maxlastmod              Find files with lastmod less than or equal to maxlastmod
 --maxsize                 Find files with size <= maxsize
 --minlastmod              Find files with lastmod greater than or equal to minlastmod
 --minsize                 Find files with size >= minsize
 --out-archiveext          Specify extension for archive files to exclude from find
 --out-archivefilepattern  Specify name pattern for archive files to exclude from find
 -R,--norecursive          Do not find recursively (no subdirectories)
 -r,--recursive            Find recursively through subdirectories*
 --settings-file           A path to a JSON file with specified find settings
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
* Skip directories that match the name `node_module` or `dist`
* Find files that have `find` in the name
* Look for files under `$XFIND_PATH/javascript` and `$XFIND_PATH/typescript`

Here's what that looks like (using the `rust` version):

```
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

```
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

By default, the _benchmark.py_ script will default to running and comparing all language
versions, but this can be customized one of two ways:

1. pass a comma-separated language/ext code argument, e.g. `-l cpp,go,hs,objc,rs,swift`
2. modify the `lang_dict` dictionary in _xfind.py_

The _benchmark.py_ script executes a series of "scenarios" for each configured language
version, and outputs whether the results of all versions match with a table of ranked
performance. At the end, the performances values from all scenarios are summed and
averaged and a final summary table is presented. Here's an example of the final
output:

```
$ python3 ./scripts/benchmark.py

 . . .

Outputs of all versions in all scenarios match

Total results for 10 out of 10 scenarios with 100 out of 100 total runs

 xfind        real    avg   rank    sys    avg   rank   user    avg   rank  total    avg   rank
-----------------------------------------------------------------------------------------------
 cljfind    281.76   2.82     19  36.96   0.37     19 470.93   4.71     19 789.65   7.90     19
 cppfind      3.65   0.04      1   0.81   0.01      1   1.60   0.02      3   6.06   0.06      1
 csfind      89.09   0.89     13  14.09   0.14     16  31.59   0.32     13 134.77   1.35     13
 dartfind   139.80   1.40     18  20.85   0.21     18 116.23   1.16     17 276.88   2.77     17
 fsfind     104.68   1.05     16  12.60   0.13     15  45.76   0.46     14 163.04   1.63     16
 gofind       9.74   0.10      3   3.93   0.04     10   1.55   0.02      2  15.22   0.15      3
 hsfind      32.28   0.32      7   1.68   0.02      3   7.05   0.07      6  41.01   0.41      6
 javafind    91.72   0.92     15  10.22   0.10     13  60.17   0.60     16 162.11   1.62     15
 jsfind      36.52   0.37      8   2.66   0.03      7  11.35   0.11      7  50.53   0.51      8
 ktfind      90.42   0.90     14  10.23   0.10     14  53.39   0.53     15 154.04   1.54     14
 objcfind    16.02   0.16      5   2.33   0.02      4   2.98   0.03      4  21.33   0.21      4
 phpfind     42.48   0.42      9   3.47   0.03      9  12.17   0.12      9  58.12   0.58      9
 plfind      46.65   0.47     10   2.60   0.03      6  15.97   0.16     10  65.22   0.65     10
 pyfind      68.71   0.69     12   8.25   0.08     11  24.34   0.24     11 101.30   1.01     12
 rbfind      65.03   0.65     11   8.48   0.08     12  25.16   0.25     12  98.67   0.99     11
 rsfind       4.64   0.05      2   1.59   0.02      2   1.04   0.01      1   7.27   0.07      2
 scalafind  131.53   1.32     17  15.86   0.16     17 132.86   1.33     18 280.25   2.80     18
 swiftfind   14.73   0.15      4   2.75   0.03      8   4.11   0.04      5  21.59   0.22      5
 tsfind      26.95   0.27      6   2.60   0.03      5  11.59   0.12      8  41.14   0.41      7
```

Notice the line above the table that says "Output of all versions in all scenarios match". It is
important to see this and similar messages on all scenario runs, otherwise one of the language
versions isn't working properly and the results will be invalid. An obvious example of this would
be attempting to run language versions that aren't built.


## TODOs

* Add `minlastmod`, `maxlastmod`, `minsize` and `maxsize` functionality (currently only implemented in `python` version)
* Separate executable code from library code to make integrating into `xsearch` easier or possible
* Define an approach for identifying the type of "extensionless" filenames, perhaps a combination of:
  * Add established filenames to list in _filetypes.json_ (e.g. _.gitignore_, _Dockerfile_, _README_, etc.) and add a check for a filename match if filename is extensionless
  * Assume a file type for any filenames that are not identified (probably BINARY would be the safest), -or- do some very minimal type detection (detect binary/text from first few bytes?)
* Determine how archive file support should work, two options:
  1. Provide option to find files inside archives - in this case should change `archivesonly` and `includearchives` options to `inarchivesonly` and `findinarchives`, respectively
  2. Find archives the same as other files (without option to look inside them) - in this case should consider removing `archivesonly` and `includearchives` options
* Add documentation about the what/why/how of `xfind`
* Add `stats` option to get a json object with various stats, such as unique extensions / extension counts, etc.
* Resolve issues with building and running some language versions in Docker: Haskell, Objc and OCaml
* Research Docker best practices to determine if there are ways to reduce the image size
* Add other language versions (in alphabetical order and subject to change)
  * [C](https://en.wikipedia.org/wiki/C_(programming_language)) - I've started a C version in the past and should finish it, if for no other than reason than to verify my assumption that it *should* be the fastest and most efficient version once complete
  * [Common Lisp](https://lisp-lang.org/) - I want to see how it compares to Clojure and learn more about macros
  * [Elixir](https://elixir-lang.org/)/[Erlang](https://www.erlang.org/) - Elixir is probably higher priority than Erlang, but it could be interesting to compare both
  * [Julia](https://julialang.org/) - Julia is described as a high-performance scripting language, so I'm interested to see how it compares to existing implementations
  * [Lua](http://www.lua.org/) - another language that I would like to compare with existing implementations
  * [Racket](https://racket-lang.org/) - this might be an alternate choice to Common Lisp, or another comparison point


## License

This project is licensed under the MIT license. See the [LICENSE](LICENSE) file for more info.
