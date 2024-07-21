# xfind

[xfind](https://github.com/clarkcb/xfind) is a command-line recursive file find utility
implemented in multiple programming languages, currently these [twenty-three](#why):

| Language       | URL |
| :------------ | :---------- |
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
| OCaml | [https://ocaml.org/](https://ocaml.org/) |
| Perl | [https://www.perl.org/](https://www.perl.org/) |
| PHP | [https://www.php.net/](https://www.php.net/) |
| PowerShell | [https://learn.microsoft.com/en-us/powershell/](https://learn.microsoft.com/en-us/powershell/) |
| Python | [https://www.python.org/](https://www.python.org/) |
| Ruby | [https://www.ruby-lang.org/](https://www.ruby-lang.org/) |
| Rust | [https://www.rust-lang.org/](https://www.rust-lang.org/) |
| Scala | [https://www.scala-lang.org/](https://www.scala-lang.org/) |
| Swift | [https://swift.org/](https://swift.org/) |
| TypeScript | [https://www.typescriptlang.org/](https://www.typescriptlang.org/) |

[Using](#usage) any language version, you can find files for numerous criteria, including:

* filter in/out by file extensions
* filter in/out directory paths by regex
* filter in/out file names by regex
* filter in/out by file types
* find before/after max/min values for lastmod and size
* find under multiple separate directories
* include/exclude hidden directories/files

There are some other features being added, such as:

* find files before and/or after lastmod date/time
* find files smaller and/or larger than a given size

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

The [process](#how) of creating `xfind` from `xsearch` was kind of interesting in and of itself.

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

There are three installation options for `xfind`:

1. Clone the repo only - if you think you will only want to compare language versions in an
   editor, then you can just clone the repo and go from there.
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

If you are interested in performance [comparisons](#comparison), and/or you want to try
[running](#usage) `xfind` in one or more languages, you will need to install the language
support for each language version you want to run, unless already installed on the system.
In many cases, the latest standard install for that language will work fine, but listed
below are some special cases/considerations:

* `clojure/cljfind` - the [leiningen](https://leiningen.org/) tool is used for package management and building
* `cpp/cppfind` - C++11 is required, but C++17 is recommended
* `csharp/CsFind` - [dotnet 6.0](https://dotnet.microsoft.com/download/dotnet/6.0)
* `fsharp/FsFind` - [dotnet 6.0](https://dotnet.microsoft.com/download/dotnet/6.0)
* `go/gofind` - the go version needs to support go modules (1.13+), but 1.16+ is recommended because `gofind` will be making use of the new [embed](https://golang.org/pkg/embed/) feature soon
* `haskell/hsfind` - this version requires the [stack](https://docs.haskellstack.org/en/stable/README/) utility (instead of just `cabal`)
* `ocaml/mlfind` - this version uses [opam](https://opam.ocaml.org/) and [core](https://opam.ocaml.org/packages/core/), but I'm currently having problems building it on OSX, adding to TODOs to investigate
* `php/phpfind` - the [composer](https://getcomposer.org/) utility is used for dependency management, also need to use a version of PHP that supports classes and namespaces (7+?)
* `python/pyfind` - this version runs via the `asyncio` module, which requires python 3.7+

Another thing you will need to do is set an environment variable called `$XFIND_PATH` to
the path that you cloned `xfind` to. For example, on my OSX machine is it set to
this:

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
want use it at least initially, because all language versions, regardless of whether
the language is compiled or interpreted, have some necessary build steps to put the version
into a runnable state.

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

You can build all language versions together by passing `'all'`:

```sh
$ ./scripts/build.sh all
```

You can use the latter approach even if you don't have all necessary software installed to
build/run all language versions; the build script will simply point out what is missing
and move on.

For each language version built, a softlink to the executable is created under `$XFIND_PATH/bin`
(`go` and `haskell` binaries are installed there directly), so after building you can try
running any version from there, either by changing to that directory or by adding it to your path:

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
 -h,--help                 Print this usage and exit
 --in-archiveext           Specify extension for archive files to include in find
 --in-archivefilepattern   Specify name pattern for archive files to include in find
 --includehidden           Include hidden files and directories
 --listdirs                Generate a list of the matching directories after finding
 --listfiles               Generate a list of the matching files after finding
 --maxdepth                Find files at most maxdepth levels below startpath
 --maxlastmod              Find files with lastmod less than or equal to maxlastmod
 --maxsize                 Find files with size <= maxsize
 --mindepth                Find files at least mindepth levels below startpath
 --minlastmod              Find files with lastmod greater than or equal to minlastmod
 --minsize                 Find files with size >= minsize
 --out-archiveext          Specify extension for archive files to exclude from find
 --out-archivefilepattern  Specify name pattern for archive files to exclude from find
 -R,--norecursive          Do not find recursively (no subdirectories)
 -r,--recursive            Find recursively through subdirectories*
 --settings-file           A path to a JSON file with specified find settings
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
averaged and a final summary table is presented. Here's an example of the final output
(from 2023-01-07; ocaml version excluded due to currently unresolved issues):

```sh
$ python3 ./scripts/benchmark.py

 . . .

Outputs of all versions in all scenarios match

Total results for 10 out of 10 scenarios with 100 out of 100 total runs

             real     avg    rank    sys     avg    rank    user     avg    rank    total     avg    rank
---------  ------  ------  ------  -----  ------  ------  ------  ------  ------  -------  ------  ------
cfind        0.16  0.0016       1   0     0            3    0     0            2     0.16  0.0016       1
cljfind    142.92  1.4292      20  30.02  0.3002      20  278.67  2.7867      21   451.61  4.5161      21
cppfind      2.2   0.022        3   0     0            2    0.8   0.008        5     3     0.03         3
csfind      24.39  0.2439      12   6.8   0.068       13   14.16  0.1416      12    45.35  0.4535      12
dartfind    68.79  0.6879      19  15.4   0.154       19   67.74  0.6774      18   151.93  1.5193      18
fsfind      38.35  0.3835      15   7.01  0.0701      14   26.79  0.2679      14    72.15  0.7215      14
gofind       0.74  0.0074       2   0     0            1    0     0            1     0.74  0.0074       2
hsfind       5.38  0.0538       6   1.8   0.018        7    2.56  0.0256       7     9.74  0.0974       6
javafind    30.82  0.3082      13   7.53  0.0753      15   36.41  0.3641      16    74.76  0.7476      15
jsfind      18.47  0.1847      10   2.81  0.0281      10   13.69  0.1369      10    34.97  0.3497      10
ktfind      40.15  0.4015      16   8.93  0.0893      16   51.01  0.5101      17   100.09  1.0009      16
objcfind     4.16  0.0416       4   0.83  0.0083       4    0.8   0.008        3     5.79  0.0579       4
phpfind     12.17  0.1217       9   2.8   0.028        9    7.74  0.0774       9    22.71  0.2271       9
plfind      11.3   0.113        8   1.81  0.0181       8    7.53  0.0753       8    20.64  0.2064       8
ps1find    156.72  1.5672      21  45.62  0.4562      21  151.4   1.514       20   353.74  3.5374      20
pyfind      38.29  0.3829      14   6.67  0.0667      12   26.73  0.2673      13    71.69  0.7169      13
rbfind      62.22  0.6222      17  11.06  0.1106      17   36.23  0.3623      15   109.51  1.0951      17
rsfind       5.02  0.0502       5   1.6   0.016        6    0.8   0.008        4     7.42  0.0742       5
scalafind   62.28  0.6228      18  12.47  0.1247      18   86.06  0.8606      19   160.81  1.6081      19
swiftfind    6.93  0.0693       7   1.53  0.0153       5    2.42  0.0242       6    10.88  0.1088       7
tsfind      19.04  0.1904      11   2.82  0.0282      11   14     0.14        11    35.86  0.3586      11
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
