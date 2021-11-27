# xfind

[xfind](https://github.com/clarkcb/xfind) is a command-line recursive file find utility
implemented in multiple programming languages, currently these [twenty-one](#why):

| Language       | URL |
| :------------ | :---------- |
| C | [https://en.wikipedia.org/wiki/C_(programming_language)](https://en.wikipedia.org/wiki/C_(programming_language)) |
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

* filter in/out by file extensions
* filter in/out directory paths by regex
* filter in/out file names by regex
* filter in/out by file types
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

You can build all language versions together by not passing an argument or passing
`'all'`:

```sh
$ ./scripts/build.sh
# -or-
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
(from 2021-11-25; ocaml version excluded due to currently unresolved issues):

```sh
$ python3 ./scripts/benchmark.py

 . . .

Outputs of all versions in all scenarios match

Total results for 10 out of 10 scenarios with 100 out of 100 total runs

             real     avg  rank    sys     avg  rank    user     avg  rank    total     avg  rank
---------  ------  ------  ----  -----  ------  ----  ------  ------  ----  -------  ------  ----
cfind        0.39  0.0039     1   0     0          2    0     0          1     0.39  0.0039     1
cljfind    109.6   1.096     20  23.08  0.2308    20  196.34  1.9634    20   329.02  3.2902    20
cppfind      1.58  0.0158     2   0     0          1    0.8   0.008      2     2.38  0.0238     2
csfind      19.08  0.1908    12   4.94  0.0494    15   12.41  0.1241    13    36.43  0.3643    12
dartfind    51.35  0.5135    19  13.1   0.131     19   56.16  0.5616    19   120.61  1.2061    19
fsfind      28.8   0.288     17   4.53  0.0453    12   22.32  0.2232    17    55.65  0.5565    17
gofind       3.95  0.0395     5   2.4   0.024     11    0.8   0.008      4     7.15  0.0715     6
hsfind       4.37  0.0437     7   1.51  0.0151     7    1.6   0.016      7     7.48  0.0748     7
javafind    20.47  0.2047    14   5.8   0.058     16   19.81  0.1981    15    46.08  0.4608    14
jsfind      14.63  0.1463    10   2.2   0.022      9   10.6   0.106     10    27.43  0.2743    10
ktfind      22.82  0.2282    15   6.01  0.0601    17   22.3   0.223     16    51.13  0.5113    16
objcfind     2.74  0.0274     3   0.8   0.008      4    0.8   0.008      5     4.34  0.0434     3
phpfind      8.18  0.0818     8   2.32  0.0232    10    4.01  0.0401     8    14.51  0.1451     8
plfind      10.11  0.1011     9   1.28  0.0128     6    7.06  0.0706     9    18.45  0.1845     9
pyfind      26.02  0.2602    16   4.88  0.0488    14   18.59  0.1859    14    49.49  0.4949    15
rbfind      19.39  0.1939    13   4.78  0.0478    13   12.27  0.1227    12    36.44  0.3644    13
rsfind       2.93  0.0293     4   0.85  0.0085     5    0.8   0.008      3     4.58  0.0458     4
scalafind   39     0.39      18   7.36  0.0736    18   49.45  0.4945    18    95.81  0.9581    18
swiftfind    3.96  0.0396     6   0.8   0.008      3    1.6   0.016      6     6.36  0.0636     5
tsfind      14.7   0.147     11   2.04  0.0204     8   10.72  0.1072    11    27.46  0.2746    11
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
  * [C](https://en.wikipedia.org/wiki/C_(programming_language)) - I've started a C version in the past and should finish it, if for no other than reason than to verify my assumption that it *should* be the fastest and most efficient version once complete<br/>UPDATE: I added the C version, and it is definitely the fastest, by more than I expected (see benchmark results above), although there are still some missing features
  * [Common Lisp](https://lisp-lang.org/) - I want to see how it compares to Clojure and learn more about macros
  * [Elixir](https://elixir-lang.org/)/[Erlang](https://www.erlang.org/) - Elixir is probably higher priority than Erlang, but it could be interesting to compare both
  * [Julia](https://julialang.org/) - Julia is described as a high-performance scripting language, so I'm interested to see how it compares to existing implementations
  * [Lua](http://www.lua.org/) - another language that I would like to compare with existing implementations
  * [Racket](https://racket-lang.org/) - this might be an alternate choice to Common Lisp, or another comparison point

## License

This project is licensed under the MIT license. See the [LICENSE](LICENSE) file for more info.
