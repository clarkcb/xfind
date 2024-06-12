# ExFind

`ExFind` is an elixir implementation of the [xfind](https://github.com/clarkcb/xfind)
recursive file finding utility.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `exfind` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:exfind, "~> 0.1.0"}
  ]
end
```

## Execution

Follow these steps to run the it on the command line:

1. Change directory into the _scripts_ directory under the `xfind` project root

```bash
$ cd scripts
```

2. Build the `elixir` version using the common build script

```bash
$ ./build.sh ex
# - OR -
$ ./build.sh elixir
```

Once complete, a softlink to the `exfind` executable (script) is created under
_<project_root>/bin_. You can run the script from there or add the directory to your `PATH`.

3. Run `exfind` with the help switch to get usage information, the output should look similar to the following.

```bash
$ exfind -h

Usage:
 exfind [options] <path> [<path> ...]

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
 --maxdepth                Find files at most maxdepth levels below startpath
 --maxlastmod              Find files with lastmod less than or equal to maxlastmod
 --maxsize                 Find files with size <= maxsize
 --mindepth                Find files at least mindepth levels below startpath
 --minlastmod              Find files with lastmod greater than or equal to minlastmod
 --minsize                 Find files with size >= minsize
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

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/exfind>.
