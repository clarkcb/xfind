import os
from pathlib import Path
from typing import Generator, Iterator, Self


class FindPath(os.PathLike):
    __slots__ = ['_orig_path', '_exp_path']

    def __init__(self, *args):
        self._orig_path: Path
        self._exp_path: Path
        if args:
            arg = args[0]
            if isinstance(arg, FindPath):
                self._orig_path = arg._orig_path
                self._exp_path = arg._exp_path
            elif isinstance(arg, os.PathLike):
                self._orig_path = Path(arg)
                self._exp_path = self._orig_path.expanduser()
            elif isinstance(arg, str):
                self._orig_path = Path(arg)
                self._exp_path = self._orig_path.expanduser()
            else:
                raise TypeError(
                    f'FindPath: arg must be a FindPath, os.PathLike (e.g. pathlib.Path), or str, not {type(arg)}')
        else:
            self._orig_path = Path()
            self._exp_path = Path()

    def __str__(self):
        return str(self._orig_path)

    def __lt__(self, other):
        return self.expanded_path < other.expanded_path

    def __fspath__(self):
        return str(self._orig_path)

    def __rtruediv__(self, other):
        return FindPath(self._orig_path.__rtruediv__(other))

    def __truediv__(self, other):
        return FindPath(self._orig_path.__truediv__(other))

    @property
    def original_path(self):
        return self._orig_path

    @property
    def expanded_path(self):
        return self._exp_path

    @property
    def anchor(self):
        return self._exp_path.anchor

    @property
    def drive(self):
        return self._exp_path.drive

    @property
    def name(self):
        return self._exp_path.name

    @property
    def parent(self):
        return self._orig_path.parent

    @property
    def parents(self):
        return self._orig_path.parents

    @property
    def parts(self):
        return self._orig_path.parts

    @property
    def root(self):
        return self._orig_path.root

    @property
    def stem(self):
        return self._orig_path.stem

    @property
    def suffix(self):
        return self._orig_path.suffix

    @property
    def suffixes(self):
        return self._orig_path.suffixes

    def exists(self) -> bool:
        return self._exp_path.exists()

    def expanduser(self) -> Self:
        return FindPath(str(self._exp_path))

    def glob(self, pattern) -> Generator:
        return self._exp_path.glob(pattern)

    def group(self) -> str:
        return self._exp_path.group()

    def is_dir(self) -> bool:
        return self._exp_path.is_dir()

    def is_file(self) -> bool:
        return self._exp_path.is_file()

    def is_junction(self) -> bool:
        return self._exp_path.is_junction()

    def is_mount(self) -> bool:
        return self._exp_path.is_mount()

    def is_symlink(self) -> bool:
        return self._exp_path.is_symlink()

    def is_socket(self) -> bool:
        return self._exp_path.is_socket()

    def is_fifo(self) -> bool:
        return self._exp_path.is_fifo()

    def is_block_device(self) -> bool:
        return self._exp_path.is_block_device()

    def is_char_device(self) -> bool:
        return self._exp_path.is_char_device()

    def iterdir(self) -> Generator:
        for p in self._exp_path.iterdir():
            yield FindPath(self._orig_path / str(p.name))

    def joinpath(self, *paths: str | os.PathLike[str]):
        return self._orig_path.joinpath(*paths)

    def read_text(self, encoding: str | None = None, errors: str | None = None) -> str:
        return self._exp_path.read_text(encoding=encoding, errors=errors)

    def stat(self, follow_symlinks=True):
        return self._exp_path.stat(follow_symlinks=follow_symlinks)

    def walk(self, top_down: bool = True,
             on_error: OSError = None,
             follow_symlinks: bool = False) -> Iterator[tuple[Self, list[str], list[str]]]:
        for (p, dirs, files) in self._exp_path.walk(top_down=top_down, on_error=on_error, follow_symlinks=follow_symlinks):
            yield FindPath(p), dirs, files
