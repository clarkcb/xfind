import pathlib


class FindPath(type(pathlib.Path())):
    __slots__ = ['orig_path']

    def __new__(cls, *args):
        expanded = str(pathlib.Path(args[0]).expanduser())
        self = super(FindPath, cls).__new__(cls, expanded, orig_path=args[0])
        self.orig_path = args[0]
        return self

    def __init__(self, *args, **kwargs):
        # subcls = type(pathlib.Path())
        # subcls.__init__(self)
        self.orig_path = args[0]

    # @property
    # def orig_path(self):
    #     """Get relative path of FileResult (does not include any containers)"""
    #     return self._orig_path

    # def __str__(self):
    #     return self._origpath

    # def exists(self):
    #     return self.expanduser().exists()
    #
    # def is_dir(self):
    #     return self.expanduser().is_dir()
    #
    # def is_file(self):
    #     return self.expanduser().is_file()
    #
    # def is_symlink(self):
    #     return self.expanduser().is_symlink()
    #
    # def open(self, mode='r', buffering=-1, encoding=None, errors=None, newline=None):
    #     if encoding is None and 'b' not in mode:
    #         encoding = 'utf-8'
    #     return super().open(mode, buffering, encoding, errors, newline)
    #
    # def stat(self, follow_symlinks=True):
    #     return self.expanduser().stat(follow_symlinks)
