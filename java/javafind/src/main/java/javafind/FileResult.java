package javafind;

import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.List;

public class FileResult {
    public static final String CONTAINER_SEPARATOR = "!";
    private final List<String> containers;
    private final Path path;
    private final FileType fileType;
    private final long fileSize;
    private final FileTime lastMod;

    public FileResult(final Path path, final FileType fileType) {
        this(new ArrayList<>(), path, fileType, 0L,null);
    }

    public FileResult(final Path path, final FileType fileType, final long fileSize, final FileTime lastMod) {
        this(new ArrayList<>(), path, fileType, fileSize, lastMod);
    }

    public FileResult(final List<String> containers, final Path path, final FileType fileType,
                      final long fileSize, final FileTime lastMod) {
        this.containers = containers;
        this.path = path;
        this.fileType = fileType;
        this.fileSize = fileSize;
        this.lastMod = lastMod;
    }

    public final List<String> getContainers() {
        return this.containers;
    }

    public final Path getPath() {
        return this.path;
    }

    public final FileType getFileType() {
        return this.fileType;
    }

    public final long getFileSize() {
        return this.fileSize;
    }

    public final FileTime getLastMod() {
        return this.lastMod;
    }

    public int compareByPath(final FileResult other, final boolean sortCaseInsensitive) {
        var p1 = this.path.getParent().toString();
        var p2 = other.path.getParent().toString();
        if (sortCaseInsensitive) {
            p1 = p1.toLowerCase();
            p2 = p2.toLowerCase();
        }
        if (p1.equals(p2)) {
            var f1 = this.path.getFileName().toString();
            var f2 = other.path.getFileName().toString();
            if (sortCaseInsensitive) {
                f1 = f1.toLowerCase();
                f2 = f2.toLowerCase();
            }
            return f1.compareTo(f2);
        }
        return p1.compareTo(p2);
    }

    public int compareByName(final FileResult other, final boolean sortCaseInsensitive) {
        var f1 = this.path.getFileName().toString();
        var f2 = other.path.getFileName().toString();
        if (sortCaseInsensitive) {
            f1 = f1.toLowerCase();
            f2 = f2.toLowerCase();
        }
        if (f1.equals(f2)) {
            var p1 = this.path.getParent().toString();
            var p2 = other.path.getParent().toString();
            if (sortCaseInsensitive) {
                p1 = p1.toLowerCase();
                p2 = p2.toLowerCase();
            }
            return p1.compareTo(p2);
        }
        return f1.compareTo(f2);
    }

    public int compareBySize(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.fileSize == other.fileSize) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return this.fileSize <= other.fileSize ? -1 : 1;
    }

    public int compareByType(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.getFileType().equals(other.getFileType())) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return this.getFileType().compareTo(other.getFileType());
    }

    public int compareByLastMod(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.lastMod.equals(other.lastMod)) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return this.lastMod.compareTo(other.lastMod);
    }

    public String toString() {
        var sb = new StringBuilder();
        if (!containers.isEmpty()) {
            for (int i = 0; i < containers.size(); i++) {
                if (i > 0) {
                    sb.append(CONTAINER_SEPARATOR);
                }
                sb.append(containers.get(i));
            }
            sb.append(CONTAINER_SEPARATOR);
        }
        sb.append(path.toString());
        return sb.toString();
    }
}
