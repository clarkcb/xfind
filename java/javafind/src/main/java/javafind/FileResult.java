package javafind;

import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.List;

public class FileResult {
    public static final String CONTAINER_SEPARATOR = "!";
    private final List<Path> containers;
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

    public FileResult(final List<Path> containers, final Path path, final FileType fileType,
                      final long fileSize, final FileTime lastMod) {
        this.containers = containers;
        this.path = path;
        this.fileType = fileType;
        this.fileSize = fileSize;
        this.lastMod = lastMod;
    }

    public final List<Path> getContainers() {
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

    private static int compareStrings(final String s1, final String s2, final boolean sortCaseInsensitive) {
        if (sortCaseInsensitive) {
            return s1.toLowerCase().compareTo(s2.toLowerCase());
        }
        return s1.compareTo(s2);
    }

    public int compareByPath(final FileResult other, final boolean sortCaseInsensitive) {
        var pComp = compareStrings(this.path.getParent().toString(), other.path.getParent().toString(), sortCaseInsensitive);
        if (pComp == 0) {
            return compareStrings(this.path.getFileName().toString(), other.path.getFileName().toString(), sortCaseInsensitive);
        }
        return pComp;
    }

    public int compareByName(final FileResult other, final boolean sortCaseInsensitive) {
        int fComp = compareStrings(this.path.getFileName().toString(), other.path.getFileName().toString(), sortCaseInsensitive);
        if (fComp == 0) {
            return compareStrings(this.path.getParent().toString(), other.path.getParent().toString(), sortCaseInsensitive);
        }
        return fComp;
    }

    public int compareBySize(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.fileSize == other.fileSize) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return this.fileSize < other.fileSize ? -1 : 1;
    }

    public int compareByType(final FileResult other, final boolean sortCaseInsensitive) {
        int ftComp = this.getFileType().compareTo(other.getFileType());
        if (ftComp == 0) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return ftComp;
    }

    public int compareByLastMod(final FileResult other, final boolean sortCaseInsensitive) {
        if (lastMod == null && other.lastMod == null) return 0;
        if (lastMod == null) return 1;
        if (other.lastMod == null) return -1;
        int lmComp = this.lastMod.compareTo(other.lastMod);
        if (lmComp == 0) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return lmComp;
    }

    public String toString() {
        var sb = new StringBuilder();
        if (!containers.isEmpty()) {
            for (int i = 0; i < containers.size(); i++) {
                if (i > 0) {
                    sb.append(CONTAINER_SEPARATOR);
                }
                sb.append(containers.get(i).toString());
            }
            sb.append(CONTAINER_SEPARATOR);
        }
        sb.append(path.toString());
        return sb.toString();
    }
}
