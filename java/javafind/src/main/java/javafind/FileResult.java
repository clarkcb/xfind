package javafind;

import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.List;

public record FileResult(List<Path> containers, Path path, FileType fileType, long fileSize, FileTime lastMod) {
    public static final String CONTAINER_SEPARATOR = "!";

    public FileResult(final Path path, final FileType fileType) {
        this(new ArrayList<>(), path, fileType, 0L,null);
    }

    public FileResult(final Path path, final FileType fileType, final long fileSize, final FileTime lastMod) {
        this(new ArrayList<>(), path, fileType, fileSize, lastMod);
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
        var fComp = compareStrings(this.path.getFileName().toString(), other.path.getFileName().toString(), sortCaseInsensitive);
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
        var ftComp = this.fileType().compareTo(other.fileType());
        if (ftComp == 0) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return ftComp;
    }

    public int compareByLastMod(final FileResult other, final boolean sortCaseInsensitive) {
        if (lastMod == null && other.lastMod == null) return 0;
        if (lastMod == null) return 1;
        if (other.lastMod == null) return -1;
        var lmComp = this.lastMod.compareTo(other.lastMod);
        if (lmComp == 0) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return lmComp;
    }

    public String toString() {
        var sb = new StringBuilder();
        if (!containers.isEmpty()) {
            for (var i = 0; i < containers.size(); i++) {
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
