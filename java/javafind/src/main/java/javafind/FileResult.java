package javafind;

import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

public class FileResult {
    public static final String CONTAINER_SEPARATOR = "!";
    private final List<String> containers;
    private final Path path;
    private final FileType fileType;
    private final BasicFileAttributes stat;

    public FileResult(final Path path, final FileType fileType) {
        this(new ArrayList<>(), path, fileType, null);
    }

    public FileResult(final Path path, final FileType fileType, final BasicFileAttributes stat) {
        this(new ArrayList<>(), path, fileType, stat);
    }

    public FileResult(final List<String> containers, final Path path, final FileType fileType,
                      final BasicFileAttributes stat) {
        this.containers = containers;
        this.path = path;
        this.fileType = fileType;
        this.stat = stat;
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

    public BasicFileAttributes getStat() {
        return stat;
    }

    public int compareByPath(final FileResult other, final boolean sortCaseInsensitive) {
        String p1 = this.path.getParent().toString();
        String p2 = other.path.getParent().toString();
        if (sortCaseInsensitive) {
            p1 = p1.toLowerCase();
            p2 = p2.toLowerCase();
        }
        if (p1.equals(p2)) {
            String f1 = this.path.getFileName().toString();
            String f2 = other.path.getFileName().toString();
            if (sortCaseInsensitive) {
                f1 = f1.toLowerCase();
                f2 = f2.toLowerCase();
            }
            return f1.compareTo(f2);
        }
        return p1.compareTo(p2);
    }

    public int compareByName(final FileResult other, final boolean sortCaseInsensitive) {
        String f1 = this.path.getFileName().toString();
        String f2 = other.path.getFileName().toString();
        if (sortCaseInsensitive) {
            f1 = f1.toLowerCase();
            f2 = f2.toLowerCase();
        }
        if (f1.equals(f2)) {
            String p1 = this.path.getParent().toString();
            String p2 = other.path.getParent().toString();
            if (sortCaseInsensitive) {
                p1 = p1.toLowerCase();
                p2 = p2.toLowerCase();
            }
            return p1.compareTo(p2);
        }
        return f1.compareTo(f2);
    }

    public int compareBySize(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.stat != null && other.stat != null) {
            if (this.stat.size() == other.stat.size()) {
                return compareByPath(other, sortCaseInsensitive);
            }
            return this.stat.size() <= other.stat.size() ? -1 : 1;
        }
        return 0;
    }

    public int compareByType(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.getFileType().equals(other.getFileType())) {
            return compareByPath(other, sortCaseInsensitive);
        }
        return this.getFileType().compareTo(other.getFileType());
    }

    public int compareByLastMod(final FileResult other, final boolean sortCaseInsensitive) {
        if (this.stat != null && other.stat != null) {
            if (this.stat.lastModifiedTime().equals(other.stat.lastModifiedTime())) {
                return compareByPath(other, sortCaseInsensitive);
            }
            return this.stat.lastModifiedTime().compareTo(other.stat.lastModifiedTime());
        }
        return 0;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (containers.size() > 0) {
            for (int i = 0; i < containers.size(); i++) {
                if (i > 0) {
                    sb.append(CONTAINER_SEPARATOR);
                }
                sb.append(containers.get(i));
            }
            sb.append(CONTAINER_SEPARATOR);
        }
        sb.append(path.toString());
//        if (stat != null) {
//            sb.append(" (").append(stat.lastModifiedTime().toInstant().getEpochSecond()).append(")");
//        }
        return sb.toString();
    }
}
