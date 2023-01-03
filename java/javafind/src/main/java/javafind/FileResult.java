package javafind;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class FileResult {
    public static final String CONTAINER_SEPARATOR = "!";
    private final List<String> containers;
    private final Path path;
    private final FileType fileType;

    public FileResult(final Path path, final FileType fileType) {
        this(new ArrayList<>(), path, fileType);
    }

    public FileResult(final List<String> containers, final Path path,
                      final FileType fileType) {
        this.containers = containers;
        this.path = path;
        this.fileType = fileType;
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

    public final int compareByPath(FileResult other) {
        if (this.path.getParent().equals(other.path.getParent())) {
            return this.path.getFileName().compareTo(other.path.getFileName());
        }
        return this.path.getParent().compareTo(other.path.getParent());
    }

    public final int compareByName(FileResult other) {
        if (this.path.getFileName().equals(other.path.getFileName())) {
            return this.path.getParent().compareTo(other.path.getParent());
        }
        return this.path.getFileName().compareTo(other.path.getFileName());
    }

    public final int compareByType(FileResult other) {
        if (this.getFileType().equals(other.getFileType())) {
            return compareByPath(other);
        }
        return this.getFileType().compareTo(other.getFileType());
    }

    public final String toString() {
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
        return sb.toString();
    }

}
