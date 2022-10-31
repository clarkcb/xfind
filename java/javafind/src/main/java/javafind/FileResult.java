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
