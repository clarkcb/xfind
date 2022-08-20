package javafind;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class FileResult {
    public static final String CONTAINER_SEPARATOR = "!";
    private final List<String> containers;
    private final File file;
    private final FileType fileType;

    public FileResult(final File file, final FileType fileType) {
        this(new ArrayList<>(), file, fileType);
    }

    public FileResult(final List<String> containers, final File file,
                      final FileType fileType) {
        this.containers = containers;
        this.file = file;
        this.fileType = fileType;
    }

    public final List<String> getContainers() {
        return this.containers;
    }

    public final File getFile() {
        return this.file;
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
//        if (path != null && !path.isEmpty()) {
//            sb.append(path).append(File.separator);
//        }
//        sb.append(fileName);
        sb.append(file.getPath());
        return sb.toString();
    }

}
