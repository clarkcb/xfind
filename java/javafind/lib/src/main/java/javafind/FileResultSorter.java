package javafind;

import java.util.Comparator;
import java.util.List;

public class FileResultSorter {
    private final FindSettings settings;

    public FileResultSorter(final FindSettings settings) {
        this.settings = settings;
    }

    public FindSettings getSettings() {
        return settings;
    }

    public final Comparator<FileResult> getFileResultComparator() {
        var sortBy = settings.getSortBy() == null ? SortBy.FILEPATH : settings.getSortBy();
        if (settings.getSortDescending()) {
            return switch (sortBy) {
                case FILENAME -> (FileResult fr1, FileResult fr2) -> fr2.compareByName(fr1, settings.getSortCaseInsensitive());
                case FILESIZE -> (FileResult fr1, FileResult fr2) -> fr2.compareBySize(fr1, settings.getSortCaseInsensitive());
                case FILETYPE -> (FileResult fr1, FileResult fr2) -> fr2.compareByType(fr1, settings.getSortCaseInsensitive());
                case LASTMOD -> (FileResult fr1, FileResult fr2) -> fr2.compareByLastMod(fr1, settings.getSortCaseInsensitive());
                default -> (FileResult fr1, FileResult fr2) -> fr2.compareByPath(fr1, settings.getSortCaseInsensitive());
            };
        }
        return switch (sortBy) {
            case FILENAME -> (FileResult fr1, FileResult fr2) -> fr1.compareByName(fr2, settings.getSortCaseInsensitive());
            case FILESIZE -> (FileResult fr1, FileResult fr2) -> fr1.compareBySize(fr2, settings.getSortCaseInsensitive());
            case FILETYPE -> (FileResult fr1, FileResult fr2) -> fr1.compareByType(fr2, settings.getSortCaseInsensitive());
            case LASTMOD -> (FileResult fr1, FileResult fr2) -> fr1.compareByLastMod(fr2, settings.getSortCaseInsensitive());
            default -> (FileResult fr1, FileResult fr2) -> fr1.compareByPath(fr2, settings.getSortCaseInsensitive());
        };
    }

    public final void sort(List<FileResult> fileResults) {
        if (fileResults.isEmpty()) {
            return;
        }
        var fileResultComparator = getFileResultComparator();
        fileResults.sort(fileResultComparator);
    }
}
