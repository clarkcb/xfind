package javafind;

import java.nio.file.Path;
import java.util.function.Function;
import java.util.regex.Matcher;

public class FileResultFormatter {
    private final FindSettings settings;
    private final Function<Path, String> formatDirPathFunc;
    private final Function<String, String> formatFileNameFunc;

    public FileResultFormatter(final FindSettings settings) {
        this.settings = settings;
        if (settings.getColorize() && !settings.getInDirPatterns().isEmpty()) {
            formatDirPathFunc = this::formatDirPathWithColor;
        } else {
            formatDirPathFunc = Path::toString;
        }
        if (settings.getColorize() && (!settings.getInExtensions().isEmpty() || !settings.getInFilePatterns().isEmpty())) {
            formatFileNameFunc = this::formatFileNameWithColor;
        } else {
            formatFileNameFunc = String::toString;
        }
    }

    public String colorize(final String s, final int matchStartIndex, final int matchEndIndex) {
        var prefix = "";
        if (matchStartIndex > 0) {
            prefix = s.substring(0, matchStartIndex);
        }
        var suffix = "";
        if (matchEndIndex < s.length()) {
            suffix = s.substring(matchEndIndex);
        }
        return prefix +
                Color.GREEN.getValue() +
                s.substring(matchStartIndex, matchEndIndex) +
                Color.RESET.getValue() +
                suffix;
    }

    private String formatDirPathWithColor(final Path dirPath) {
        var formattedDirPath = dirPath.toString();
        for (var p : settings.getInDirPatterns()) {
            Matcher m = p.matcher(formattedDirPath);
            if (m.find()) {
                formattedDirPath = colorize(formattedDirPath, m.start(), m.end());
                break;
            }
        }
        return formattedDirPath;
    }

    public String formatDirPath(final Path dirPath) {
        return formatDirPathFunc.apply(dirPath);
    }

    private String formatFileNameWithColor(final String fileName) {
        var formattedFileName = fileName;
        for (var p : settings.getInFilePatterns()) {
            Matcher m = p.matcher(formattedFileName);
            if (m.find()) {
                formattedFileName = colorize(formattedFileName, m.start(), m.end());
                break;
            }
        }
        if (!settings.getInExtensions().isEmpty()) {
            var idx = formattedFileName.lastIndexOf('.');
            if (idx > 0 && idx < formattedFileName.length() - 1) {
                formattedFileName = colorize(formattedFileName, idx + 1, formattedFileName.length());
            }
        }
        return formattedFileName;
    }

    public String formatFileName(final String fileName) {
        return formatFileNameFunc.apply(fileName);
    }

    public final String formatPath(final Path path) {
        var parent = ".";
        if (path.getParent() != null) {
            parent = this.formatDirPath(path.getParent());
        }
        var fileName = formatFileName(path.getFileName().toString());
        return Path.of(parent, fileName).toString();
    }

    public final String formatFileResult(final FileResult result) {
        return formatPath(result.path());
    }
}
