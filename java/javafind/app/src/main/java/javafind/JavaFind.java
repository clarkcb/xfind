/*******************************************************************************
JavaFind

Main class for initiating javafind from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import java.io.IOException;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

import static javafind.Logger.log;
import static javafind.Logger.logError;

public class JavaFind {

    private static void handleError(final String message) {
        log("");
        logError(message);
    }

    private static void handleError(final String message, final FindOptions options) {
        log("");
        logError(message + "\n");
        options.usage(1);
    }

    private static List<Path> getMatchingDirs(final List<FileResult> results) {
        return results.stream()
                .map(fr -> fr.path().getParent()).distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingDirs(final List<FileResult> results, final FileResultFormatter formatter) {
        var dirs = getMatchingDirs(results);
        if (!dirs.isEmpty()) {
            log(String.format("\nMatching directories (%d):", dirs.size()));
            for (var d : dirs) {
                log(formatter.formatDirPath(d));
            }
        } else {
            log("\nMatching directories: 0");
        }
    }

    private static void printMatchingFiles(final List<FileResult> results, final FileResultFormatter formatter) {
        if (!results.isEmpty()) {
            log(String.format("\nMatching files (%d):", results.size()));
            for (var f : results) {
                log(formatter.formatFileResult(f));
            }
        } else {
            log("\nMatching files: 0");
        }
    }

    public static void main(final String[] args) {
        try {
            var options = new FindOptions();

            try {
                var settings = options.settingsFromArgs(args);

                if (settings.getDebug()) {
                    log("\nsettings: " + settings + "\n");
                }

                if (settings.getPrintUsage()) {
                    log("");
                    options.usage(0);
                }

                var finder = new Finder(settings);
                finder.validateSettings();
                var fileResults = finder.find();
                var formatter = new FileResultFormatter(settings);

                if (settings.getPrintDirs()) {
                    printMatchingDirs(fileResults, formatter);
                }
                if (settings.getPrintFiles()) {
                    printMatchingFiles(fileResults, formatter);
                }

            } catch (FindException e) {
                handleError(e.getMessage(), options);
            }

        } catch (IOException e) {
            handleError(e.getMessage());
        }
    }
}
