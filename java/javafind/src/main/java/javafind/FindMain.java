/*******************************************************************************
FindMain

Main class for initiating javafind from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import org.json.simple.parser.ParseException;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static javafind.Logger.log;

public class FindMain {

    private static void handleError(final String message) {
        log("");
        Logger.logError(message);
    }

    private static void handleError(final String message, final FindOptions options) {
        log("");
        Logger.logError(message + "\n");
        options.usage(1);
    }

    private static List<String> getMatchingDirs(final List<FileResult> fileResults) {
        return fileResults.stream()
                .map(fr -> fr.getPath().getParent().toString()).distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingDirs(final List<FileResult> fileResults) {
        var dirs = getMatchingDirs(fileResults);
        if (!dirs.isEmpty()) {
            log(String.format("\nMatching directories (%d):", dirs.size()));
            for (String d : dirs) {
                log(d);
            }
        } else {
            log("\nMatching directories: 0");
        }
    }

    private static List<String> getMatchingFiles(final List<FileResult> fileResults) {
        return fileResults.stream().map(FileResult::toString).collect(Collectors.toList());
    }

    private static void printMatchingFiles(final List<FileResult> fileResults) {
        var files = getMatchingFiles(fileResults);
        if (!files.isEmpty()) {
            log(String.format("\nMatching files (%d):", files.size()));
            for (String f : files) {
                log(f);
            }
        } else {
            log("\nMatching files: 0");
        }
    }

    public static void main(final String[] args) {

        try {
            FindOptions options = new FindOptions();

            try {
                FindSettings settings = options.settingsFromArgs(args);

                if (settings.getDebug()) {
                    log("\nsettings:");
                    log(settings.toString() + "\n");
                }

                if (settings.getPrintUsage()) {
                    log("");
                    options.usage(0);
                }

                Finder finder = new Finder(settings);

                finder.validateSettings();
                List<FileResult> fileResults = finder.find();

                if (settings.getListDirs()) {
                    printMatchingDirs(fileResults);
                }
                if (settings.getListFiles()) {
                    printMatchingFiles(fileResults);
                }

            } catch (FindException e) {
                handleError(e.getMessage(), options);
            }

        } catch (ParseException | IOException e) {
            handleError(e.getMessage());
        }
    }
}
