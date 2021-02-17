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

    private static void handleError(final String message, FindOptions options) {
        log("");
        Logger.logError(message + "\n");
        options.usage(1);
    }

    private static int signum(final int num) {
        return Integer.compare(num, 0);
    }

    private static int compareResults(final FindResult r1, final FindResult r2) {
        int pathCmp = r1.getFindFile().getPath().toLowerCase()
                .compareTo(r2.getFindFile().getPath().toLowerCase());
        if (pathCmp == 0) {
            int fileCmp = r1.getFindFile().getFileName().toLowerCase()
                    .compareTo(r2.getFindFile().getFileName().toLowerCase());
            if (fileCmp == 0) {
                int lineNumCmp = signum(r1.getLineNum() - r2.getLineNum());
                if (lineNumCmp == 0) {
                    return signum(r1.getMatchStartIndex() - r2.getMatchStartIndex());
                }
                return lineNumCmp;
            }
            return fileCmp;
        }
        return pathCmp;
    }

    private static List<FindResult> getSortedFindResults(List<FindResult> results) {
        return results.stream().sorted(FindMain::compareResults)
                .collect(Collectors.toList());
    }

    private static void printFindResults(List<FindResult> results, FindSettings settings) {
        List<FindResult> sortedResults = getSortedFindResults(results);
        FindResultFormatter formatter = new FindResultFormatter(settings);
        log(String.format("Find results (%d):", sortedResults.size()));
        for (FindResult r : sortedResults) {
            log(formatter.format(r));
        }
    }

    private static List<String> getMatchingDirs(List<FindResult> results) {
        return results.stream().map(r -> r.getFindFile().getPath()).distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingDirs(List<FindResult> results) {
        List<String> dirs = getMatchingDirs(results);
        log(String.format("\nDirectories with matches (%d):", dirs.size()));
        for (String d : dirs) {
            log(d);
        }
    }

    private static List<String> getMatchingFiles(List<FindResult> results) {
        return results.stream().map(r -> r.getFindFile().toString()).distinct()
                .sorted().collect(Collectors.toList());
    }

    private static void printMatchingFiles(List<FindResult> results) {
        List<String> files = getMatchingFiles(results);
        log(String.format("\nFiles with matches (%d):", files.size()));
        for (String f : files) {
            log(f);
        }
    }

    private static List<String> getMatchingLines(List<FindResult> results, FindSettings settings) {
        List<String> lines = new ArrayList<>();
        for (FindResult r : results) {
            lines.add(r.getLine().trim());
        }
        if (settings.getUniqueLines()) {
            Set<String> lineSet = new HashSet<>(lines);
            lines = new ArrayList<>(lineSet);
        }
        lines.sort(Comparator.comparing(String::toUpperCase));
        return lines;
    }

    private static void printMatchingLines(List<FindResult> results, FindSettings settings) {
        List<String> lines = getMatchingLines(results, settings);
        String hdr;
        if (settings.getUniqueLines()) {
            hdr = "\nUnique lines with matches (%d):";
        } else {
            hdr = "\nLines with matches (%d):";
        }
        log(String.format(hdr, lines.size()));
        for (String line : lines) {
            log(line);
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
                List<FindResult> results = finder.find();

                // print the results
                if (settings.getPrintResults()) {
                    log("");
                    printFindResults(results, settings);
                }
                if (settings.getListDirs()) {
                    printMatchingDirs(results);
                }
                if (settings.getListFiles()) {
                    printMatchingFiles(results);
                }
                if (settings.getListLines()) {
                    printMatchingLines(results, settings);
                }

            } catch (FindException e) {
                handleError(e.getMessage(), options);
            }

        } catch (ParseException | IOException e) {
            handleError(e.getMessage());
        }
    }
}
