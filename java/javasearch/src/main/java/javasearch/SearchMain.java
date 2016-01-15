/*******************************************************************************
SearchMain

Main class for initiating javasearch from command line

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;

public class SearchMain {

    private static void handleError(final String message) {
        Logger.log("");
        Logger.logError(message);
    }

    private static void handleError(final String message, SearchOptions options) {
        Logger.log("");
        Logger.logError(message + "\n");
        options.usage(1);
    }

    public static void main(final String[] args) {

        try {
            SearchOptions options = new SearchOptions();

            try {
                SearchSettings settings = options.settingsFromArgs(args);

                if (settings.getDebug()) {
                    Logger.log("\nsettings:");
                    Logger.log(settings.toString() + "\n");
                }

                if (settings.getPrintUsage()) {
                    Logger.log("");
                    options.usage(0);
                }

                Searcher searcher = new Searcher(settings);

                searcher.validateSettings();
                searcher.search();

                // print the results
                if (settings.getPrintResults()) {
                    Logger.log("");
                    searcher.printSearchResults();
                }
                if (settings.getListDirs()) {
                    searcher.printMatchingDirs();
                }
                if (settings.getListFiles()) {
                    searcher.printMatchingFiles();
                }
                if (settings.getListLines()) {
                    searcher.printMatchingLines();
                }

            } catch (SearchException e) {
                handleError(e.getMessage(), options);
            }

        } catch (ParserConfigurationException | SAXException | IOException e) {
            handleError(e.getMessage());
        }
    }
}
