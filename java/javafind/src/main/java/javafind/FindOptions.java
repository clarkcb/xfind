/*******************************************************************************
FindOptions

Class to encapsulate all command line find options

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class FindOptions {
    private static final String FIND_OPTIONS_JSON_PATH = "/findoptions.json";
    private final List<FindOption> options;
    private final Map<String, String> longArgMap = new HashMap<>();


    public FindOptions() throws IOException {
        options = new ArrayList<>();
        setOptionsFromJson();
    }

    @FunctionalInterface
    private interface BooleanSetter {
        void set(Boolean b, FindSettings settings);
    }

    private final int boolActionMapSize = 18;
    private final Map<String, BooleanSetter> boolActionMap = new HashMap<>(boolActionMapSize) {
        {
            put("archivesonly", (b, settings) -> settings.setArchivesOnly(b));
            put("debug", (b, settings) -> settings.setDebug(b));
            put("excludehidden", (b, settings) -> settings.setIncludeHidden(!b));
            put("followsymlinks", (b, settings) -> settings.setFollowSymlinks(b));
            put("help", (b, settings) -> settings.setPrintUsage(b));
            put("includearchives", (b, settings) -> settings.setIncludeArchives(b));
            put("includehidden", (b, settings) -> settings.setIncludeHidden(b));
            put("nofollowsymlinks", (b, settings) -> settings.setFollowSymlinks(!b));
            put("noprintdirs", (b, settings) -> settings.setPrintDirs(!b));
            put("noprintfiles", (b, settings) -> settings.setPrintFiles(!b));
            put("norecursive", (b, settings) -> settings.setRecursive(!b));
            put("printdirs", (b, settings) -> settings.setPrintDirs(b));
            put("printfiles", (b, settings) -> settings.setPrintFiles(b));
            put("recursive", (b, settings) -> settings.setRecursive(b));
            put("sort-ascending", (b, settings) -> settings.setSortDescending(!b));
            put("sort-caseinsensitive", (b, settings) -> settings.setSortCaseInsensitive(b));
            put("sort-casesensitive", (b, settings) -> settings.setSortCaseInsensitive(!b));
            put("sort-descending", (b, settings) -> settings.setSortDescending(b));
            put("verbose", (b, settings) -> settings.setVerbose(b));
            put("version", (b, settings) -> settings.setPrintVersion(b));
        }
    };

    @FunctionalInterface
    private interface StringSetter {
        void set(String s, FindSettings settings);
    }

    private final int stringActionMapSize = 17;
    private final Map<String, StringSetter> stringActionMap = new HashMap<>(stringActionMapSize) {
        {
            put("in-archiveext", (s, settings) -> settings.addInArchiveExtension(s));
            put("in-archivefilepattern", (s, settings) -> settings.addInArchiveFilePattern(s));
            put("in-dirpattern", (s, settings) -> settings.addInDirPattern(s));
            put("in-ext", (s, settings) -> settings.addInExtension(s));
            put("in-filepattern", (s, settings) -> settings.addInFilePattern(s));
            put("in-filetype", (s, settings) -> settings.addInFileType(s));
            put("maxlastmod", (s, settings) -> settings.setMaxLastMod(s));
            put("minlastmod", (s, settings) -> settings.setMinLastMod(s));
            put("out-archiveext", (s, settings) -> settings.addOutArchiveExtension(s));
            put("out-archivefilepattern", (s, settings) -> settings.addOutArchiveFilePattern(s));
            put("out-dirpattern", (s, settings) -> settings.addOutDirPattern(s));
            put("out-ext", (s, settings) -> settings.addOutExtension(s));
            put("out-filepattern", (s, settings) -> settings.addOutFilePattern(s));
            put("out-filetype", (s, settings) -> settings.addOutFileType(s));
            put("path", (s, settings) -> settings.addPath(s));
            // put("settings-file", (s, settings) -> settingsFromFilePath(s, settings));
            put("sort-by", (s, settings) -> settings.setSortBy(SortBy.forName(s)));
        }
    };

    @FunctionalInterface
    private interface IntegerSetter {
        void set(Integer i, FindSettings settings);
    }

    private final int intActionMapSize = 2;
    private final Map<String, IntegerSetter> intActionMap = new HashMap<>(intActionMapSize) {
        {
            put("maxdepth", (i, settings) -> settings.setMaxDepth(i));
            put("mindepth", (i, settings) -> settings.setMinDepth(i));
        }
    };

    @FunctionalInterface
    private interface LongSetter {
        void set(Long l, FindSettings settings);
    }

    private final int longActionMapSize = 2;
    private final Map<String, LongSetter> longActionMap = new HashMap<>(longActionMapSize) {
        {
            put("maxsize", (l, settings) -> settings.setMaxSize(l));
            put("minsize", (l, settings) -> settings.setMinSize(l));
        }
    };

    private void setOptionsFromJson() {
        var findOptionsInputStream = getClass().getResourceAsStream(FIND_OPTIONS_JSON_PATH);
        assert findOptionsInputStream != null;
        var jsonObj = new JSONObject(new JSONTokener(findOptionsInputStream));
        var findOptionsArray = jsonObj.getJSONArray("findoptions");

        for (int i=0; i<findOptionsArray.length(); i++) {
            var findOptionObj = findOptionsArray.getJSONObject(i);
            var longArg = findOptionObj.getString("long");
            longArgMap.put(longArg, longArg);
            var desc = findOptionObj.getString("desc");
            var shortArg = "";
            if (findOptionObj.has("short")) {
                shortArg = findOptionObj.getString("short");
                longArgMap.put(shortArg, longArg);
            }
            options.add(new FindOption(shortArg, longArg, desc));
        }
    }

    private void settingsFromFilePath(final String filePath, FindSettings settings) throws FindException {
        var path = Paths.get(filePath);
        try {
            if (!Files.exists(path)) {
                throw new FindException("Settings file not found: " + filePath);
            }
            if (!FileUtil.hasExtension(filePath, "json")) {
                throw new FindException("Invalid settings file type (must be JSON): " + filePath);
            }
            settingsFromJson(FileUtil.getFileContents(path), settings);
        } catch (FileNotFoundException e) {
            throw new FindException("Settings file not found: " + filePath);
        } catch (IOException e) {
            throw new FindException("IOException reading settings file: " + filePath);
        }
    }

    public void settingsFromJson(final String json, FindSettings settings) {
        var jsonObj = new JSONObject(new JSONTokener(json));
        for (var ko : jsonObj.keySet()) {
            var vo = jsonObj.get(ko);
            applySetting(ko, vo, settings);
        }
    }

    private void applySetting(final String arg, final Object obj, FindSettings settings) {
        if (obj instanceof Boolean) {
            try {
                applySetting(arg, (Boolean)obj, settings);
            } catch (FindException e) {
                Logger.logError("FindException: " + e.getMessage());
            }
        } else if (obj instanceof String) {
            try {
                applySetting(arg, (String)obj, settings);
            } catch (FindException e) {
                Logger.logError("FindException: " + e.getMessage());
            }
        } else if (obj instanceof Integer) {
            if (intActionMap.containsKey(arg)) {
                try {
                    applySetting(arg, (Integer)obj, settings);
                } catch (FindException e) {
                    Logger.logError("FindException: " + e.getMessage());
                }
            } else if (longActionMap.containsKey(arg)) {
                try {
                    applySetting(arg, ((Integer) obj).longValue(), settings);
                } catch (FindException e) {
                    Logger.logError("FindException: " + e.getMessage());
                }
            } else {
                Logger.logError("Invalid option: " + arg);
            }
        } else if (obj instanceof Long) {
            if (intActionMap.containsKey(arg)) {
                try {
                    applySetting(arg, ((Long) obj).intValue(), settings);
                } catch (FindException e) {
                    Logger.logError("FindException: " + e.getMessage());
                }
            } else if (longActionMap.containsKey(arg)) {
                try {
                    applySetting(arg, (Long)obj, settings);
                } catch (FindException e) {
                    Logger.logError("FindException: " + e.getMessage());
                }
            } else {
                Logger.logError("Invalid option: " + arg);
            }
        } else if (obj instanceof JSONArray) {
            for (int i=0; i < ((JSONArray)obj).length(); i++) {
                applySetting(arg, ((JSONArray)obj).get(i), settings);
            }
        } else {
            Logger.log("obj is another class type");
        }
    }

    private void applySetting(final String arg, final Boolean val, FindSettings settings)
            throws FindException{
        if (this.boolActionMap.containsKey(arg)) {
            this.boolActionMap.get(arg).set(val, settings);
        } else {
            throw new FindException("Invalid option: " + arg);
        }
    }

    private void applySetting(final String arg, final String val, FindSettings settings)
            throws FindException {
        if (this.stringActionMap.containsKey(arg)) {
            this.stringActionMap.get(arg).set(val, settings);
        } else if (arg.equals("path")) {
            settings.addPath(val);
        } else {
            throw new FindException("Invalid option: " + arg);
        }
    }

    private void applySetting(final String arg, final Integer val, FindSettings settings)
            throws FindException{
        if (this.intActionMap.containsKey(arg)) {
            this.intActionMap.get(arg).set(val, settings);
        } else {
            throw new FindException("Invalid option: " + arg);
        }
    }

    private void applySetting(final String arg, final Long val, FindSettings settings)
            throws FindException{
        if (this.longActionMap.containsKey(arg)) {
            this.longActionMap.get(arg).set(val, settings);
        } else {
            throw new FindException("Invalid option: " + arg);
        }
    }

    public final FindSettings settingsFromArgs(final String[] args) throws FindException {
        var settings = new FindSettings();
        // default printFiles to true since running from command line
        settings.setPrintFiles(true);

        Queue<String> queue = new LinkedList<>(Arrays.asList(args));
        while (!queue.isEmpty()) {
            var arg = queue.remove();
            if (arg.startsWith("-")) {
                while (arg.startsWith("-")) {
                    arg = arg.substring(1);
                }
                var longArg = longArgMap.get(arg);
                if (this.boolActionMap.containsKey(longArg)) {
                    this.boolActionMap.get(longArg).set(true, settings);
                } else if (this.stringActionMap.containsKey(longArg)
                        || this.intActionMap.containsKey(longArg)
                        || this.longActionMap.containsKey(longArg)
                        || longArg.equals("settings-file")) {
                    if (!queue.isEmpty()) {
                        String argVal = queue.remove();
                        if (this.stringActionMap.containsKey(longArg)) {
                            this.stringActionMap.get(longArg).set(argVal, settings);
                        } else if (this.intActionMap.containsKey(longArg)) {
                            this.intActionMap.get(longArg).set(Integer.parseInt(argVal), settings);
                        } else if (this.longActionMap.containsKey(longArg)) {
                            this.longActionMap.get(longArg).set(Long.parseLong(argVal), settings);
                        } else if (longArg.equals("settings-file")) {
                            settingsFromFilePath(argVal, settings);
                        }
                    } else {
                        throw new FindException("Missing value for option " + arg);
                    }
                } else {
                    throw new FindException("Invalid option: " + arg);
                }
            } else {
                settings.addPath(arg);
            }
        }
        return settings;
    }

    public final void usage(final int exitStatus) {
        System.out.println(this.getUsageString());
        System.exit(exitStatus);
    }

    public final String getUsageString() {
        var sb = new StringBuilder();
        sb.append("Usage:\n");
        sb.append(" javafind [options] <path> [<path> ...]\n\n");
        sb.append("Options:\n");

        this.options.sort(Comparator.comparing(FindOption::getSortArg));

        var optStrings = new ArrayList<>();
        var optDescs = new ArrayList<>();
        int longest = 0;
        for (var opt : this.options) {
            var optString = new StringBuilder();
            var shortArg = opt.getShortArg();
            if (null != shortArg && !shortArg.isEmpty()) {
                optString.append("-").append(shortArg).append(",");
            }
            optString.append("--").append(opt.getLongArg());
            if (optString.length() > longest) {
                longest = optString.length();
            }
            optStrings.add(optString.toString());
            optDescs.add(opt.getDescription());
        }
        final var format = " %1$-" + longest + "s  %2$s\n";
        for (int i = 0; i < optStrings.size(); i++) {
            sb.append(String.format(format, optStrings.get(i), optDescs.get(i)));
        }
        return sb.toString();
    }
}
