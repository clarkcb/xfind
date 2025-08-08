/*******************************************************************************
FindOptions

Class to encapsulate all command line find options

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

import static javafind.FindError.STARTPATH_NOT_DEFINED;

public class FindOptions {
    private static final String FIND_OPTIONS_JSON_PATH = "/findoptions.json";
    private final List<FindOption> options;
    private final Map<String, String> longArgMap = new HashMap<>(){
        {
            put("path", "path");
        }
    };

    public FindOptions() throws IOException {
        options = new ArrayList<>();
        setOptionsFromJson();
    }

    @FunctionalInterface
    private interface BooleanSetter {
        void set(Boolean b, FindSettings settings);
    }

    private final int boolActionMapSize = 20;
    private final Map<String, BooleanSetter> boolActionMap = new HashMap<>(boolActionMapSize) {
        {
            put("archivesonly", (b, settings) -> settings.setArchivesOnly(b));
            put("colorize", (b, settings) -> settings.setColorize(b));
            put("debug", (b, settings) -> settings.setDebug(b));
            put("excludehidden", (b, settings) -> settings.setIncludeHidden(!b));
            put("followsymlinks", (b, settings) -> settings.setFollowSymlinks(b));
            put("help", (b, settings) -> settings.setPrintUsage(b));
            put("includearchives", (b, settings) -> settings.setIncludeArchives(b));
            put("includehidden", (b, settings) -> settings.setIncludeHidden(b));
            put("nocolorize", (b, settings) -> settings.setColorize(!b));
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

    private final int stringActionMapSize = 16;
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

        for (var i=0; i<findOptionsArray.length(); i++) {
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

    private void applySetting(final String arg, final Object obj, FindSettings settings)
            throws FindException {
        if (this.boolActionMap.containsKey(arg)) {
            if (obj instanceof Boolean b) {
                this.boolActionMap.get(arg).set(b, settings);
            } else {
                throw new FindException("Invalid value for option: " + arg);
            }
        } else if (this.stringActionMap.containsKey(arg)) {
            if (obj instanceof String s) {
                this.stringActionMap.get(arg).set(s, settings);
            } else if (obj instanceof Collection coll) {
                for (Object item : coll) {
                    if (!(item instanceof String)) {
                        throw new FindException("Invalid value for option: " + arg);
                    }
                    this.stringActionMap.get(arg).set((String) item, settings);
                }
            } else if (obj instanceof JSONArray jsonArray) {
                for (var i = 0; i < jsonArray.length(); i++) {
                    Object item = jsonArray.get(i);
                    if (!(item instanceof String)) {
                        throw new FindException("Invalid value for option: " + arg);
                    }
                    this.stringActionMap.get(arg).set((String)item, settings);
                }
            } else {
                throw new FindException("Invalid value for option: " + arg);
            }
        } else if (this.intActionMap.containsKey(arg)) {
            if (obj instanceof Integer i) {
                this.intActionMap.get(arg).set(i, settings);
            } else if (obj instanceof Long l) {
                this.intActionMap.get(arg).set(l.intValue(), settings);
            } else {
                throw new FindException("Invalid value for option: " + arg);
            }
        } else if (this.longActionMap.containsKey(arg)) {
            if (obj instanceof Integer i) {
                this.longActionMap.get(arg).set(i.longValue(), settings);
            } else if (obj instanceof Long l) {
                this.longActionMap.get(arg).set(l, settings);
            } else {
                throw new FindException("Invalid value for option: " + arg);
            }
        } else if (arg.equals("settings-file")) {
            if (obj instanceof String filePath) {
                updateSettingsFromFilePath(settings, filePath);
            } else {
                throw new FindException("Invalid value for option: " + arg);
            }
        } else {
            throw new FindException("Invalid option: " + arg);
        }
    }

    private void updateSettingsFromJson(FindSettings settings, final String json) throws FindException {
        try {
            var jsonObj = new JSONObject(new JSONTokener(json));
            // keys are sorted so that output is consistent across all versions
            var keys = jsonObj.keySet().stream().sorted().toList();
            var invalidKeys = keys.stream().filter(k -> !longArgMap.containsKey(k)).toList();
            if (!invalidKeys.isEmpty()) {
                throw new FindException("Invalid option: " + invalidKeys.get(0));
            }
            for (var k : keys) {
                var v = jsonObj.get(k);
                if (v != null) {
                    applySetting(k, v, settings);
                }
            }
        } catch (JSONException e) {
            throw new FindException("Invalid JSON format: " + e.getMessage());
        } catch (ClassCastException e) {
            throw new FindException("Invalid value type in JSON: " + e.getMessage());
        }
    }

    public final FindSettings settingsFromJson(final String json) throws FindException {
        var settings = new FindSettings();
        updateSettingsFromJson(settings, json);
        return settings;
    }

    private void updateSettingsFromFilePath(FindSettings settings, final String filePath) throws FindException {
        var path = FileUtil.expandPath(Paths.get(filePath));
        if (!Files.exists(path)) {
            throw new FindException("Settings file not found: " + filePath);
        }
        if (!filePath.endsWith(".json")) {
            throw new FindException("Invalid settings file (must be JSON): " + filePath);
        }
        try {
            updateSettingsFromJson(settings, FileUtil.getFileContents(path));
        } catch (FileNotFoundException e) {
            throw new FindException("Settings file not found: " + filePath);
        } catch (IOException e) {
            throw new FindException("IOException reading settings file: " + filePath);
        }
    }

    public FindSettings settingsFromFilePath(final String filePath) throws FindException {
        var settings = new FindSettings();
        updateSettingsFromFilePath(settings, filePath);
        return settings;
    }

    private Map<String, Object> argMapFromArgs(final String[] args) throws FindException {
        Map<String, Object> argMap = new HashMap<>();
        Deque<String> deque = new ArrayDeque<>(Arrays.asList(args));
        var stop = false;
        while (!deque.isEmpty() && !stop) {
            var arg = deque.removeFirst();
            if (arg.startsWith("-")) {
                var argNames = new ArrayList<String>();
                if (arg.startsWith("--")) {
                    if (arg.length() < 3) {
                        throw new FindException("Invalid option: " + arg);
                    }
                    arg = arg.substring(2);
                    if (arg.contains("=")) {
                        var parts = arg.split("=", 2);
                        if (parts.length != 2) {
                            throw new FindException("Invalid option: " + arg);
                        }
                        arg = parts[0];
                        var value = parts[1];
                        deque.addFirst(value);
                    }
                    if (!longArgMap.containsKey(arg)) {
                        throw new FindException("Invalid option: " + arg);
                    }
                    argNames.add(longArgMap.get(arg));
                } else if (arg.length() > 1) {
                    arg = arg.substring(1);
                    for (char c : arg.toCharArray()) {
                        if (!longArgMap.containsKey(String.valueOf(c))) {
                            throw new FindException("Invalid option: -" + c);
                        }
                        argNames.add(longArgMap.get(String.valueOf(c)));
                    }
                } else {
                    throw new FindException("Invalid option: " + arg);
                }

                for (String argName : argNames) {
                    if (this.boolActionMap.containsKey(argName)) {
                        argMap.put(argName, true);
                    } else if (this.stringActionMap.containsKey(argName)
                            || this.intActionMap.containsKey(argName)
                            || this.longActionMap.containsKey(argName)
                            || argName.equals("settings-file")) {
                        if (!deque.isEmpty()) {
                            String argVal = deque.remove();
                            if (this.stringActionMap.containsKey(argName)) {
                                if (!argMap.containsKey(argName)) {
                                    argMap.put(argName, new ArrayList<String>());
                                }
                                ((ArrayList<String>)argMap.get(argName)).add(argVal);
                            } else if (this.intActionMap.containsKey(argName)) {
                                argMap.put(argName, Integer.parseInt(argVal));
                            } else if (this.longActionMap.containsKey(argName)) {
                                argMap.put(argName, Long.parseLong(argVal));
                            } else if (argName.equals("settings-file")) {
                                argMap.put(argName, argVal);
                            } else {
                                throw new FindException("Invalid option: " + arg);
                            }
                        } else {
                            throw new FindException("Missing value for option " + arg);
                        }
                    } else {
                        throw new FindException("Invalid option: " + arg);
                    }
                }
            } else {
                // treat as a path
                final String path = "path";
                if (!argMap.containsKey(path)) {
                    argMap.put(path, new ArrayList<String>());
                }
                ((ArrayList<String>)argMap.get(path)).add(arg);
            }
            stop = argMap.containsKey("help") || argMap.containsKey("version");
        }
        return argMap;
    }

    public final FindSettings settingsFromArgs(final String[] args) throws FindException {
        if (args == null || args.length == 0) {
            throw new FindException(STARTPATH_NOT_DEFINED.getMessage());
        }
        var settings = new FindSettings();
        // default printFiles to true since running from command line
        settings.setPrintFiles(true);

        var argMap = argMapFromArgs(args);
        if (argMap.containsKey("help")) {
            settings.setPrintUsage(true);
            return settings;
        }
        if (argMap.containsKey("version")) {
            settings.setPrintVersion(true);
            return settings;
        }
        for (var entry : argMap.entrySet()) {
            applySetting(entry.getKey(), entry.getValue(), settings);
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
            var shortArg = opt.shortArg();
            if (null != shortArg && !shortArg.isEmpty()) {
                optString.append("-").append(shortArg).append(",");
            }
            optString.append("--").append(opt.longArg());
            if (optString.length() > longest) {
                longest = optString.length();
            }
            optStrings.add(optString.toString());
            optDescs.add(opt.description());
        }
        final var format = " %1$-" + longest + "s  %2$s\n";
        for (var i = 0; i < optStrings.size(); i++) {
            sb.append(String.format(format, optStrings.get(i), optDescs.get(i)));
        }
        return sb.toString();
    }
}
