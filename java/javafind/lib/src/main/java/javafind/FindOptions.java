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

import java.io.IOException;
import java.util.*;

import static javafind.FindError.STARTPATH_NOT_DEFINED;

public class FindOptions {
    private static final String FIND_OPTIONS_JSON_PATH = "/findoptions.json";
    private final List<FindOption> options;
    private final Map<String, String> boolMap = new HashMap<>();
    private final Map<String, String> strMap = new HashMap<>();
    private final Map<String, String> intMap = new HashMap<>();
    private final Map<String, String> longMap = new HashMap<>();
    private final ArgTokenizer argTokenizer;

    public FindOptions() throws IOException {
        options = new ArrayList<>();
        strMap.put("path", "path");
        setOptionsFromJson();
        argTokenizer = new ArgTokenizer(boolMap, strMap, intMap, longMap);
    }

    @FunctionalInterface
    private interface BooleanSetter {
        void set(Boolean b, FindSettings settings);
    }

    private final int boolActionMapSize = 22;
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
            if (boolActionMap.containsKey(longArg)) {
                boolMap.put(longArg, longArg);
            } else if (stringActionMap.containsKey(longArg)) {
                strMap.put(longArg, longArg);
            } else if (intActionMap.containsKey(longArg)) {
                intMap.put(longArg, longArg);
            } else if (longActionMap.containsKey(longArg)) {
                longMap.put(longArg, longArg);
            }
            var desc = findOptionObj.getString("desc");
            var shortArg = "";
            if (findOptionObj.has("short")) {
                shortArg = findOptionObj.getString("short");
                if (boolActionMap.containsKey(longArg)) {
                    boolMap.put(shortArg, longArg);
                } else if (stringActionMap.containsKey(longArg)) {
                    strMap.put(shortArg, longArg);
                } else if (intActionMap.containsKey(longArg)) {
                    intMap.put(shortArg, longArg);
                } else if (longActionMap.containsKey(longArg)) {
                    longMap.put(shortArg, longArg);
                }
            }
            options.add(new FindOption(shortArg, longArg, desc));
        }
    }

    private void applyArgTokenToSettings(final ArgToken argToken, FindSettings settings)
            throws FindException {
        if (argToken.type().equals(ArgTokenType.BOOL)) {
            if (argToken.value() instanceof Boolean b) {
                this.boolActionMap.get(argToken.name()).set(b, settings);
            } else {
                throw new FindException("Invalid value for option: " + argToken.name());
            }
        } else if (argToken.type().equals(ArgTokenType.STR)) {
            if (argToken.value() instanceof String s) {
                if (argToken.name().equals("settings-file")) {
                    updateSettingsFromFilePath(settings, s);
                } else {
                    this.stringActionMap.get(argToken.name()).set(s, settings);
                }
            } else if (argToken.value() instanceof Collection<?> coll) {
                for (Object item : coll) {
                    if (item instanceof String s) {
                        this.stringActionMap.get(argToken.name()).set(s, settings);
                    } else {
                        throw new FindException("Invalid value for option: " + argToken.name());
                    }
                }
            } else if (argToken.value() instanceof JSONArray jsonArray) {
                for (var i = 0; i < jsonArray.length(); i++) {
                    Object item = jsonArray.get(i);
                    if (item instanceof String s) {
                        this.stringActionMap.get(argToken.name()).set(s, settings);
                    } else {
                        throw new FindException("Invalid value for option: " + argToken.name());
                    }
                }
            } else {
                throw new FindException("Invalid value for option: " + argToken.name());
            }
        } else if (argToken.type().equals(ArgTokenType.INT)) {
            if (argToken.value() instanceof Integer i) {
                this.intActionMap.get(argToken.name()).set(i, settings);
            } else if (argToken.value() instanceof Long l) {
                this.intActionMap.get(argToken.name()).set(l.intValue(), settings);
            } else {
                throw new FindException("Invalid value for option: " + argToken.name());
            }
        } else if (argToken.type().equals(ArgTokenType.LONG)) {
            if (argToken.value() instanceof Integer i) {
                this.longActionMap.get(argToken.name()).set(i.longValue(), settings);
            } else if (argToken.value() instanceof Long l) {
                this.longActionMap.get(argToken.name()).set(l, settings);
            } else {
                throw new FindException("Invalid value for option: " + argToken.name());
            }
        } else {
            throw new FindException("Invalid option: " + argToken.name());
        }
    }

    private void updateSettingsFromArgTokens(FindSettings settings, final List<ArgToken> argTokens) throws FindException {
        for (var argToken : argTokens) {
            applyArgTokenToSettings(argToken, settings);
        }
    }

    public void updateSettingsFromJson(FindSettings settings, final String json) throws FindException {
        var argTokens = argTokenizer.tokenizeJson(json);
        updateSettingsFromArgTokens(settings, argTokens);
    }

    public final FindSettings settingsFromJson(final String json) throws FindException {
        var settings = new FindSettings();
        updateSettingsFromJson(settings, json);
        return settings;
    }

    public void updateSettingsFromFilePath(FindSettings settings, final String filePath) throws FindException {
        var argTokens = argTokenizer.tokenizeFilePath(filePath);
        updateSettingsFromArgTokens(settings, argTokens);
    }

    public FindSettings settingsFromFilePath(final String filePath) throws FindException {
        var settings = new FindSettings();
        updateSettingsFromFilePath(settings, filePath);
        return settings;
    }

    public final void updateSettingsFromArgs(FindSettings settings, final String[] args) throws FindException {
        var argTokens = argTokenizer.tokenizeArgs(args);
        updateSettingsFromArgTokens(settings, argTokens);
    }

    public final FindSettings settingsFromArgs(final String[] args) throws FindException {
        if (args == null || args.length == 0) {
            throw new FindException(STARTPATH_NOT_DEFINED.getMessage());
        }

        var settings = new FindSettings();
        // default printFiles to true since running from command line
        settings.setPrintFiles(true);
        updateSettingsFromArgs(settings, args);
        return settings;
    }

    private String getUsageString() {
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

    public final void usage(final int exitStatus) {
        System.out.println(this.getUsageString());
        System.exit(exitStatus);
    }
}
