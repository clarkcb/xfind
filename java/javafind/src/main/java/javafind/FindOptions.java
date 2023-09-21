/*******************************************************************************
FindOptions

Class to encapsulate all command line find options

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public class FindOptions {
    private final List<FindOption> options;

    public FindOptions() throws IOException, ParseException {
        options = new ArrayList<>();
        setOptionsFromJson();
    }

    @FunctionalInterface
    private interface ArgSetter {
        void set(String s, FindSettings settings);
    }

    private final int actionMapSize = 20;
    private final Map<String, ArgSetter> argActionMap = new HashMap<String, ArgSetter>(actionMapSize) {
        {
            put("in-archiveext", (s, settings) -> settings.addInArchiveExtension(s));
            put("in-archivefilepattern", (s, settings) -> settings.addInArchiveFilePattern(s));
            put("in-dirpattern", (s, settings) -> settings.addInDirPattern(s));
            put("in-ext", (s, settings) -> settings.addInExtension(s));
            put("in-filepattern", (s, settings) -> settings.addInFilePattern(s));
            put("in-filetype", (s, settings) -> settings.addInFileType(s));
            put("maxdepth", (s, settings) -> settings.setMaxDepth(Integer.parseInt(s)));
            put("maxlastmod", (s, settings) -> settings.setMaxLastMod(s));
            put("maxsize", (s, settings) -> settings.setMaxSize(Integer.parseInt(s)));
            put("mindepth", (s, settings) -> settings.setMinDepth(Integer.parseInt(s)));
            put("minlastmod", (s, settings) -> settings.setMinLastMod(s));
            put("minsize", (s, settings) -> settings.setMinSize(Integer.parseInt(s)));
            put("out-archiveext", (s, settings) -> settings.addOutArchiveExtension(s));
            put("out-archivefilepattern", (s, settings) -> settings.addOutArchiveFilePattern(s));
            put("out-dirpattern", (s, settings) -> settings.addOutDirPattern(s));
            put("out-ext", (s, settings) -> settings.addOutExtension(s));
            put("out-filepattern", (s, settings) -> settings.addOutFilePattern(s));
            put("out-filetype", (s, settings) -> settings.addOutFileType(s));
            put("path", (s, settings) -> settings.addPath(s));
            put("settings-file", (s, settings) -> settingsFromFilePath(s, settings));
            put("sort-by", (s, settings) -> settings.setSortBy(SortByUtil.fromName(s)));
        }
    };

    @FunctionalInterface
    private interface BooleanFlagSetter {
        void set(Boolean b, FindSettings settings);
    }

    private final int flagMapSize = 16;
    private final Map<String, BooleanFlagSetter> boolflagActionMap = new HashMap<String, BooleanFlagSetter>(flagMapSize) {
        {
            put("archivesonly", (b, settings) -> settings.setArchivesOnly(b));
            put("debug", (b, settings) -> settings.setDebug(b));
            put("excludehidden", (b, settings) -> settings.setExcludeHidden(b));
            put("help", (b, settings) -> settings.setPrintUsage(b));
            put("includearchives", (b, settings) -> settings.setIncludeArchives(b));
            put("includehidden", (b, settings) -> settings.setExcludeHidden(!b));
            put("listdirs", (b, settings) -> settings.setListDirs(b));
            put("listfiles", (b, settings) -> settings.setListFiles(b));
            put("norecursive", (b, settings) -> settings.setRecursive(!b));
            put("recursive", (b, settings) -> settings.setRecursive(b));
            put("sort-ascending", (b, settings) -> settings.setSortDescending(!b));
            put("sort-caseinsensitive", (b, settings) -> settings.setSortCaseInsensitive(b));
            put("sort-casesensitive", (b, settings) -> settings.setSortCaseInsensitive(!b));
            put("sort-descending", (b, settings) -> settings.setSortDescending(b));
            put("verbose", (b, settings) -> settings.setVerbose(b));
            put("version", (b, settings) -> settings.setPrintVersion(b));
        }
    };

    private void setOptionsFromJson() throws IOException, ParseException {
        final var findOptionsJsonPath = "/findoptions.json";
        var findOptionsInputStream = getClass().getResourceAsStream(findOptionsJsonPath);
        assert findOptionsInputStream != null;
        var obj = new JSONParser().parse(new InputStreamReader(findOptionsInputStream));
        var jsonObj = (JSONObject)obj;
        var findoptionsArray = (JSONArray) jsonObj.get("findoptions");

        for (var o : findoptionsArray) {
            var findoptionMap = (Map) o;
            var longArg = (String) findoptionMap.get("long");
            var desc = (String) findoptionMap.get("desc");
            var shortArg = "";
            if (findoptionMap.containsKey("short")) {
                shortArg = (String) findoptionMap.get("short");
            }
            options.add(new FindOption(shortArg, longArg, desc));
        }
    }

    private List<String> listFromJSONArray(final JSONArray arr) {
        List<String> list = new ArrayList<>();
        for (var o : arr) {
            list.add((String)o);
        }
        return list;
    }

    private void settingsFromFilePath(final String filePath, final FindSettings settings) {
        var path = Paths.get(filePath);
        try {
            if (!Files.exists(path)) {
                Logger.log("Settings file not found: " + filePath);
                System.exit(1);
            }
            if (!FileUtil.hasExtension(filePath, "json")) {
                Logger.log("Invalid settings file type (just be JSON): " + filePath);
                System.exit(1);
            }
            settingsFromJson(FileUtil.getFileContents(path), settings);
        } catch (FileNotFoundException e) {
            Logger.log("Settings file not found: " + filePath);
            System.exit(1);
        } catch (IOException e) {
            Logger.log("IOException reading settings file: " + filePath);
            System.exit(1);
        } catch (ParseException e) {
            Logger.log("ParseException trying to parse the JSON in " + filePath);
            System.exit(1);
        }
    }

    public void settingsFromJson(final String json, FindSettings settings) throws ParseException {
        var obj = JSONValue.parseWithException(json);
        var jsonObject = (JSONObject)obj;
        for (var ko : jsonObject.keySet()) {
            var k = (String)ko;
            var vo = jsonObject.get(ko);
            applySetting(k, vo, settings);
        }
    }

    private void applySetting(final String arg, final Object obj, FindSettings settings) {
        if (obj.getClass().equals(String.class)) {
            try {
                applySetting(arg, (String)obj, settings);
            } catch (FindException e) {
                Logger.logError("FindException: " + e.getMessage());
            }
        } else if (obj.getClass().equals(Boolean.class)) {
            try {
                applySetting(arg, (Boolean)obj, settings);
            } catch (FindException e) {
                Logger.logError("FindException: " + e.getMessage());
            }
        } else if (obj.getClass().equals(Long.class)) {
            try {
                applySetting(arg, obj.toString(), settings);
            } catch (FindException e) {
                Logger.logError("FindException: " + e.getMessage());
            }
        } else if (obj.getClass().equals(JSONArray.class)) {
            for (String s : listFromJSONArray((JSONArray)obj)) {
                try {
                    applySetting(arg, s, settings);
                } catch (FindException e) {
                    Logger.logError("FindException: " + e.getMessage());
                }
            }
        } else {
            Logger.log("obj is another class type");
        }
    }

    private void applySetting(final String arg, final String val, FindSettings settings)
            throws FindException{
        if (this.argActionMap.containsKey(arg)) {
            this.argActionMap.get(arg).set(val, settings);
        } else if (arg.equals("path")) {
            settings.addPath(val);
        } else {
            throw new FindException("Invalid option: " + arg);
        }
    }

    private void applySetting(final String arg, final Boolean val, FindSettings settings)
            throws FindException{
        if (this.boolflagActionMap.containsKey(arg)) {
            this.boolflagActionMap.get(arg).set(val, settings);
        } else {
            throw new FindException("Invalid option: " + arg);
        }
    }

    public final FindSettings settingsFromArgs(final String[] args) throws FindException {
        var settings = new FindSettings();
        // default listFiles to true since running from command line
        settings.setListFiles(true);

        // add short arg mappings
        options.stream().filter(o -> !o.getShortArg().isEmpty()).forEach(o -> {
            if (argActionMap.containsKey(o.getLongArg())) {
                argActionMap.put(o.getShortArg(), argActionMap.get(o.getLongArg()));
            } else if (boolflagActionMap.containsKey(o.getLongArg())) {
                boolflagActionMap.put(o.getShortArg(), boolflagActionMap.get(o.getLongArg()));
            }
        });

        Queue<String> queue = new LinkedList<>(Arrays.asList(args));
        while (!queue.isEmpty()) {
            String arg = queue.remove();
            if (arg.startsWith("-")) {
                while (arg.startsWith("-")) {
                    arg = arg.substring(1);
                }
                if (this.argActionMap.containsKey(arg)) {
                    if (!queue.isEmpty()) {
                        String argVal = queue.remove();
                        this.argActionMap.get(arg).set(argVal, settings);
                    } else {
                        throw new FindException("Missing value for option " + arg);
                    }
                } else if (this.boolflagActionMap.containsKey(arg)) {
                    this.boolflagActionMap.get(arg).set(true, settings);
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
        for (FindOption opt : this.options) {
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
