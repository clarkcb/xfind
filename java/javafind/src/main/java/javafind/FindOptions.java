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

import java.io.*;
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

    private final int actionMapSize = 24;
    private final Map<String, ArgSetter> argActionMap = new HashMap<String, ArgSetter>(actionMapSize) {
        {
            put("in-archiveext", (s, settings) -> settings.addInArchiveExtension(s));
            put("in-archivefilepattern", (s, settings) -> settings.addInArchiveFilePattern(s));
            put("in-dirpattern", (s, settings) -> settings.addInDirPattern(s));
            put("in-ext", (s, settings) -> settings.addInExtension(s));
            put("in-filepattern", (s, settings) -> settings.addInFilePattern(s));
            put("in-filetype", (s, settings) -> settings.addInFileType(s));
            put("out-archiveext", (s, settings) -> settings.addOutArchiveExtension(s));
            put("out-archivefilepattern", (s, settings) -> settings.addOutArchiveFilePattern(s));
            put("out-dirpattern", (s, settings) -> settings.addOutDirPattern(s));
            put("out-ext", (s, settings) -> settings.addOutExtension(s));
            put("out-filepattern", (s, settings) -> settings.addOutFilePattern(s));
            put("out-filetype", (s, settings) -> settings.addOutFileType(s));
            put("settings-file", (s, settings) -> settingsFromFilePath(s, settings));
        }
    };

    @FunctionalInterface
    private interface BooleanFlagSetter {
        void set(Boolean b, FindSettings settings);
    }

    private final int flagMapSize = 22;
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
            put("verbose", (b, settings) -> settings.setVerbose(b));
            put("version", (b, settings) -> settings.setPrintVersion(b));
        }
    };

    private void setOptionsFromJson() throws IOException, ParseException {
        final String findOptionsJsonPath = "/findoptions.json";
        InputStream findOptionsInputStream = getClass().getResourceAsStream(findOptionsJsonPath);
        Object obj = new JSONParser().parse(new InputStreamReader(findOptionsInputStream));
        JSONObject jsonObj = (JSONObject)obj;
        JSONArray findoptionsArray = (JSONArray) jsonObj.get("findoptions");

        for (Object o : findoptionsArray) {
            Map findoptionMap = (Map) o;
            String longArg = (String) findoptionMap.get("long");
            String desc = (String) findoptionMap.get("desc");
            String shortArg = "";
            if (findoptionMap.containsKey("short")) {
                shortArg = (String) findoptionMap.get("short");
            }
            options.add(new FindOption(shortArg, longArg, desc));
        }
    }

    private List<String> listFromJSONArray(final JSONArray arr) {
        List<String> list = new ArrayList<>();
        for (Object o : arr) {
            list.add((String)o);
        }
        return list;
    }

    private void settingsFromFilePath(final String filePath, final FindSettings settings) {
        Path path = Paths.get(filePath);
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
        Object obj = JSONValue.parseWithException(json);
        JSONObject jsonObject = (JSONObject)obj;
        for (Object ko : jsonObject.keySet()) {
            String k = (String)ko;
            Object vo = jsonObject.get(ko);
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
        FindSettings settings = new FindSettings();
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
                while (arg.length() > 0 && arg.startsWith("-")) {
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
        StringBuilder sb = new StringBuilder();
        sb.append("Usage:\n");
        sb.append(" javafind [options] <path> [<path> ...]\n\n");
        sb.append("Options:\n");

        this.options.sort(Comparator.comparing(FindOption::getSortArg));

        List<String> optStrings = new ArrayList<>();
        List<String> optDescs = new ArrayList<>();
        int longest = 0;
        for (FindOption opt : this.options) {
            StringBuilder optString = new StringBuilder();
            String shortArg = opt.getShortArg();
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
        final String format = " %1$-" + longest + "s  %2$s\n";
        for (int i = 0; i < optStrings.size(); i++) {
            sb.append(String.format(format, optStrings.get(i), optDescs.get(i)));
        }
        return sb.toString();
    }
}
