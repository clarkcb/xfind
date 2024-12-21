package groovyfind

import groovy.json.JsonSlurper
import groovy.transform.CompileStatic

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths


@CompileStatic
class FindOption {
    final String shortArg
    final String longArg
    final String description

    FindOption(final String shortArg, final String longArg,
               final String description) {
        this.shortArg = shortArg
        this.longArg = longArg
        this.description = description
    }

    final String getSortArg() {
        if (null != this.shortArg && !this.shortArg.isEmpty()) {
            return this.shortArg.toLowerCase() + '@' + this.longArg
        }
        this.longArg
    }
}

class FindOptions {
    private static final String FIND_OPTIONS_JSON_PATH = '/findoptions.json'
    private final List<FindOption> options
    private final Map<String, String> longArgMap = new HashMap<>()

    FindOptions() throws IOException {
        options = []
        setOptionsFromJson()
    }

    @FunctionalInterface
    private interface BooleanSetter {
        void set(Boolean b, FindSettings settings);
    }

    private final Map<String, BooleanSetter> boolActionMap = [
            archivesonly: { Boolean b, FindSettings settings -> settings.archivesOnly = b },
            debug: { Boolean b, FindSettings settings -> settings.debug = b },
            excludehidden: { Boolean b, FindSettings settings -> settings.includeHidden = !b },
            followsymlinks: { Boolean b, FindSettings settings -> settings.followSymlinks = b },
            help: { Boolean b, FindSettings settings -> settings.printUsage = b },
            includearchives: { Boolean b, FindSettings settings -> settings.includeArchives = b },
            includehidden: { Boolean b, FindSettings settings -> settings.includeHidden = b },
            nofollowsymlinks: { Boolean b, FindSettings settings -> settings.followSymlinks = !b },
            noprintdirs: { Boolean b, FindSettings settings -> settings.printDirs = !b },
            noprintfiles: { Boolean b, FindSettings settings -> settings.printFiles = !b },
            norecursive: { Boolean b, FindSettings settings -> settings.recursive = !b },
            printdirs: { Boolean b, FindSettings settings -> settings.printDirs = b },
            printfiles: { Boolean b, FindSettings settings -> settings.printFiles = b },
            recursive: { Boolean b, FindSettings settings -> settings.recursive = b },
            'sort-ascending': { Boolean b, FindSettings settings -> settings.sortDescending = !b },
            'sort-caseinsensitive': { Boolean b, FindSettings settings -> settings.sortCaseInsensitive = b },
            'sort-casesensitive': { Boolean b, FindSettings settings -> settings.sortCaseInsensitive = !b },
            'sort-descending': { Boolean b, FindSettings settings -> settings.sortDescending = b },
            verbose: { Boolean b, FindSettings settings -> settings.verbose = b },
            version: { Boolean b, FindSettings settings -> settings.printVersion = b }
    ]

    @FunctionalInterface
    private interface StringSetter {
        void set(String s, FindSettings settings);
    }

    private final Map<String, StringSetter> stringActionMap = [
            'in-archiveext': { String s, FindSettings settings -> settings.addInArchiveExtension(s) },
            'in-archivefilepattern': { String s, FindSettings settings -> settings.addInArchiveFilePattern(s) },
            'in-dirpattern': { String s, FindSettings settings -> settings.addInDirPattern(s) },
            'in-ext': { String s, FindSettings settings -> settings.addInExtension(s) },
            'in-filepattern': { String s, FindSettings settings -> settings.addInFilePattern(s) },
            'in-filetype': { String s, FindSettings settings -> settings.addInFileType(s) },
            maxlastmod: { String s, FindSettings settings -> settings.setMaxLastModFromString(s) },
            minlastmod: { String s, FindSettings settings -> settings.setMinLastModFromString(s) },
            'out-archiveext': { String s, FindSettings settings -> settings.addOutArchiveExtension(s) },
            'out-archivefilepattern': { String s, FindSettings settings -> settings.addOutArchiveFilePattern(s) },
            'out-dirpattern': { String s, FindSettings settings -> settings.addOutDirPattern(s) },
            'out-ext': { String s, FindSettings settings -> settings.addOutExtension(s) },
            'out-filepattern': { String s, FindSettings settings -> settings.addOutFilePattern(s) },
            'out-filetype': { String s, FindSettings settings -> settings.addOutFileType(s) },
            path: { String s, FindSettings settings -> settings.addPath(s) },
            'sort-by': { String s, FindSettings settings -> settings.setSortBy(SortBy.forName(s)) }
    ]

    @FunctionalInterface
    private interface IntegerSetter {
        void set(Integer i, FindSettings settings);
    }

    private final Map<String, IntegerSetter> intActionMap = [
            maxdepth: { Integer i, FindSettings settings -> settings.setMaxDepth(i) },
            mindepth: { Integer i, FindSettings settings -> settings.setMinDepth(i) }
    ]

    @FunctionalInterface
    private interface LongSetter {
        void set(Long l, FindSettings settings);
    }

    private final Map<String, LongSetter> longActionMap = [
            maxsize: { Long l, FindSettings settings -> settings.setMaxSize(l) },
            minsize: { Long l, FindSettings settings -> settings.setMinSize(l) }
    ]

    private void setOptionsFromJson() throws IOException {
        JsonSlurper jsonSlurper = new JsonSlurper()
        InputStream findOptionsInputStream = getClass().getResourceAsStream(FIND_OPTIONS_JSON_PATH)
        assert findOptionsInputStream != null

        def jsonObj = jsonSlurper.parse(findOptionsInputStream)
        assert jsonObj instanceof Map
        assert jsonObj.findoptions instanceof List

        List findOptionsArray = (List)jsonObj.findoptions

        for (int i = 0; i < findOptionsArray.size(); i++) {
            Map findOptionObj = (Map)findOptionsArray[i]
            String longArg = findOptionObj.long
            longArgMap.put(longArg, longArg)
            String desc = findOptionObj.desc
            String shortArg = ''
            if ('short' in findOptionObj) {
                shortArg = findOptionObj.short
                longArgMap.put(shortArg, longArg)
            }
            options.add(new FindOption(shortArg, longArg, desc))
        }
    }

    private void applySetting(final String arg, final Object obj, FindSettings settings)
            throws FindException {
        if (arg in this.boolActionMap) {
            if (obj instanceof Boolean) {
                ((BooleanSetter)this.boolActionMap[arg]).set((Boolean)obj, settings)
            } else {
                throw new FindException("Invalid value for option: ${arg}");
            }
        } else if (arg in this.stringActionMap) {
            if (obj instanceof String) {
                ((StringSetter)this.stringActionMap[arg]).set((String)obj, settings)
            } else if (obj instanceof List) {
                for (int i = 0; i < ((List) obj).size(); i++) {
                    Object item = ((List) obj)[i]
                    if (item instanceof String) {
                        ((StringSetter) this.stringActionMap[arg]).set((String) item, settings)
                    } else {
                        throw new FindException("Invalid value for option: ${arg}");
                    }
                }
            } else {
                throw new FindException("Invalid value for option: ${arg}");
            }
        } else if (arg in this.intActionMap) {
            if (obj instanceof Integer) {
                ((IntegerSetter)this.intActionMap[arg]).set((Integer)obj, settings)
            } else if (obj instanceof Long) {
                ((IntegerSetter)this.intActionMap[arg]).set(((Long)obj).intValue(), settings)
            } else {
                throw new FindException("Invalid value for option: ${arg}");
            }
        } else if (arg in this.longActionMap) {
            if (obj instanceof Integer) {
                ((LongSetter)this.longActionMap[arg]).set(((Integer)obj).longValue(), settings)
            } else if (obj instanceof Long) {
                ((LongSetter)this.longActionMap[arg]).set((Long)obj, settings)
            } else {
                throw new FindException("Invalid value for option: ${arg}");
            }
        } else {
            throw new FindException("Invalid option: ${arg}")
        }
    }

    void settingsFromJson(final String json, FindSettings settings) {
        JsonSlurper jsonSlurper = new JsonSlurper()
        def jsonObj = jsonSlurper.parseText(json)
        assert jsonObj instanceof Map<String, Object>
        jsonObj.keySet().each { ko ->
            applySetting(ko, jsonObj.get(ko), settings)
        }
    }

    private void settingsFromFilePath(final String filePath, final FindSettings settings) {
        Path path = Paths.get(filePath)
        try {
            if (!Files.exists(path)) {
                Logger.log("Settings file not found: ${filePath}")
                System.exit(1)
            }
            if (!FileUtil.hasExtension(filePath, 'json')) {
                Logger.log("Invalid settings file type (must be JSON): ${filePath}")
                System.exit(1)
            }
            settingsFromJson(FileUtil.getFileContents(path), settings)
        } catch (FileNotFoundException ignored) {
            Logger.log("Settings file not found: ${filePath}")
            System.exit(1)
        } catch (IOException ignored) {
            Logger.log("IOException reading settings file: ${filePath}")
            System.exit(1)
        }
    }

    final FindSettings settingsFromArgs(final String[] args) throws FindException {
        FindSettings settings = new FindSettings()
        // default printFiles to true since running from command line
        settings.setPrintFiles(true)

        Queue<String> queue = new LinkedList<>(Arrays.asList(args))
        while (!queue.isEmpty()) {
            String arg = queue.remove()
            if (arg.startsWith('-')) {
                while (arg.startsWith('-')) {
                    arg = arg.substring(1)
                }
                var longArg = longArgMap.get(arg)
                if (longArg in this.boolActionMap) {
                    ((BooleanSetter)this.boolActionMap[longArg]).set(true, settings)
                } else if (longArg in this.stringActionMap
                        || longArg in this.intActionMap
                        || longArg in this.longActionMap
                        || longArg == 'settings-file') {
                    if (!queue.isEmpty()) {
                        String argVal = queue.remove()
                        if (longArg in this.stringActionMap) {
                            ((StringSetter)this.stringActionMap[longArg]).set(argVal, settings)
                        } else if (longArg in this.intActionMap) {
                            ((IntegerSetter)this.intActionMap[longArg]).set(Integer.parseInt(argVal), settings)
                        } else if (longArg in this.longActionMap) {
                            ((LongSetter)this.longActionMap[longArg]).set(Long.parseLong(argVal), settings)
                        } else if (longArg == 'settings-file') {
                            settingsFromFilePath(argVal, settings)
                        }
                    } else {
                        throw new FindException("Missing value for option ${arg}")
                    }
                } else {
                    throw new FindException("Invalid option: ${arg}")
                }
            } else {
                settings.addPath(arg)
            }
        }
        settings
    }

    final void usage(final int exitStatus) {
        System.out.println(this.getUsageString())
        System.exit(exitStatus)
    }

    final String getUsageString() {
        StringBuilder sb = new StringBuilder()
        sb.append('Usage:\n')
        sb.append(' groovyfind [options] <path> [<path> ...]\n\n')
        sb.append('Options:\n')

        this.options.sort(Comparator.comparing(FindOption::getSortArg))

        List<String> optStrings = []
        List<String> optDescs = []
        int longest = 0
        this.options.each { opt ->
            StringBuilder optString = new StringBuilder()
            String shortArg = opt.shortArg
            if (null != shortArg && !shortArg.isEmpty()) {
                optString.append('-').append(shortArg).append(',')
            }
            optString.append('--').append(opt.longArg)
            if (optString.length() > longest) {
                longest = optString.length()
            }
            optStrings.add(optString.toString())
            optDescs.add(opt.description)
        }
        final String format = ' %1$-' + longest + 's  %2$s\n'
        for (int i = 0; i < optStrings.size(); i++) {
            sb.append(String.format(format, optStrings.get(i), optDescs.get(i)))
        }
        sb.toString()
    }
}
