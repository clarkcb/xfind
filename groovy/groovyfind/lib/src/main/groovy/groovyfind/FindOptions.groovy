package groovyfind

import groovy.json.JsonSlurper
import groovy.transform.CompileStatic

import java.nio.file.Files
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
        return this.longArg
    }
}

class FindOptions {

    private static final String FIND_OPTIONS_JSON_PATH = '/findoptions.json'
    private final List<FindOption> options

    FindOptions() throws IOException {
        options = []
        setOptionsFromJson()
    }

    @FunctionalInterface
    private interface ArgSetter {
        void set(String s, FindSettings settings);
    }

    private final def argActionMap = [
            'in-archiveext': { String s, FindSettings settings -> settings.addInArchiveExtension(s) },
            'in-archivefilepattern': { String s, FindSettings settings -> settings.addInArchiveFilePattern(s) },
            'in-dirpattern': { String s, FindSettings settings -> settings.addInDirPattern(s) },
            'in-ext': { String s, FindSettings settings -> settings.addInExtension(s) },
            'in-filepattern': { String s, FindSettings settings -> settings.addInFilePattern(s) },
            'in-filetype': { String s, FindSettings settings -> settings.addInFileType(s) },
            maxdepth: { String s, FindSettings settings -> settings.setMaxDepth(Integer.parseInt(s)) },
            maxlastmod: { String s, FindSettings settings -> settings.setMaxLastModFromString(s) },
            maxsize: { String s, FindSettings settings -> settings.setMaxSize(Integer.parseInt(s)) },
            mindepth: { String s, FindSettings settings -> settings.setMinDepth(Integer.parseInt(s)) },
            minlastmod: { String s, FindSettings settings -> settings.setMinLastModFromString(s) },
            minsize: { String s, FindSettings settings -> settings.setMinSize(Integer.parseInt(s)) },
            'out-archiveext': { String s, FindSettings settings -> settings.addOutArchiveExtension(s) },
            'out-archivefilepattern': { String s, FindSettings settings -> settings.addOutArchiveFilePattern(s) },
            'out-dirpattern': { String s, FindSettings settings -> settings.addOutDirPattern(s) },
            'out-ext': { String s, FindSettings settings -> settings.addOutExtension(s) },
            'out-filepattern': { String s, FindSettings settings -> settings.addOutFilePattern(s) },
            'out-filetype': { String s, FindSettings settings -> settings.addOutFileType(s) },
            path: { String s, FindSettings settings -> settings.addPath(s) },
            'settings-file': { String s, FindSettings settings -> settingsFromFilePath(s, settings) },
            'sort-by': { String s, FindSettings settings -> settings.setSortBy(SortByUtil.fromName(s)) }
    ]

    @FunctionalInterface
    private interface BooleanFlagSetter {
        void set(Boolean b, FindSettings settings);
    }

    private final def boolFlagActionMap = [
            archivesonly: { Boolean b, FindSettings settings -> settings.archivesOnly = b },
            debug: { Boolean b, FindSettings settings -> settings.debug = b },
            excludehidden: { Boolean b, FindSettings settings -> settings.includeHidden = !b },
            help: { Boolean b, FindSettings settings -> settings.printUsage = b },
            includearchives: { Boolean b, FindSettings settings -> settings.includeArchives = b },
            includehidden: { Boolean b, FindSettings settings -> settings.includeHidden = b },
            listdirs: { Boolean b, FindSettings settings -> settings.listDirs = b },
            listfiles: { Boolean b, FindSettings settings -> settings.listFiles = b },
            nolistfiles: { Boolean b, FindSettings settings -> settings.listFiles = !b },
            norecursive: { Boolean b, FindSettings settings -> settings.recursive = !b },
            recursive: { Boolean b, FindSettings settings -> settings.recursive = b },
            'sort-ascending': { Boolean b, FindSettings settings -> settings.sortDescending = !b },
            'sort-caseinsensitive': { Boolean b, FindSettings settings -> settings.sortCaseInsensitive = b },
            'sort-casesensitive': { Boolean b, FindSettings settings -> settings.sortCaseInsensitive = !b },
            'sort-descending': { Boolean b, FindSettings settings -> settings.sortDescending = b },
            verbose: { Boolean b, FindSettings settings -> settings.verbose = b },
            version: { Boolean b, FindSettings settings -> settings.printVersion = b }
    ]

    private void setOptionsFromJson() throws IOException {
        def jsonSlurper = new JsonSlurper()
        def findOptionsInputStream = getClass().getResourceAsStream(FIND_OPTIONS_JSON_PATH)
        assert findOptionsInputStream != null

        def jsonObj = jsonSlurper.parse(findOptionsInputStream)
        assert jsonObj instanceof Map
        assert jsonObj.findoptions instanceof List

        def findOptionsArray = (List)jsonObj.findoptions

        for (int i = 0; i < findOptionsArray.size(); i++) {
            def findOptionObj = (Map)findOptionsArray[i]
            String longArg = findOptionObj.long
            String desc = findOptionObj.desc
            String shortArg = ''
            if (findOptionObj.containsKey('short')) {
                shortArg = findOptionObj.short
            }
            options.add(new FindOption(shortArg, longArg, desc))
        }
    }

    private void settingsFromFilePath(final String filePath, final FindSettings settings) {
        def path = Paths.get(filePath)
        try {
            if (!Files.exists(path)) {
                Logger.log("Settings file not found: ${filePath}")
                System.exit(1)
            }
            if (!FileUtil.hasExtension(filePath, 'json')) {
                Logger.log("Invalid settings file type (just be JSON): ${filePath}")
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

    void settingsFromJson(final String json, FindSettings settings) {
        def jsonSlurper = new JsonSlurper()
        def jsonObj = jsonSlurper.parseText(json)
        assert jsonObj instanceof Map<String, Object>
        jsonObj.keySet().each { ko ->
            applySetting(ko, jsonObj.get(ko), settings)
        }
    }

    private void applySetting(final String arg, final Object obj, FindSettings settings) {
        if (obj instanceof String) {
            try {
                applySetting(arg, (String)obj, settings)
            } catch (FindException e) {
                Logger.logError("FindException: ${e.getMessage()}")
            }
        } else if (obj instanceof Boolean) {
            try {
                applySetting(arg, (Boolean)obj, settings)
            } catch (FindException e) {
                Logger.logError("FindException: ${e.getMessage()}")
            }
        } else if (obj instanceof Long) {
            try {
                applySetting(arg, obj.toString(), settings)
            } catch (FindException e) {
                Logger.logError("FindException: ${e.getMessage()}")
            }
        } else if (obj instanceof List) {
            for (int i=0; i < ((List)obj).size(); i++) {
                applySetting(arg, ((List)obj)[i], settings)
            }
        } else {
            Logger.log('obj is another class type')
        }
    }

    private void applySetting(final String arg, final String val, FindSettings settings)
            throws FindException {
        if (this.argActionMap.containsKey(arg)) {
            ((ArgSetter)this.argActionMap.get(arg)).set(val, settings)
        } else if (arg == 'path') {
            settings.addPath(val)
        } else {
            throw new FindException("Invalid option: ${arg}")
        }
    }

    private void applySetting(final String arg, final Boolean val, FindSettings settings)
            throws FindException{
        if (this.boolFlagActionMap.containsKey(arg)) {
            ((BooleanFlagSetter)this.boolFlagActionMap.get(arg)).set(val, settings)
        } else {
            throw new FindException("Invalid option: ${arg}")
        }
    }

    final FindSettings settingsFromArgs(final String[] args) throws FindException {
        def settings = new FindSettings()
        // default listFiles to true since running from command line
        settings.setListFiles(true)

        // add short arg mappings
        options.stream().filter(o -> !o.shortArg.isEmpty()).forEach(o -> {
            if (argActionMap.containsKey(o.longArg)) {
                argActionMap.put(o.shortArg, argActionMap.get(o.longArg))
            } else if (boolFlagActionMap.containsKey(o.longArg)) {
                boolFlagActionMap.put(o.shortArg, boolFlagActionMap.get(o.longArg))
            }
        })

        Queue<String> queue = new LinkedList<>(Arrays.asList(args))
        while (!queue.isEmpty()) {
            String arg = queue.remove()
            if (arg.startsWith('-')) {
                while (arg.startsWith('-')) {
                    arg = arg.substring(1)
                }
                if (this.argActionMap.containsKey(arg)) {
                    if (!queue.isEmpty()) {
                        String argVal = queue.remove()
                        ((ArgSetter)this.argActionMap.get(arg)).set(argVal, settings)
                    } else {
                        throw new FindException("Missing value for option ${arg}")
                    }
                } else if (this.boolFlagActionMap.containsKey(arg)) {
                    ((BooleanFlagSetter)this.boolFlagActionMap.get(arg)).set(true, settings)
                } else {
                    throw new FindException("Invalid option: ${arg}")
                }
            } else {
                settings.addPath(arg)
            }
        }
        return settings
    }

    final void usage(final int exitStatus) {
        System.out.println(this.getUsageString())
        System.exit(exitStatus)
    }

    final String getUsageString() {
        def sb = new StringBuilder()
        sb.append('Usage:\n')
        sb.append(' groovyfind [options] <path> [<path> ...]\n\n')
        sb.append('Options:\n')

        this.options.sort(Comparator.comparing(FindOption::getSortArg))

        def optStrings = []
        def optDescs = []
        int longest = 0
        this.options.each { opt ->
            def optString = new StringBuilder()
            def shortArg = opt.shortArg
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
        final def format = ' %1$-' + longest + 's  %2$s\n'
        for (int i = 0; i < optStrings.size(); i++) {
            sb.append(String.format(format, optStrings.get(i), optDescs.get(i)))
        }
        return sb.toString()
    }

}
