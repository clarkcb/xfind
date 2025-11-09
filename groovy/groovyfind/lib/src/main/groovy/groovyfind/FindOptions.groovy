package groovyfind

import groovy.json.JsonSlurper
import groovy.transform.CompileStatic
import groovy.transform.TypeCheckingMode

interface Option {
    String shortArg()
    String longArg()
    String description()
    ArgTokenType argType()
}

@CompileStatic
class FindOption implements Option {
    final String shortArg
    final String longArg
    final String description
    final ArgTokenType argType

    FindOption(final String shortArg, final String longArg,
               final String description, final ArgTokenType argType) {
        this.shortArg = shortArg
        this.longArg = longArg
        this.description = description
        this.argType = argType
    }

    final String getSortArg() {
        if (null != this.shortArg && !this.shortArg.isEmpty()) {
            return this.shortArg.toLowerCase() + '@' + this.longArg
        }
        this.longArg
    }

    @Override
    String shortArg() {
        return shortArg
    }

    @Override
    String longArg() {
        return longArg
    }

    @Override
    String description() {
        return description
    }

    @Override
    ArgTokenType argType() {
        return argType
    }
}

@CompileStatic(TypeCheckingMode.SKIP)
class FindOptions {
    private static final String FIND_OPTIONS_JSON_PATH = '/findoptions.json'
    private final List<FindOption> options
    private ArgTokenizer argTokenizer

    FindOptions() throws IOException {
        options = []
        setOptionsFromJson()
        argTokenizer = new ArgTokenizer(options as List<Option>)
    }

    @FunctionalInterface
    private interface BooleanSetter {
        void set(Boolean b, FindSettings settings)
    }

    private final Map<String, BooleanSetter> boolActionMap = [
            archivesonly: { Boolean b, FindSettings settings -> settings.archivesOnly = b },
            colorize: { Boolean b, FindSettings settings -> settings.colorize = b },
            debug: { Boolean b, FindSettings settings -> settings.debug = b },
            excludehidden: { Boolean b, FindSettings settings -> settings.includeHidden = !b },
            followsymlinks: { Boolean b, FindSettings settings -> settings.followSymlinks = b },
            help: { Boolean b, FindSettings settings -> settings.printUsage = b },
            includearchives: { Boolean b, FindSettings settings -> settings.includeArchives = b },
            includehidden: { Boolean b, FindSettings settings -> settings.includeHidden = b },
            nocolorize: { Boolean b, FindSettings settings -> settings.colorize = !b },
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
        void set(String s, FindSettings settings)
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
        void set(Integer i, FindSettings settings)
    }

    private final Map<String, IntegerSetter> intActionMap = [
            maxdepth: { Integer i, FindSettings settings -> settings.setMaxDepth(i) },
            mindepth: { Integer i, FindSettings settings -> settings.setMinDepth(i) }
    ]

    @FunctionalInterface
    private interface LongSetter {
        void set(Long l, FindSettings settings)
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
            ArgTokenType argType = ArgTokenType.UNKNOWN
            if (longArg in this.boolActionMap) {
                argType = ArgTokenType.BOOL
            } else if (longArg in this.stringActionMap) {
                argType = ArgTokenType.STR
            } else if (longArg in this.intActionMap) {
                argType = ArgTokenType.INT
            } else if (longArg in this.longActionMap) {
                argType = ArgTokenType.LONG
            }
            String desc = findOptionObj.desc
            String shortArg = ''
            if ('short' in findOptionObj) {
                shortArg = findOptionObj.short
            }
            options.add(new FindOption(shortArg, longArg, desc, argType))
        }
    }

    private void applyArgTokenToSetting(final ArgToken argToken, FindSettings settings)
            throws FindException {
        if (argToken.type == ArgTokenType.BOOL) {
            if (argToken.value instanceof Boolean) {
                ((BooleanSetter)this.boolActionMap[argToken.name]).set((Boolean)argToken.value, settings)
            } else {
                throw new FindException("Invalid value for option: ${argToken.name}")
            }
        } else if (argToken.type == ArgTokenType.STR) {
            if (argToken.value instanceof String) {
                if (argToken.name == 'settings-file') {
                    updateSettingsFromFilePath(settings, argToken.value as String)
                } else {
                    ((StringSetter)this.stringActionMap[argToken.name]).set((String)argToken.value, settings)
                }
            } else if (argToken.value instanceof List) {
                for (int i = 0; i < ((List) argToken.value).size(); i++) {
                    Object item = ((List) argToken.value)[i]
                    if (item instanceof String) {
                        ((StringSetter) this.stringActionMap[argToken.name]).set((String) item, settings)
                    } else {
                        throw new FindException("Invalid value for option: ${argToken.name}")
                    }
                }
            } else {
                throw new FindException("Invalid value for option: ${argToken.name}")
            }
        } else if (argToken.type == ArgTokenType.INT) {
            if (argToken.value instanceof Integer) {
                ((IntegerSetter)this.intActionMap[argToken.name]).set((Integer)argToken.value, settings)
            } else if (argToken.value instanceof Long) {
                ((IntegerSetter)this.intActionMap[argToken.name]).set(((Long)argToken.value).intValue(), settings)
            } else {
                throw new FindException("Invalid value for option: ${argToken.name}")
            }
        } else if (argToken.type == ArgTokenType.LONG) {
            if (argToken.value instanceof Integer) {
                ((LongSetter)this.longActionMap[argToken.name]).set(((Integer)argToken.value).longValue(), settings)
            } else if (argToken.value instanceof Long) {
                ((LongSetter)this.longActionMap[argToken.name]).set((Long)argToken.value, settings)
            } else {
                throw new FindException("Invalid value for option: ${argToken.name}")
            }
        } else {
            throw new FindException("Invalid option: ${argToken.name}")
        }
    }

    private void updateSettingsFromArgTokens(FindSettings settings, final List<ArgToken> argTokens) {
        for (var argToken : argTokens) {
            applyArgTokenToSetting(argToken, settings)
        }
    }

    final void updateSettingsFromJson(FindSettings settings, final String json) {
        var argTokens = argTokenizer.tokenizeJson(json)
        updateSettingsFromArgTokens(settings, argTokens)
    }

    final FindSettings settingsFromJson(final String json) throws FindException {
        var settings = new FindSettings()
        updateSettingsFromJson(settings, json)
        return settings
    }

    final void updateSettingsFromFilePath(FindSettings settings, final String filePath) {
        var argTokens = argTokenizer.tokenizeFilePath(filePath)
        updateSettingsFromArgTokens(settings, argTokens)
    }

    final FindSettings settingsFromFilePath(final String filePath) throws FindException {
        var settings = new FindSettings()
        updateSettingsFromFilePath(settings, filePath)
        return settings
    }

    final void updateSettingsFromArgs(FindSettings settings, final String[] args) throws FindException {
        var argTokens = argTokenizer.tokenizeArgs(args)
        updateSettingsFromArgTokens(settings, argTokens)
    }

    final FindSettings settingsFromArgs(final String[] args) throws FindException {
        if (args == null || args.length == 0) {
            throw new FindException(FindError.STARTPATH_NOT_DEFINED.getMessage())
        }
        var settings = new FindSettings()
        // default printFiles to true since running from command line
        settings.setPrintFiles(true)

        updateSettingsFromArgs(settings, args)

        return settings
    }

    private String getUsageString() {
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

    final void usage(final int exitStatus) {
        System.out.println(this.getUsageString())
        System.exit(exitStatus)
    }
}
