package groovyfind

import java.nio.file.Files
import java.nio.file.Paths

import groovy.json.JsonSlurper

import java.util.stream.Collectors

class ArgTokenizer {
    private Map<String, String> boolMap = new HashMap<>()
    private Map<String, String> strMap = new HashMap<>()
    private Map<String, String> intMap = new HashMap<>()
    private Map<String, String> longMap = new HashMap<>()

    ArgTokenizer(final Map<String, String> boolMap,
                 final Map<String, String> strMap,
                 final Map<String, String> intMap,
                 final Map<String, String> longMap) {
        this.boolMap = boolMap
        this.strMap = strMap
        this.intMap = intMap
        this.longMap = longMap
    }

    List<ArgToken> tokenizeArgs(final String[] args) throws FindException {
        List<ArgToken> argTokens = new ArrayList<>()
        Deque<String> deque = new ArrayDeque<>(Arrays.asList(args))
        var stop = false
        while (!deque.isEmpty() && !stop) {
            var arg = deque.removeFirst()
            if (arg.startsWith("-")) {
                var argNames = new ArrayList<String>()
                if (arg.startsWith("--")) {
                    if (arg.length() < 3) {
                        throw new FindException("Invalid option: " + arg)
                    }
                    arg = arg.substring(2)
                    if (arg.contains("=")) {
                        var parts = arg.split("=", 2)
                        if (parts.length != 2) {
                            throw new FindException("Invalid option: " + arg)
                        }
                        arg = parts[0]
                        var value = parts[1]
                        deque.addFirst(value)
                    }
                    argNames.add(arg)
                } else if (arg.length() > 1) {
                    arg = arg.substring(1)
                    for (char c : arg.toCharArray()) {
                        var cs = String.valueOf(c)
                        if (boolMap.containsKey(cs)) {
                            argNames.add(boolMap.get(cs))
                        } else if (strMap.containsKey(cs)) {
                            argNames.add(strMap.get(cs))
                        } else if (intMap.containsKey(cs)) {
                            argNames.add(intMap.get(cs))
                        } else if (longMap.containsKey(cs)) {
                            argNames.add(longMap.get(cs))
                        } else {
                            throw new FindException("Invalid option: -" + c)
                        }
                    }
                } else {
                    throw new FindException("Invalid option: " + arg)
                }

                for (String argName : argNames) {
                    if (this.boolMap.containsKey(argName)) {
                        String longName = this.boolMap.get(argName)
                        argTokens.add(new ArgToken(longName, ArgTokenType.BOOL, true))
                        if (longName == "help" || longName == "version") {
                            stop = true
                            break
                        }
                    } else if (this.strMap.containsKey(argName)
                            || this.intMap.containsKey(argName)
                            || this.longMap.containsKey(argName)
                            || argName == "settings-file") {
                        if (!deque.isEmpty()) {
                            String argVal = deque.remove()
                            if (this.strMap.containsKey(argName)) {
                                argTokens.add(new ArgToken(this.strMap.get(argName), ArgTokenType.STR, argVal))
                            } else if (this.intMap.containsKey(argName)) {
                                argTokens.add(new ArgToken(this.intMap.get(argName), ArgTokenType.INT, Integer.parseInt(argVal)))
                            } else if (this.longMap.containsKey(argName)) {
                                argTokens.add(new ArgToken(this.longMap.get(argName), ArgTokenType.LONG, Long.parseLong(argVal)))
                            } else if (argName == "settings-file") {
                                argTokens.add(new ArgToken(argName, ArgTokenType.STR, argVal))
                            } else {
                                throw new FindException("Invalid option: " + arg)
                            }
                        } else {
                            throw new FindException("Missing value for option " + arg)
                        }
                    } else {
                        throw new FindException("Invalid option: " + arg)
                    }
                }
            } else {
                // treat as a path
                var path = 'path'
                argTokens.add(new ArgToken(path, ArgTokenType.STR, arg))
            }
        }
        return argTokens
    }

    List<ArgToken> tokenizeArgMap(final Map<String, Object> argMap) throws FindException {
        List<ArgToken> argTokens = new ArrayList<>()
        var keys = argMap.keySet().stream().sorted().collect(Collectors.toList())
        for (var k : keys) {
            var v = argMap.get(k)
            if (this.boolMap.containsKey(k)) {
                if (v instanceof Boolean) {
                    argTokens.add(new ArgToken(this.boolMap.get(k), ArgTokenType.BOOL, v))
                } else {
                    throw new FindException("Invalid value for option: " + k)
                }
            } else if (this.strMap.containsKey(k)) {
                if (v instanceof String) {
                    argTokens.add(new ArgToken(this.strMap.get(k), ArgTokenType.STR, v))
                } else if (v instanceof List) {
                    for (int i = 0; i < ((List) v).size(); i++) {
                        Object item = ((List) v)[i]
                        if (item instanceof String) {
                            argTokens.add(new ArgToken(this.strMap.get(k), ArgTokenType.STR, item))
                        } else {
                            throw new FindException("Invalid value for option: " + k)
                        }
                    }
                } else {
                    throw new FindException("Invalid value for option: " + k)
                }
            } else if (this.intMap.containsKey(k)) {
                if (v instanceof Integer) {
                    argTokens.add(new ArgToken(this.intMap.get(k), ArgTokenType.INT, v))
                } else if (v instanceof Long) {
                    argTokens.add(new ArgToken(this.intMap.get(k), ArgTokenType.INT, v.intValue()))
                } else {
                    throw new FindException("Invalid value for option: " + k)
                }
            } else if (this.longMap.containsKey(k)) {
                if (v instanceof Long) {
                    argTokens.add(new ArgToken(this.longMap.get(k), ArgTokenType.LONG, v))
                } else if (v instanceof Integer) {
                    argTokens.add(new ArgToken(this.longMap.get(k), ArgTokenType.LONG, v.longValue()))
                } else {
                    throw new FindException("Missing value for option " + k)
                }
            } else {
                throw new FindException("Invalid key in JSON: " + k)
            }
        }
        return argTokens
    }

    List<ArgToken> tokenizeJson(final String json) throws FindException {
        try {
            JsonSlurper jsonSlurper = new JsonSlurper()
            def jsonObj = jsonSlurper.parseText(json)
            assert jsonObj instanceof Map
            return tokenizeArgMap((Map<String, Object>) jsonObj)
        } catch (FindException e) {
            throw e
        } catch (Exception e) {
            throw new FindException("Error parsing JSON settings: ${e.message}")
        }
    }

    List<ArgToken> tokenizeFilePath(final String filePath) throws FindException {
        var path = FileUtil.expandPath(Paths.get(filePath))
        if (!Files.exists(path)) {
            throw new FindException("Settings file not found: " + filePath)
        }
        if (!filePath.endsWith(".json")) {
            throw new FindException("Invalid settings file (must be JSON): " + filePath)
        }
        try {
            return tokenizeJson(FileUtil.getFileContents(path))
        } catch (FileNotFoundException ignored) {
            throw new FindException("Settings file not found: " + filePath)
        } catch (IOException ignored) {
            throw new FindException("IOException reading settings file: " + filePath)
        } catch (Exception e) {
            throw new FindException("Exception: " + e.getMessage())
        }
    }
}
