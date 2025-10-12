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

public class ArgTokenizer {
    private final Map<String, String> boolMap;
    private final Map<String, String> strMap;
    private final Map<String, String> intMap;
    private final Map<String, String> longMap;

    public ArgTokenizer(List<? extends Option> options) {
        this.boolMap = new HashMap<>();
        this.strMap = new HashMap<>();
        this.intMap = new HashMap<>();
        this.longMap = new HashMap<>();
        for (Option option : options) {
            if (option.argType() == ArgTokenType.BOOL) {
                this.boolMap.put(option.longArg(), option.longArg());
                if (option.shortArg() != null && !option.shortArg().isEmpty()) {
                    this.boolMap.put(option.shortArg(), option.longArg());
                }
            } else if (option.argType() == ArgTokenType.STR) {
                this.strMap.put(option.longArg(), option.longArg());
                if (option.shortArg() != null && !option.shortArg().isEmpty()) {
                    this.strMap.put(option.shortArg(), option.longArg());
                }
            } else if (option.argType() == ArgTokenType.INT) {
                this.intMap.put(option.longArg(), option.longArg());
                if (option.shortArg() != null && !option.shortArg().isEmpty()) {
                    this.intMap.put(option.shortArg(), option.longArg());
                }
            } else if (option.argType() == ArgTokenType.LONG) {
                this.longMap.put(option.longArg(), option.longArg());
                if (option.shortArg() != null && !option.shortArg().isEmpty()) {
                    this.longMap.put(option.shortArg(), option.longArg());
                }
            }
        }
    }

    public List<ArgToken> tokenizeArgs(final String[] args) throws FindException {
        List<ArgToken> argTokens = new ArrayList<>();
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
                    argNames.add(arg);
                } else if (arg.length() > 1) {
                    arg = arg.substring(1);
                    for (char c : arg.toCharArray()) {
                        var cs = String.valueOf(c);
                        if (boolMap.containsKey(cs)) {
                            argNames.add(boolMap.get(cs));
                        } else if (strMap.containsKey(cs)) {
                            argNames.add(strMap.get(cs));
                        } else if (intMap.containsKey(cs)) {
                            argNames.add(intMap.get(cs));
                        } else if (longMap.containsKey(cs)) {
                            argNames.add(longMap.get(cs));
                        } else {
                            throw new FindException("Invalid option: " + cs);
                        }
                    }
                } else {
                    throw new FindException("Invalid option: " + arg);
                }

                for (String argName : argNames) {
                    if (this.boolMap.containsKey(argName)) {
                        argTokens.add(new ArgToken(argName, ArgTokenType.BOOL, true));
                        if (argName.equals("help") || argName.equals("version")) {
                            stop = true;
                            break;
                        }
                    } else if (this.strMap.containsKey(argName)
                            || this.intMap.containsKey(argName)
                            || this.longMap.containsKey(argName)
                            || argName.equals("settings-file")) {
                        if (!deque.isEmpty()) {
                            String argVal = deque.remove();
                            if (this.strMap.containsKey(argName)) {
                                argTokens.add(new ArgToken(argName, ArgTokenType.STR, argVal));
                            } else if (this.intMap.containsKey(argName)) {
                                argTokens.add(new ArgToken(argName, ArgTokenType.INT, Integer.parseInt(argVal)));
                            } else if (this.longMap.containsKey(argName)) {
                                argTokens.add(new ArgToken(argName, ArgTokenType.LONG, Long.parseLong(argVal)));
                            } else if (argName.equals("settings-file")) {
                                argTokens.add(new ArgToken(argName, ArgTokenType.STR, argVal));
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
                argTokens.add(new ArgToken(path, ArgTokenType.STR, arg));
            }
        }
        return argTokens;
    }

    public List<ArgToken> tokenizeJson(final String json) throws FindException {
        List<ArgToken> argTokens = new ArrayList<>();
        try {
            var jsonObj = new JSONObject(new JSONTokener(json));
            // keys are sorted so that output is consistent across all versions
            var keys = jsonObj.keySet().stream().sorted().toList();
            for (var k : keys) {
                var v = jsonObj.get(k);
                if (this.boolMap.containsKey(k)) {
                    if (v instanceof Boolean b) {
                        argTokens.add(new ArgToken(k, ArgTokenType.BOOL, b));
                    } else {
                        throw new FindException("Missing value for option " + k);
                    }
                } else if (this.strMap.containsKey(k)) {
                    if (v instanceof String s) {
                        argTokens.add(new ArgToken(k, ArgTokenType.STR, s));
                    } else if (v instanceof JSONArray jsonArray) {
                        for (var i = 0; i < jsonArray.length(); i++) {
                            Object item = jsonArray.get(i);
                            if (item instanceof String s) {
                                argTokens.add(new ArgToken(k, ArgTokenType.STR, s));
                            } else {
                                throw new FindException("Invalid value for option: " + k);
                            }
                        }
                    } else {
                        throw new FindException("Missing value for option " + k);
                    }
                } else if (this.intMap.containsKey(k)) {
                    if (v instanceof Integer i) {
                        argTokens.add(new ArgToken(k, ArgTokenType.INT, i));
                    } else if (v instanceof Long l) {
                        argTokens.add(new ArgToken(k, ArgTokenType.INT, l.intValue()));
                    } else {
                        throw new FindException("Missing value for option " + k);
                    }
                } else if (this.longMap.containsKey(k)) {
                    if (v instanceof Long l) {
                        argTokens.add(new ArgToken(k, ArgTokenType.LONG, l));
                    } else if (v instanceof Integer i) {
                        argTokens.add(new ArgToken(k, ArgTokenType.LONG, i.longValue()));
                    } else {
                        throw new FindException("Missing value for option " + k);
                    }
                } else {
                    throw new FindException("Invalid key in JSON: " + k);
                }
            }
        } catch (JSONException e) {
            throw new FindException("Invalid JSON format: " + e.getMessage());
        } catch (ClassCastException e) {
            throw new FindException("Invalid value type in JSON: " + e.getMessage());
        }
        return argTokens;
    }

    public List<ArgToken> tokenizeFilePath(final String filePath) throws FindException {
        var path = FileUtil.expandPath(Paths.get(filePath));
        if (!Files.exists(path)) {
            throw new FindException("Settings file not found: " + filePath);
        }
        if (!filePath.endsWith(".json")) {
            throw new FindException("Invalid settings file (must be JSON): " + filePath);
        }
        try {
            return tokenizeJson(FileUtil.getFileContents(path));
        } catch (FileNotFoundException e) {
            throw new FindException("Settings file not found: " + filePath);
        } catch (IOException e) {
            throw new FindException("IOException reading settings file: " + filePath);
        } catch (Exception e) {
            throw new FindException("Exception: " + e.getMessage());
        }
    }
}
