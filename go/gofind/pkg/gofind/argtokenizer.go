package gofind

import (
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"strconv"
	"strings"
)

type ArgTokenType int

const (
	ArgTokenTypeUnknown ArgTokenType = iota
	ArgTokenTypeBool    ArgTokenType = iota
	ArgTokenTypeString  ArgTokenType = iota
	ArgTokenTypeInt     ArgTokenType = iota
	ArgTokenTypeLong    ArgTokenType = iota
)

type ArgToken struct {
	Name  string
	Type  ArgTokenType
	Value interface{}
}

type ArgTokenizer struct {
	BoolMap   map[string]string
	StringMap map[string]string
	IntMap    map[string]string
	LongMap   map[string]string
}

// TODO: change FindOption to Option interface
func NewArgTokenizer(options []*FindOption) *ArgTokenizer {
	boolMap := make(map[string]string)
	stringMap := make(map[string]string)
	stringMap["path"] = "path"
	intMap := make(map[string]string)
	longMap := make(map[string]string)

	for _, o := range options {
		if o.ArgType == ArgTokenTypeBool {
			boolMap[o.Long] = o.Long
			if o.Short != "" {
				boolMap[o.Short] = o.Long
			}
		} else if o.ArgType == ArgTokenTypeString {
			stringMap[o.Long] = o.Long
			if o.Short != "" {
				stringMap[o.Short] = o.Long
			}
		} else if o.ArgType == ArgTokenTypeInt {
			intMap[o.Long] = o.Long
			if o.Short != "" {
				intMap[o.Short] = o.Long
			}
		} else if o.ArgType == ArgTokenTypeLong {
			longMap[o.Long] = o.Long
			if o.Short != "" {
				longMap[o.Short] = o.Long
			}
		}
	}
	return &ArgTokenizer{
		boolMap,
		stringMap,
		intMap,
		longMap,
	}
}

func (at *ArgTokenizer) TokenizeArgs(args []string) ([]*ArgToken, error) {
	var argTokens []*ArgToken

	for i := 0; i < len(args); {
		if strings.HasPrefix(args[i], "-") {
			argNames := []string{}
			argVal := ""
			if strings.HasPrefix(args[i], "--") && len(args[i]) > 2 {
				// Process long arg
				longArg := strings.TrimLeft(args[i], "-")
				if strings.Index(longArg, "=") > -1 {
					parts := strings.Split(longArg, "=")
					if len(parts) > 0 {
						longArg = parts[0]
					}
					if len(parts) > 1 {
						argVal = parts[1]
					}
				}
				argNames = append(argNames, longArg)
			} else if len(args[i]) > 1 {
				// Process short arg(s)
				shortArgs := strings.TrimLeft(args[i], "-")
				for _, c := range shortArgs {
					cs := string(c)
					if bn, isBool := at.BoolMap[cs]; isBool {
						argNames = append(argNames, bn)
					} else if sn, isString := at.StringMap[cs]; isString {
						argNames = append(argNames, sn)
					} else if in, isInt := at.IntMap[cs]; isInt {
						argNames = append(argNames, in)
					} else if ln, isLong := at.LongMap[cs]; isLong {
						argNames = append(argNames, ln)
					} else {
						return nil, fmt.Errorf("Invalid option: %s", cs)
					}
				}
			} else {
				return nil, fmt.Errorf("Invalid option: %s", args[i])
			}

			for _, argName := range argNames {
				if _, isBool := at.BoolMap[argName]; isBool {
					argTokens = append(argTokens, &ArgToken{argName, ArgTokenTypeBool, true})
				} else {
					if argVal == "" {
						i++
						if len(args) < i+1 {
							return nil, fmt.Errorf("Missing value for option: %s", argName)
						}
						argVal = args[i]
					}

					if _, isString := at.StringMap[argName]; isString {
						argTokens = append(argTokens, &ArgToken{argName, ArgTokenTypeString, argVal})
					} else if _, isInt := at.IntMap[argName]; isInt {
						intVal, err := strconv.Atoi(argVal)
						if err != nil {
							return nil, fmt.Errorf("Invalid value for option %s", argName)
						}
						argTokens = append(argTokens, &ArgToken{argName, ArgTokenTypeInt, intVal})
					} else if _, isLong := at.LongMap[argName]; isLong {
						longVal, err := strconv.ParseInt(argVal, 0, 64)
						if err != nil {
							return nil, fmt.Errorf("Invalid value for option %s", argName)
						}
						argTokens = append(argTokens, &ArgToken{argName, ArgTokenTypeLong, longVal})
					} else if argName == "settings-file" {
						argTokens = append(argTokens, &ArgToken{argName, ArgTokenTypeString, argVal})
					} else {
						return nil, fmt.Errorf("Invalid option: %s", argName)
					}
				}
			}
		} else {
			argTokens = append(argTokens, &ArgToken{"path", ArgTokenTypeString, args[i]})
		}
		i++
	}
	return argTokens, nil
}

type ArgMap map[string]interface{}

func (at *ArgTokenizer) tokenizeArgMap(argMap ArgMap) ([]*ArgToken, error) {
	var argTokens []*ArgToken
	for k := range argMap {
		v := argMap[k]
		if bName, isBool := at.BoolMap[k]; isBool {
			switch v := v.(type) {
			case bool:
				argTokens = append(argTokens, &ArgToken{bName, ArgTokenTypeBool, v})
			default:
				return nil, fmt.Errorf("Invalid value for option: %v", k)
			}
		} else if sName, isString := at.StringMap[k]; isString {
			switch v := v.(type) {
			case string:
				argTokens = append(argTokens, &ArgToken{sName, ArgTokenTypeString, v})
			case []string:
				for i := range v {
					argTokens = append(argTokens, &ArgToken{sName, ArgTokenTypeString, v[i]})
				}
			case []interface{}:
				for i := range v {
					argTokens = append(argTokens, &ArgToken{sName, ArgTokenTypeString, v[i].(string)})
				}
			default:
				Log(fmt.Sprintf("k: %v", k))
				Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
				const errMsg = "Unknown data type in ArgMap"
				Log(errMsg)
				return nil, fmt.Errorf("Invalid value for option: %v", k)
			}
		} else if iName, isInt := at.IntMap[k]; isInt {
			switch v := v.(type) {
			case int:
				argTokens = append(argTokens, &ArgToken{iName, ArgTokenTypeInt, v})
			case float32, float64:
				argTokens = append(argTokens, &ArgToken{iName, ArgTokenTypeInt, int(v.(float64))})
			default:
				Log(fmt.Sprintf("k: %v", k))
				Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
				return nil, fmt.Errorf("Invalid value for option: %v", k)
			}
		} else if lName, isLong := at.LongMap[k]; isLong {
			switch v := v.(type) {
			case int64:
				argTokens = append(argTokens, &ArgToken{lName, ArgTokenTypeLong, v})
			case float32, float64:
				argTokens = append(argTokens, &ArgToken{lName, ArgTokenTypeLong, int64(v.(float64))})
			default:
				Log(fmt.Sprintf("k: %v", k))
				Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
				const errMsg = "Unknown data type in ArgMap"
				Log(errMsg)
				return nil, fmt.Errorf("Invalid value for option: %v", k)
			}
		} else if k == "settings-file" {
			switch v := v.(type) {
			case string:
				argTokens = append(argTokens, &ArgToken{sName, ArgTokenTypeString, v})
			case []string:
				for i := range v {
					argTokens = append(argTokens, &ArgToken{sName, ArgTokenTypeString, v[i]})
				}
			case []interface{}:
				for i := range v {
					argTokens = append(argTokens, &ArgToken{sName, ArgTokenTypeString, v[i].(string)})
				}
			default:
				Log(fmt.Sprintf("k: %v", k))
				Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
				const errMsg = "Unknown data type in ArgMap"
				Log(errMsg)
				return nil, fmt.Errorf("Invalid value for option: %v", k)
			}
		} else {
			return nil, fmt.Errorf("Invalid option: %v", k)
		}
	}
	return argTokens, nil
}

func (at *ArgTokenizer) tokenizeJsonByteArray(data []byte) ([]*ArgToken, error) {
	var argMap ArgMap
	if err := json.Unmarshal(data, &argMap); err != nil {
		return nil, fmt.Errorf("Unable to parse JSON")
	}
	return at.tokenizeArgMap(argMap)
}

func (at *ArgTokenizer) TokenizeJson(jsonString string) ([]*ArgToken, error) {
	return at.tokenizeJsonByteArray([]byte(jsonString))
}

func (at *ArgTokenizer) TokenizeFile(filePath string) ([]*ArgToken, error) {
	expandedPath := ExpandPath(filePath)
	if data, err := os.ReadFile(expandedPath); err != nil {
		if strings.HasSuffix(err.Error(), "no such file or directory") {
			return nil, fmt.Errorf("Settings file not found: %v", filePath)
		}
		return nil, err
	} else {
		argTokens, err := at.tokenizeJsonByteArray(data)
		if err != nil {
			if err.Error() == "Unable to parse JSON" {
				return nil, fmt.Errorf("Invalid JSON settings file: %v", filePath)
			}
			return nil, err
		}
		return argTokens, nil
	}
}
