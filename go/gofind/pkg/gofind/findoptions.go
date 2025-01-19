package gofind

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"strconv"
	"strings"
)

type FindOption struct {
	Short string
	Long  string
	Desc  string
}

type FindOptions struct {
	FindOptions []*FindOption
}

func FindOptionsFromJson() (*FindOptions, error) {
	config := NewFindConfig()
	data, err := os.ReadFile(config.FINDOPTIONSPATH)
	if err != nil {
		return &FindOptions{}, err
	}
	var findOptions FindOptions
	if err = json.Unmarshal(data, &findOptions); err != nil {
		return &FindOptions{}, err
	}

	// TEMPORARY
	//findOptions.generateCodeFile("/Users/cary/src/xfind/go/gofind/pkg/gofind/findoptionsgen.go")

	return &findOptions, nil
}

func NewFindOptions() *FindOptions {
	findOptions, err := FindOptionsFromJson()
	if err != nil {
		// do something
	}
	return findOptions
}

func (fo *FindOptions) isValidOption(opt string) bool {
	if opt == "path" {
		return true
	}
	for _, o := range fo.FindOptions {
		if o.Long == opt || (o.Short != "" && o.Short == opt) {
			return true
		}
	}
	return false
}

func (fo *FindOptions) UpdateSettingsFromJson(data []byte, settings *FindSettings) error {
	boolActionMap := fo.getBoolActionMap()
	stringActionMap := fo.getStringActionMap()
	intActionMap := fo.getIntActionMap()
	longActionMap := fo.getLongActionMap()
	type JsonSettings map[string]interface{}
	var jsonSettings JsonSettings
	if err := json.Unmarshal(data, &jsonSettings); err != nil {
		errMsg := fmt.Sprintf("Unable to parse JSON")
		return fmt.Errorf(errMsg)
	}
	for k := range jsonSettings {
		if !fo.isValidOption(k) {
			return fmt.Errorf(fmt.Sprintf("Invalid option: %v", k))
		}
	}
	for k := range jsonSettings {
		if bf, isBool := boolActionMap[k]; isBool {
			if v, hasVal := jsonSettings[k]; hasVal {
				bf(v.(bool), settings)
			} else {
				errMsg := fmt.Sprintf("Invalid value for option: %v", k)
				return fmt.Errorf(errMsg)
			}
		} else if sf, isString := stringActionMap[k]; isString {
			if v, hasVal := jsonSettings[k]; hasVal {
				switch v := v.(type) {
				case string:
					sf(v, settings)
				case int:
					sf(strconv.Itoa(v), settings)
				case float32, float64:
					sf(fmt.Sprintf("%v", v.(float64)), settings)
				case []interface{}:
					for i := range v {
						sf(v[i].(string), settings)
					}
				default:
					Log(fmt.Sprintf("k: %v", k))
					Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
					errMsg := fmt.Sprintf("Unknown data type in settings file")
					Log(errMsg)
					return fmt.Errorf(errMsg)
				}
			} else {
				errMsg := fmt.Sprintf("Invalid value for option: %v", k)
				return fmt.Errorf(errMsg)
			}
		} else if iff, isInt := intActionMap[k]; isInt {
			if v, hasVal := jsonSettings[k]; hasVal {
				switch v := v.(type) {
				case int:
					iff(v, settings)
				case float32, float64:
					iff(int(v.(float64)), settings)
				default:
					Log(fmt.Sprintf("k: %v", k))
					Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
					errMsg := fmt.Sprintf("Unknown data type in settings file")
					return fmt.Errorf(errMsg)
				}
			} else {
				errMsg := fmt.Sprintf("Invalid value for option: %v", k)
				return fmt.Errorf(errMsg)
			}
		} else if lff, isLong := longActionMap[k]; isLong {
			if v, hasVal := jsonSettings[k]; hasVal {
				switch v := v.(type) {
				case int64:
					lff(v, settings)
				case float32, float64:
					lff(int64(v.(float64)), settings)
				default:
					Log(fmt.Sprintf("k: %v", k))
					Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
					errMsg := fmt.Sprintf("Unknown data type in settings file")
					Log(errMsg)
					return fmt.Errorf(errMsg)
				}
			} else {
				errMsg := fmt.Sprintf("Invalid value for option: %v", k)
				return fmt.Errorf(errMsg)
			}
		} else {
			errMsg := fmt.Sprintf("Invalid option: %v", k)
			return fmt.Errorf(errMsg)
		}
	}
	return nil
}

func (fo *FindOptions) UpdateSettingsFromFile(filePath string, settings *FindSettings) error {
	expandedPath := ExpandPath(filePath)
	if data, err := os.ReadFile(expandedPath); err != nil {
		if strings.HasSuffix(err.Error(), "no such file or directory") {
			errMsg := fmt.Sprintf("Settings file not found: %v", filePath)
			return fmt.Errorf(errMsg)
		}
		return err
	} else {
		if err := fo.UpdateSettingsFromJson(data, settings); err != nil {
			if err.Error() == "Unable to parse JSON" {
				errMsg := fmt.Sprintf("Invalid settings file (must be JSON): %v", filePath)
				return fmt.Errorf(errMsg)
			}
			return err
		}
		return nil
	}
}

func (fo *FindOptions) FindSettingsFromArgs(args []string) (*FindSettings, error) {
	settings := GetDefaultFindSettings()
	// default printFiles to true since running as cli
	settings.SetPrintFiles(true)
	boolActionMap := fo.getBoolActionMap()
	stringActionMap := fo.getStringActionMap()
	intActionMap := fo.getIntActionMap()
	longActionMap := fo.getLongActionMap()

	for i := 0; i < len(args); {
		if strings.HasPrefix(args[i], "-") {
			k := strings.TrimLeft(args[i], "-")
			if fo.isValidOption(k) {
				if bf, isBool := boolActionMap[k]; isBool {
					bf(true, settings)
				} else {
					i++
					if len(args) < i+1 {
						return nil, fmt.Errorf("Missing value for option: %s", k)
					}
					val := args[i]

					if sf, isString := stringActionMap[k]; isString {
						sf(val, settings)
					} else if iff, isInt := intActionMap[k]; isInt {
						intVal, err := strconv.Atoi(val)
						if err != nil {
							return nil, fmt.Errorf("Invalid value for option %s", k)
						}
						iff(intVal, settings)
					} else if lff, isLong := longActionMap[k]; isLong {
						longVal, err := strconv.ParseInt(val, 0, 64)
						if err != nil {
							return nil, fmt.Errorf("Invalid value for option %s", k)
						}
						lff(longVal, settings)
					} else if k == "settings-file" {
						err := fo.UpdateSettingsFromFile(val, settings)
						if err != nil {
							return nil, err
						}
					} else {
						return nil, fmt.Errorf("Invalid option: %s", k)
					}
				}
			} else {
				return nil, fmt.Errorf("Invalid option: %s", k)
			}
		} else {
			settings.AddPath(args[i])
		}
		i++
	}
	if settings.Debug() {
		settings.SetVerbose(true)
	}
	return settings, nil
}

func (fo *FindOptions) getUsageString() string {
	var buffer bytes.Buffer
	buffer.WriteString("\nUsage:\n")
	buffer.WriteString(" gofind [options] <path> [<path> ...]\n\nOptions:\n")
	sortKeyMap := fo.getSortKeyMap()
	optStringMap := fo.getOptStringMap()
	optDescMap := fo.getOptDescMap()
	sortedKeys := GetSortedKeys(sortKeyMap)
	optStrings := GetMapValues(optStringMap)
	longestLen := GetLongestLen(optStrings)
	optFormat := fmt.Sprintf(" %%-%ds  %%s\n", longestLen)
	for _, k := range sortedKeys {
		o := optStringMap[sortKeyMap[k]]
		d := optDescMap[sortKeyMap[k]]
		buffer.WriteString(fmt.Sprintf(optFormat, o, d))
	}
	return buffer.String()
}

func (fo *FindOptions) PrintUsage() {
	Log(fo.getUsageString())
	os.Exit(0)
}

func (fo *FindOptions) PrintVersion() {
	config := NewFindConfig()
	Log(fmt.Sprintf("xfind version %s", config.VERSION))
	os.Exit(0)
}

func (fo *FindOptions) getSortKeyMap() map[string]string {
	m := map[string]string{}
	for _, o := range fo.FindOptions {
		sortKey := ""
		if o.Short == "" {
			sortKey = strings.ToLower(o.Long)
		} else {
			sortKey = fmt.Sprintf("%s@%s", strings.ToLower(o.Short),
				strings.ToLower(o.Long))
		}
		m[sortKey] = o.Long
	}
	return m
}

func (fo *FindOptions) getOptStringMap() map[string]string {
	m := map[string]string{}
	for _, o := range fo.FindOptions {
		optString := ""
		if o.Short != "" {
			optString = fmt.Sprintf("-%s,", o.Short)
		}
		optString = fmt.Sprintf("%s--%s", optString, o.Long)
		m[o.Long] = optString
	}
	return m
}

func (fo *FindOptions) getOptDescMap() map[string]string {
	m := map[string]string{}
	for _, o := range fo.FindOptions {
		m[o.Long] = o.Desc
	}
	return m
}

type boolAction func(b bool, settings *FindSettings)

func (fo *FindOptions) getBoolActionMap() map[string]boolAction {
	m := map[string]boolAction{
		"archivesonly": func(b bool, settings *FindSettings) {
			settings.SetArchivesOnly(b)
		},
		"debug": func(b bool, settings *FindSettings) {
			settings.SetDebug(b)
		},
		"excludearchives": func(b bool, settings *FindSettings) {
			settings.SetIncludeArchives(!b)
		},
		"excludehidden": func(b bool, settings *FindSettings) {
			settings.SetIncludeHidden(!b)
		},
		"followsymlinks": func(b bool, settings *FindSettings) {
			settings.SetFollowSymlinks(b)
		},
		"help": func(b bool, settings *FindSettings) {
			settings.SetPrintUsage(b)
		},
		"includearchives": func(b bool, settings *FindSettings) {
			settings.SetIncludeArchives(b)
		},
		"includehidden": func(b bool, settings *FindSettings) {
			settings.SetIncludeHidden(b)
		},
		"nofollowsymlinks": func(b bool, settings *FindSettings) {
			settings.SetFollowSymlinks(!b)
		},
		"noprintdirs": func(b bool, settings *FindSettings) {
			settings.SetPrintDirs(!b)
		},
		"noprintfiles": func(b bool, settings *FindSettings) {
			settings.SetPrintFiles(!b)
		},
		"norecursive": func(b bool, settings *FindSettings) {
			settings.SetRecursive(!b)
		},
		"printdirs": func(b bool, settings *FindSettings) {
			settings.SetPrintDirs(b)
		},
		"printfiles": func(b bool, settings *FindSettings) {
			settings.SetPrintFiles(b)
		},
		"recursive": func(b bool, settings *FindSettings) {
			settings.SetRecursive(b)
		},
		"sort-ascending": func(b bool, settings *FindSettings) {
			settings.SetSortDescending(!b)
		},
		"sort-caseinsensitive": func(b bool, settings *FindSettings) {
			settings.SetSortCaseInsensitive(b)
		},
		"sort-casesensitive": func(b bool, settings *FindSettings) {
			settings.SetSortCaseInsensitive(!b)
		},
		"sort-descending": func(b bool, settings *FindSettings) {
			settings.SetSortDescending(b)
		},
		"verbose": func(b bool, settings *FindSettings) {
			settings.SetVerbose(b)
		},
		"version": func(b bool, settings *FindSettings) {
			settings.SetPrintVersion(b)
		},
	}
	for _, o := range fo.FindOptions {
		if o.Short != "" {
			if f, ok := m[o.Long]; ok {
				m[o.Short] = f
			}
		}
	}
	return m
}

type stringAction func(s string, settings *FindSettings)

func (fo *FindOptions) getStringActionMap() map[string]stringAction {
	m := map[string]stringAction{
		"in-archiveext": func(s string, settings *FindSettings) {
			settings.AddInArchiveExtension(s)
		},
		"in-archivefilepattern": func(s string, settings *FindSettings) {
			settings.AddInArchiveFilePattern(s)
		},
		"in-dirpattern": func(s string, settings *FindSettings) {
			settings.AddInDirPattern(s)
		},
		"in-ext": func(s string, settings *FindSettings) {
			settings.AddInExtension(s)
		},
		"in-filepattern": func(s string, settings *FindSettings) {
			settings.AddInFilePattern(s)
		},
		"in-filetype": func(s string, settings *FindSettings) {
			settings.AddInFileType(GetFileTypeForName(s))
		},
		"maxlastmod": func(s string, settings *FindSettings) {
			settings.SetMaxLastModFromString(s)
		},
		"minlastmod": func(s string, settings *FindSettings) {
			settings.SetMinLastModFromString(s)
		},
		"out-archiveext": func(s string, settings *FindSettings) {
			settings.AddOutArchiveExtension(s)
		},
		"out-archivefilepattern": func(s string, settings *FindSettings) {
			settings.AddOutArchiveFilePattern(s)
		},
		"out-dirpattern": func(s string, settings *FindSettings) {
			settings.AddOutDirPattern(s)
		},
		"out-ext": func(s string, settings *FindSettings) {
			settings.AddOutExtension(s)
		},
		"out-filepattern": func(s string, settings *FindSettings) {
			settings.AddOutFilePattern(s)
		},
		"out-filetype": func(s string, settings *FindSettings) {
			settings.AddOutFileType(GetFileTypeForName(s))
		},
		"path": func(s string, settings *FindSettings) {
			settings.AddPath(s)
		},
		"sort-by": func(s string, settings *FindSettings) {
			settings.SetSortByFromString(s)
		},
	}
	for _, o := range fo.FindOptions {
		if o.Short != "" {
			if f, ok := m[o.Long]; ok {
				m[o.Short] = f
			}
		}
	}
	return m
}

type intAction func(i int, settings *FindSettings)

func (fo *FindOptions) getIntActionMap() map[string]intAction {
	m := map[string]intAction{
		"maxdepth": func(i int, settings *FindSettings) {
			settings.SetMaxDepth(i)
		},
		"mindepth": func(i int, settings *FindSettings) {
			settings.SetMinDepth(i)
		},
	}
	return m
}

type longAction func(l int64, settings *FindSettings)

func (fo *FindOptions) getLongActionMap() map[string]longAction {
	m := map[string]longAction{
		"maxsize": func(l int64, settings *FindSettings) {
			settings.SetMaxSize(l)
		},
		"minsize": func(l int64, settings *FindSettings) {
			settings.SetMinSize(l)
		},
	}
	return m
}

func (fo *FindOptions) generateCodeFile(filePath string) {
	var buffer bytes.Buffer
	depth := 0
	buffer.WriteString("package gofind\n\n")
	buffer.WriteString("func GetFindOptions() *FindOptions {\n")
	depth++
	buffer.WriteString(fmt.Sprintf("%sreturn &FindOptions{\n", strings.Repeat("\t", depth)))
	depth++
	buffer.WriteString(fmt.Sprintf("%s[]*FindOption{\n", strings.Repeat("\t", depth)))
	depth++
	for _, so := range fo.FindOptions {
		buffer.WriteString(fmt.Sprintf("%s{\"%s\", \"%s\", \"%s\"},\n",
			strings.Repeat("\t", depth), so.Short, so.Long, EscapeQuotes(so.Desc)))
	}
	depth--
	buffer.WriteString(fmt.Sprintf("%s},\n", strings.Repeat("\t", depth)))
	depth--
	buffer.WriteString(fmt.Sprintf("%s}\n}\n", strings.Repeat("\t", depth)))
	os.WriteFile(filePath, buffer.Bytes(), 0644)
}
