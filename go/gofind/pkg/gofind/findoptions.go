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

func (fo *FindOptions) SettingsFromFile(filePath string, settings *FindSettings) error {
	if data, err := os.ReadFile(filePath); err != nil {
		return err
	} else {
		return fo.SettingsFromJson(data, settings)
	}
}

func (fo *FindOptions) SettingsFromJson(data []byte, settings *FindSettings) error {
	argActionMap := fo.getArgActionMap()
	boolFlagActionMap := fo.getBoolFlagActionMap()
	type JsonSettings map[string]interface{}
	var jsonSettings JsonSettings
	if err := json.Unmarshal(data, &jsonSettings); err != nil {
		return err
	}
	for k := range jsonSettings {
		if af, isAction := argActionMap[k]; isAction {
			if v, hasVal := jsonSettings[k]; hasVal {
				switch v := v.(type) {
				case string:
					af(v, settings)
				case int:
					af(strconv.Itoa(v), settings)
				case float32, float64:
					af(fmt.Sprintf("%v", v.(float64)), settings)
				case []interface{}:
					for i := range v {
						af(v[i].(string), settings)
					}
				default:
					Log(fmt.Sprintf("k: %v", k))
					Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
					errMsg := fmt.Sprintf("Unknown data type in settings file")
					Log(errMsg)
					return fmt.Errorf(errMsg)
				}
			} else {
				Log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if ff, isFlag := boolFlagActionMap[k]; isFlag {
			if v, hasVal := jsonSettings[k]; hasVal {
				ff(v.(bool), settings)
			} else {
				Log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if k == "path" {
			if sp, hasStartPath := jsonSettings[k]; hasStartPath {
				settings.AddPath(sp.(string))
			} else {
				Log("path value is invalid")
			}
		} else {
			return fmt.Errorf("Invalid option: %s", k)
		}
	}
	return nil
}

func (fo *FindOptions) FindSettingsFromArgs(args []string) (*FindSettings, error) {
	settings := GetDefaultFindSettings()
	// default printFiles to true since running as cli
	settings.SetPrintFiles(true)
	argActionMap := fo.getArgActionMap()
	flagActionMap := fo.getBoolFlagActionMap()

	if false {
		Log(fmt.Sprintf("argActionMap: %v", argActionMap))
		Log(fmt.Sprintf("flagActionMap: %v", flagActionMap))
	}

	for i := 0; i < len(args); {
		if strings.HasPrefix(args[i], "-") {
			k := strings.TrimLeft(args[i], "-")
			if false {
				Log(fmt.Sprintf("k: %s\n", k))
			}
			if af, isAction := argActionMap[k]; isAction {
				i++
				if len(args) < i+1 {
					return nil, fmt.Errorf("Missing value for option: %s", k)
				}
				val := args[i]
				af(val, settings)
			} else if ff, isFlag := flagActionMap[k]; isFlag {
				ff(true, settings)
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

type argAction func(s string, settings *FindSettings)

func (fo *FindOptions) getArgActionMap() map[string]argAction {
	m := map[string]argAction{
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
		"maxdepth": func(s string, settings *FindSettings) {
			settings.SetMaxDepthFromString(s)
		},
		"maxlastmod": func(s string, settings *FindSettings) {
			settings.SetMaxLastModFromString(s)
		},
		"maxsize": func(s string, settings *FindSettings) {
			settings.SetMaxSizeFromString(s)
		},
		"mindepth": func(s string, settings *FindSettings) {
			settings.SetMinDepthFromString(s)
		},
		"minlastmod": func(s string, settings *FindSettings) {
			settings.SetMinLastModFromString(s)
		},
		"minsize": func(s string, settings *FindSettings) {
			settings.SetMinSizeFromString(s)
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
		"settings-file": func(s string, settings *FindSettings) {
			fo.SettingsFromFile(s, settings)
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

type boolFlagAction func(b bool, settings *FindSettings)

func (fo *FindOptions) getBoolFlagActionMap() map[string]boolFlagAction {
	m := map[string]boolFlagAction{
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
		"help": func(b bool, settings *FindSettings) {
			settings.SetPrintUsage(b)
		},
		"includearchives": func(b bool, settings *FindSettings) {
			settings.SetIncludeArchives(b)
		},
		"includehidden": func(b bool, settings *FindSettings) {
			settings.SetIncludeHidden(b)
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
