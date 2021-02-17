package gofind

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
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
	data, err := ioutil.ReadFile(FINDOPTIONSPATH)
	if err != nil {
		return &FindOptions{}, err
	}
	var findOptions FindOptions
	if err = json.Unmarshal(data, &findOptions); err != nil {
		return &FindOptions{}, err
	}
	return &findOptions, nil
}

func NewFindOptions() *FindOptions {
	findOptions, err := FindOptionsFromJson()
	if err != nil {
		// do something
	}
	return findOptions
}

func (so *FindOptions) SettingsFromFile(filepath string, settings *FindSettings) error {
	if data, err := ioutil.ReadFile(filepath); err != nil {
		return err
	} else {
		return so.SettingsFromJson(data, settings)
	}
}

func (so *FindOptions) SettingsFromJson(data []byte, settings *FindSettings) error {
	argActionMap := so.getArgActionMap()
	boolFlagActionMap := so.getBoolFlagActionMap()
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
					log(fmt.Sprintf("k: %v", k))
					log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
					errMsg := fmt.Sprintf("Unknown data type in settings file")
					log(errMsg)
					return fmt.Errorf(errMsg)
				}
			} else {
				log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if ff, isFlag := boolFlagActionMap[k]; isFlag {
			if v, hasVal := jsonSettings[k]; hasVal {
				ff(v.(bool), settings)
			} else {
				log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if k == "startpath" {
			if sp, hasStartPath := jsonSettings[k]; hasStartPath {
				settings.StartPath = sp.(string)
			} else {
				log("startpath value is invalid")
			}
		} else {
			return fmt.Errorf("Invalid option: %s", k)
		}

	}
	return nil
}

func (so *FindOptions) FindSettingsFromArgs(args []string) (*FindSettings, error) {
	settings := GetDefaultFindSettings()
	argActionMap := so.getArgActionMap()
	flagActionMap := so.getBoolFlagActionMap()

	if false {
		log(fmt.Sprintf("argActionMap: %v", argActionMap))
		log(fmt.Sprintf("flagActionMap: %v", flagActionMap))
	}

	for i := 0; i < len(args); {
		if strings.HasPrefix(args[i], "-") {
			k := strings.TrimLeft(args[i], "-")
			if false {
				log(fmt.Sprintf("k: %s\n", k))
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
			settings.StartPath = args[i]
		}
		i++
	}
	if settings.Debug {
		settings.Verbose = true
	}
	return settings, nil
}

func (so *FindOptions) getUsageString() string {
	var buffer bytes.Buffer
	buffer.WriteString("\nUsage:\n")
	buffer.WriteString(" gofind [options] -s <findpattern> <startpath>\n\nOptions:\n")
	sortKeyMap := so.getSortKeyMap()
	optStringMap := so.getOptStringMap()
	optDescMap := so.getOptDescMap()
	sortedKeys := getSortedKeys(sortKeyMap)
	optStrings := getMapValues(optStringMap)
	longestLen := getLongestLen(optStrings)
	optFormat := fmt.Sprintf(" %%-%ds  %%s\n", longestLen)
	for _, k := range sortedKeys {
		o := optStringMap[sortKeyMap[k]]
		d := optDescMap[sortKeyMap[k]]
		buffer.WriteString(fmt.Sprintf(optFormat, o, d))
	}
	return buffer.String()
}

func (so *FindOptions) PrintUsage() {
	log(so.getUsageString())
	os.Exit(0)
}

func (so *FindOptions) PrintVersion() {
	log(fmt.Sprintf("xfind version %s", VERSION))
	os.Exit(0)
}

func (so *FindOptions) getSortKeyMap() map[string]string {
	m := map[string]string{}
	for _, o := range so.FindOptions {
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

func (so *FindOptions) getOptStringMap() map[string]string {
	m := map[string]string{}
	for _, o := range so.FindOptions {
		optString := ""
		if o.Short != "" {
			optString = fmt.Sprintf("-%s,", o.Short)
		}
		optString = fmt.Sprintf("%s--%s", optString, o.Long)
		m[o.Long] = optString
	}
	return m
}

func (so *FindOptions) getOptDescMap() map[string]string {
	m := map[string]string{}
	for _, o := range so.FindOptions {
		m[o.Long] = o.Desc
	}
	return m
}

type argAction func(s string, settings *FindSettings)

func (so *FindOptions) getArgActionMap() map[string]argAction {
	m := map[string]argAction{
		"encoding": func(s string, settings *FindSettings) {
			settings.TextFileEncoding = s
		},
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
			settings.AddInFileType(getFileTypeForName(s))
		},
		"in-linesafterpattern": func(s string, settings *FindSettings) {
			settings.AddInLinesAfterPattern(s)
		},
		"in-linesbeforepattern": func(s string, settings *FindSettings) {
			settings.AddInLinesBeforePattern(s)
		},
		"linesafter": func(s string, settings *FindSettings) {
			num, err := strconv.Atoi(s)
			if err == nil {
				settings.LinesAfter = num
			} else {
				log(fmt.Sprintf("Invalid value for linesafter: %s\n", s))
			}
		},
		"linesaftertopattern": func(s string, settings *FindSettings) {
			settings.AddLinesAfterToPattern(s)
		},
		"linesafteruntilpattern": func(s string, settings *FindSettings) {
			settings.AddLinesAfterUntilPattern(s)
		},
		"linesbefore": func(s string, settings *FindSettings) {
			num, err := strconv.Atoi(s)
			if err == nil {
				settings.LinesBefore = num
			} else {
				log(fmt.Sprintf("Invalid value for linesbefore: %s\n", s))
			}
		},
		"maxlinelength": func(s string, settings *FindSettings) {
			num, err := strconv.Atoi(s)
			if err == nil {
				settings.MaxLineLength = num
			} else {
				log(fmt.Sprintf("Invalid value for maxlinelength: %s\n", s))
			}
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
			settings.AddOutFileType(getFileTypeForName(s))
		},
		"out-linesafterpattern": func(s string, settings *FindSettings) {
			settings.AddOutLinesAfterPattern(s)
		},
		"out-linesbeforepattern": func(s string, settings *FindSettings) {
			settings.AddOutLinesBeforePattern(s)
		},
		"findpattern": func(s string, settings *FindSettings) {
			settings.AddFindPattern(s)
		},
		"settings-file": func(s string, settings *FindSettings) {
			so.SettingsFromFile(s, settings)
		},
	}
	for _, o := range so.FindOptions {
		if o.Short != "" {
			if f, ok := m[o.Long]; ok {
				m[o.Short] = f
			}
		}
	}
	return m
}

type boolFlagAction func(b bool, settings *FindSettings)

func (so *FindOptions) getBoolFlagActionMap() map[string]boolFlagAction {
	m := map[string]boolFlagAction{
		"allmatches": func(b bool, settings *FindSettings) {
			settings.FirstMatch = !b
		},
		"archivesonly": func(b bool, settings *FindSettings) {
			settings.SetArchivesOnly(b)
		},
		"debug": func(b bool, settings *FindSettings) {
			settings.SetDebug(b)
		},
		"excludehidden": func(b bool, settings *FindSettings) {
			settings.ExcludeHidden = b
		},
		"firstmatch": func(b bool, settings *FindSettings) {
			settings.FirstMatch = b
		},
		"help": func(b bool, settings *FindSettings) {
			settings.PrintUsage = b
		},
		"includehidden": func(b bool, settings *FindSettings) {
			settings.ExcludeHidden = !b
		},
		"listdirs": func(b bool, settings *FindSettings) {
			settings.ListDirs = b
		},
		"listfiles": func(b bool, settings *FindSettings) {
			settings.ListFiles = b
		},
		"listlines": func(b bool, settings *FindSettings) {
			settings.ListLines = b
		},
		"multilineoption-REMOVE": func(b bool, settings *FindSettings) {
			settings.MultiLineFind = b
		},
		"noprintmatches": func(b bool, settings *FindSettings) {
			settings.PrintResults = !b
		},
		"norecursive": func(b bool, settings *FindSettings) {
			settings.Recursive = !b
		},
		"nofindarchives": func(b bool, settings *FindSettings) {
			settings.FindArchives = !b
		},
		"printmatches": func(b bool, settings *FindSettings) {
			settings.PrintResults = b
		},
		"recursive": func(b bool, settings *FindSettings) {
			settings.Recursive = b
		},
		"findarchives": func(b bool, settings *FindSettings) {
			settings.FindArchives = b
		},
		"uniquelines": func(b bool, settings *FindSettings) {
			settings.UniqueLines = b
		},
		"verbose": func(b bool, settings *FindSettings) {
			settings.Verbose = b
		},
		"version": func(b bool, settings *FindSettings) {
			settings.PrintVersion = b
		},
	}
	for _, o := range so.FindOptions {
		if o.Short != "" {
			if f, ok := m[o.Long]; ok {
				m[o.Short] = f
			}
		}
	}
	return m
}

type XmlFindOptions struct {
	XmlFindOptions []XmlFindOption `xml:"findoption"`
}

type XmlFindOption struct {
	Short string `xml:"short,attr"`
	Long  string `xml:"long,attr"`
	Desc  string `xml:",chardata"`
}

func findOptionsFromXml() (*FindOptions, error) {
	findOptionsXmlPath := fmt.Sprintf("%s/shared/findoptions.xml", XFINDPATH)
	var findOptions []*FindOption
	xmlFindOptions := &XmlFindOptions{}

	if err := loadXmlFile(expandPath(findOptionsXmlPath), xmlFindOptions); err != nil {
		return nil, err
	}

	for _, x := range xmlFindOptions.XmlFindOptions {
		findOption := &FindOption{x.Short, x.Long, strings.TrimSpace(x.Desc)}
		findOptions = append(findOptions, findOption)
	}
	return &FindOptions{findOptions}, nil
}
