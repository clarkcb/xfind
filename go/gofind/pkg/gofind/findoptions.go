package gofind

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

type FindOption struct {
	Short   string
	Long    string
	Desc    string
	argType ArgTokenType
}

func (o FindOption) ShortArg() string {
	return o.Short
}

func (o FindOption) LongArg() string {
	return o.Long
}

func (o FindOption) Description() string {
	return o.Desc
}

func (o FindOption) ArgType() ArgTokenType {
	return o.argType
}

type FindOptions struct {
	FindOptions     []*FindOption
	BoolActionMap   map[string]boolAction
	StringActionMap map[string]stringAction
	IntActionMap    map[string]intAction
	LongActionMap   map[string]longAction
	ArgTokenizer    *ArgTokenizer
}

type boolAction func(b bool, settings *FindSettings)

func getBoolActionMap() map[string]boolAction {
	m := map[string]boolAction{
		"archivesonly": func(b bool, settings *FindSettings) {
			settings.SetArchivesOnly(b)
		},
		"colorize": func(b bool, settings *FindSettings) {
			settings.SetColorize(b)
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
		"nocolorize": func(b bool, settings *FindSettings) {
			settings.SetColorize(!b)
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
	return m
}

type stringAction func(s string, settings *FindSettings)

func getStringActionMap() map[string]stringAction {
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
	return m
}

type intAction func(i int, settings *FindSettings)

func getIntActionMap() map[string]intAction {
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

func getLongActionMap() map[string]longAction {
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

type JsonFindOptions struct {
	FindOptions []*FindOption
}

func FindOptionsFromJson() (*FindOptions, error) {
	config := NewFindConfig()
	data, err := os.ReadFile(config.FINDOPTIONSPATH)
	if err != nil {
		return &FindOptions{}, err
	}
	var jsonFindOptions JsonFindOptions
	if err = json.Unmarshal(data, &jsonFindOptions); err != nil {
		return &FindOptions{}, err
	}

	// TEMPORARY
	//findOptions.generateCodeFile("/Users/cary/src/xfind/go/gofind/pkg/gofind/findoptionsgen.go")

	var findOptions []*FindOption
	boolActionMap := getBoolActionMap()
	stringActionMap := getStringActionMap()
	intActionMap := getIntActionMap()
	longActionMap := getLongActionMap()

	for _, jo := range jsonFindOptions.FindOptions {
		var argType ArgTokenType
		if _, isBool := boolActionMap[jo.Long]; isBool {
			argType = ArgTokenTypeBool
		} else if _, isString := stringActionMap[jo.Long]; isString {
			argType = ArgTokenTypeString
		} else if _, isInt := intActionMap[jo.Long]; isInt {
			argType = ArgTokenTypeInt
		} else if _, isLong := longActionMap[jo.Long]; isLong {
			argType = ArgTokenTypeLong
		}
		fo := &FindOption{Short: jo.Short, Long: jo.Long, Desc: jo.Desc, argType: argType}
		findOptions = append(findOptions, fo)
	}

	argOptions := make([]ArgOption, len(findOptions))
	for i, o := range findOptions {
		argOptions[i] = o
	}

	return &FindOptions{
		findOptions,
		boolActionMap,
		stringActionMap,
		intActionMap,
		longActionMap,
		NewArgTokenizer(argOptions),
	}, nil
}

func NewFindOptions() *FindOptions {
	findOptions, err := FindOptionsFromJson()
	if err != nil {
		// do something
	}
	return findOptions
}

func (fo *FindOptions) updateSettingsFromArgTokens(settings *FindSettings, argTokens []*ArgToken) error {
	for _, argToken := range argTokens {
		if argToken.Type == ArgTokenTypeBool {
			if bf, isBool := fo.BoolActionMap[argToken.Name]; isBool {
				bf(argToken.Value.(bool), settings)
			} else {
				return fmt.Errorf("Invalid value for option: %v", argToken.Name)
			}
		} else if argToken.Type == ArgTokenTypeString {
			if sf, isString := fo.StringActionMap[argToken.Name]; isString {
				sf(argToken.Value.(string), settings)
			} else if argToken.Name == "settings-file" {
				err := fo.UpdateSettingsFromFile(settings, argToken.Value.(string))
				if err != nil {
					return err
				}
			} else {
				return fmt.Errorf("Invalid value for option: %v", argToken.Name)
			}
		} else if argToken.Type == ArgTokenTypeInt {
			if iff, isInt := fo.IntActionMap[argToken.Name]; isInt {
				iff(argToken.Value.(int), settings)
			} else {
				return fmt.Errorf("Invalid value for option: %v", argToken.Name)
			}
		} else if argToken.Type == ArgTokenTypeLong {
			if lff, isLong := fo.LongActionMap[argToken.Name]; isLong {
				lff(argToken.Value.(int64), settings)
			} else {
				return fmt.Errorf("Invalid value for option: %v", argToken.Name)
			}
		} else {
			return fmt.Errorf("Invalid option: %v", argToken.Name)
		}
	}
	return nil
}

func (fo *FindOptions) UpdateSettingsFromJson(settings *FindSettings, jsonString string) error {
	argTokens, err := fo.ArgTokenizer.TokenizeJson(jsonString)
	if err != nil {
		return err
	}
	return fo.updateSettingsFromArgTokens(settings, argTokens)
}

func (fo *FindOptions) UpdateSettingsFromFile(settings *FindSettings, filePath string) error {
	argTokens, err := fo.ArgTokenizer.TokenizeFile(filePath)
	if err != nil {
		return err
	}
	return fo.updateSettingsFromArgTokens(settings, argTokens)
}

func (fo *FindOptions) UpdateSettingsFromArgs(settings *FindSettings, args []string) error {
	argTokens, err := fo.ArgTokenizer.TokenizeArgs(args)
	if err != nil {
		return err
	}
	return fo.updateSettingsFromArgTokens(settings, argTokens)
}

func (fo *FindOptions) FindSettingsFromArgs(args []string) (*FindSettings, error) {
	settings := GetDefaultFindSettings()
	// default printFiles to true since running as cli
	settings.SetPrintFiles(true)

	err := fo.UpdateSettingsFromArgs(settings, args)

	return settings, err
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
		if o.ShortArg() == "" {
			sortKey = strings.ToLower(o.LongArg())
		} else {
			sortKey = fmt.Sprintf("%s@%s", strings.ToLower(o.ShortArg()),
				strings.ToLower(o.LongArg()))
		}
		m[sortKey] = o.LongArg()
	}
	return m
}

func (fo *FindOptions) getOptStringMap() map[string]string {
	m := map[string]string{}
	for _, o := range fo.FindOptions {
		optString := ""
		if o.ShortArg() != "" {
			optString = fmt.Sprintf("-%s,", o.ShortArg())
		}
		optString = fmt.Sprintf("%s--%s", optString, o.LongArg())
		m[o.LongArg()] = optString
	}
	return m
}

func (fo *FindOptions) getOptDescMap() map[string]string {
	m := map[string]string{}
	for _, o := range fo.FindOptions {
		m[o.LongArg()] = o.Description()
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
	for _, o := range fo.FindOptions {
		buffer.WriteString(fmt.Sprintf("%s{\"%s\", \"%s\", \"%s\"},\n",
			strings.Repeat("\t", depth), o.ShortArg(), o.LongArg(), EscapeQuotes(o.Description())))
	}
	depth--
	buffer.WriteString(fmt.Sprintf("%s},\n", strings.Repeat("\t", depth)))
	depth--
	buffer.WriteString(fmt.Sprintf("%s}\n}\n", strings.Repeat("\t", depth)))
	err := os.WriteFile(filePath, buffer.Bytes(), 0644)
	if err != nil {
		panic(err)
	}
}
