package gofind

import (
	"fmt"
	"regexp"
	"strings"
)

func StringListToString(list []string) string {
	var b strings.Builder
	b.WriteString("[")
	var elems []string
	for _, l := range list {
		elems = append(elems, fmt.Sprintf("\"%s\"", l))
	}
	b.WriteString(strings.Join(elems, ", "))
	b.WriteString("]")
	return b.String()
}

func FileTypeListToString(list []FileType) string {
	var b strings.Builder
	b.WriteString("[")
	var elems []string
	for _, ft := range list {
		elems = append(elems, fmt.Sprintf("%s", GetNameForFileType(ft)))
	}
	b.WriteString(strings.Join(elems, ", "))
	b.WriteString("]")
	return b.String()
}

func RegexpListToString(list []*regexp.Regexp) string {
	var b strings.Builder
	b.WriteString("[")
	for i, r := range list {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(fmt.Sprintf("\"%s\"", r.String()))
	}
	b.WriteString("]")
	return b.String()
}

func PatternsToString(fp *Patterns) string {
	return RegexpListToString(fp.patterns)
}

func EscapeQuotes(s string) string {
	return strings.Replace(s, "\"", "\\\"", -1)
}
