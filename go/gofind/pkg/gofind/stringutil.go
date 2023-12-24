package gofind

import (
	"bytes"
	"fmt"
	"strings"
)

func StringListToString(list []string) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	var elems []string
	for _, l := range list {
		elems = append(elems, fmt.Sprintf("\"%s\"", l))
	}
	buffer.WriteString(strings.Join(elems, ", "))
	buffer.WriteString("]")
	return buffer.String()
}

func FileTypeListToString(list []FileType) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	var elems []string
	for _, ft := range list {
		elems = append(elems, fmt.Sprintf("%s", GetNameForFileType(ft)))
	}
	buffer.WriteString(strings.Join(elems, ", "))
	buffer.WriteString("]")
	return buffer.String()
}

func PatternsToString(fp *Patterns) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	for i, r := range fp.patterns {
		if i > 0 {
			buffer.WriteString(", ")
		}
		buffer.WriteString(fmt.Sprintf("\"%s\"", r.String()))
	}
	buffer.WriteString("]")
	return buffer.String()
}

func EscapeQuotes(s string) string {
	return strings.Replace(s, "\"", "\\\"", -1)
}
