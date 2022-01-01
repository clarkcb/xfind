package gofind

import (
	"bytes"
	"fmt"
	"strings"
)

func stringListToString(list []string) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	var elems []string
	for _, l := range list {
		elems = append(elems, fmt.Sprintf("\"%s\"", l))
	}
	buffer.WriteString(strings.Join(elems, ","))
	buffer.WriteString("]")
	return buffer.String()
}

func fileTypeListToString(list []FileType) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	var elems []string
	for _, ft := range list {
		elems = append(elems, fmt.Sprintf("\"%s\"", getNameForFileType(ft)))
	}
	buffer.WriteString(strings.Join(elems, ","))
	buffer.WriteString("]")
	return buffer.String()
}

func findPatternsToString(fp *FindPatterns) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	for i, r := range fp.patterns {
		if i > 0 {
			buffer.WriteString(",")
		}
		buffer.WriteString(fmt.Sprintf("\"%s\"", r.String()))
	}
	buffer.WriteString("]")
	return buffer.String()
}
