package gofind

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type FindItemsIterator struct {
	idx   int
	items *FindItems
}

func NewFindItemsIterator(sf *FindItems) *FindItemsIterator {
	return &FindItemsIterator{
		-1,
		sf,
	}
}

func (i *FindItemsIterator) Next() bool {
	i.idx++
	if i.idx >= len(i.items.items) {
		return false
	}
	return true
}

func (i *FindItemsIterator) Value() *FindItem {
	return i.items.items[i.idx]
}

type FindItems struct {
	items     []*FindItem
	strPtrMap map[string]*string
}

func NewFindItems() *FindItems {
	return &FindItems{
		[]*FindItem{},
		make(map[string]*string),
	}
}

// limits string pointers to one per distinct string (memory management)
func (si *FindItems) getStrPtr(s *string) *string {
	strPtr := s
	if sp, ok := si.strPtrMap[*s]; ok {
		strPtr = sp
	} else {
		si.strPtrMap[*s] = s
	}
	return strPtr
}

func (si *FindItems) AddItem(i *FindItem) {
	si.items = append(si.items, &FindItem{
		i.Containers,
		si.getStrPtr(i.Path),
		si.getStrPtr(i.Name),
		i.fileType,
	})
}

func (si *FindItems) Count() int {
	return len(si.items)
}

func (si *FindItems) IsEmpty() bool {
	return len(si.items) == 0
}

func (si *FindItems) Iterator() *FindItemsIterator {
	return NewFindItemsIterator(si)
}

type FindItem struct {
	Containers []string
	Path       *string
	Name       *string
	fileType   FileType
}

func NewFindItem(path *string, name *string, fileType FileType) *FindItem {
	return &FindItem{
		[]string{},
		path,
		name,
		fileType,
	}
}

func (si *FindItem) AddContainer(c string) {
	si.Containers = append(si.Containers, c)
}

const containerSeparator = "!"

func (si *FindItem) String() string {
	var buffer bytes.Buffer
	if len(si.Containers) > 0 {
		buffer.WriteString(strings.Join(si.Containers, containerSeparator))
		buffer.WriteString(containerSeparator)
	}
	path := normalizePath(*si.Path)
	if isDotDir(path) {
		buffer.WriteString(fmt.Sprintf("%s%c%s", path, os.PathSeparator, *si.Name))
	} else {
		buffer.WriteString(filepath.Join(*si.Path, *si.Name))
	}
	return buffer.String()
}
