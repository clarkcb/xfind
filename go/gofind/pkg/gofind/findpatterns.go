package gofind

import (
	"regexp"
)

type FindPatternsIterator struct {
	idx      int
	patterns *FindPatterns
}

func NewFindPatternsIterator(fp *FindPatterns) *FindPatternsIterator {
	return &FindPatternsIterator{
		-1,
		fp,
	}
}

func (it *FindPatternsIterator) Next() bool {
	it.idx++
	if it.idx >= len(it.patterns.patterns) {
		return false
	}
	return true
}

func (it *FindPatternsIterator) Value() *regexp.Regexp {
	return it.patterns.patterns[it.idx]
}

type FindPatterns struct {
	patterns []*regexp.Regexp
}

func NewFindPatterns() *FindPatterns {
	return &FindPatterns{
		[]*regexp.Regexp{},
	}
}

func (fp *FindPatterns) AddPattern(s string) {
	fp.patterns = append(fp.patterns, regexp.MustCompile(s))
}

func (fp *FindPatterns) IsEmpty() bool {
	return len(fp.patterns) == 0
}

func (fp *FindPatterns) Iterator() *FindPatternsIterator {
	return NewFindPatternsIterator(fp)
}

func (fp *FindPatterns) MatchesAny(s string) bool {
	for _, p := range fp.patterns {
		if p.MatchString(s) {
			return true
		}
	}
	return false
}

func (fp *FindPatterns) AnyMatchesAny(ss []string) bool {
	for _, s := range ss {
		if fp.MatchesAny(s) {
			return true
		}
	}
	return false
}
