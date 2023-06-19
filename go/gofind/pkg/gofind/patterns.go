package gofind

import (
	"regexp"
)

type PatternsIterator struct {
	idx      int
	patterns *Patterns
}

func NewPatternsIterator(fp *Patterns) *PatternsIterator {
	return &PatternsIterator{
		-1,
		fp,
	}
}

func (it *PatternsIterator) Next() bool {
	it.idx++
	if it.idx >= len(it.patterns.patterns) {
		return false
	}
	return true
}

func (it *PatternsIterator) Value() *regexp.Regexp {
	return it.patterns.patterns[it.idx]
}

type Patterns struct {
	patterns []*regexp.Regexp
}

func NewPatterns() *Patterns {
	return &Patterns{
		[]*regexp.Regexp{},
	}
}

func (fp *Patterns) AddPattern(r *regexp.Regexp) {
	fp.patterns = append(fp.patterns, r)
}

func (fp *Patterns) AddPatternString(s string) {
	fp.patterns = append(fp.patterns, regexp.MustCompile(s))
}

func (fp *Patterns) Get(i int) *regexp.Regexp {
	if i < len(fp.patterns) {
		return fp.patterns[i]
	}
	return nil
}

func (fp *Patterns) Len() int {
	return len(fp.patterns)
}

func (fp *Patterns) IsEmpty() bool {
	return len(fp.patterns) == 0
}

func (fp *Patterns) Iterator() *PatternsIterator {
	return NewPatternsIterator(fp)
}

func (fp *Patterns) MatchesAny(s string) bool {
	for _, p := range fp.patterns {
		if p.MatchString(s) {
			return true
		}
	}
	return false
}

func (fp *Patterns) AnyMatchesAny(ss []string) bool {
	for _, s := range ss {
		if fp.MatchesAny(s) {
			return true
		}
	}
	return false
}
