package gofind

import (
	"regexp"
)

type FindPatternsIterator struct {
	idx      int
	patterns *FindPatterns
}

func NewFindPatternsIterator(sp *FindPatterns) *FindPatternsIterator {
	return &FindPatternsIterator{
		-1,
		sp,
	}
}

func (i *FindPatternsIterator) Next() bool {
	i.idx++
	if i.idx >= len(i.patterns.patterns) {
		return false
	}
	return true
}

func (i *FindPatternsIterator) Value() *regexp.Regexp {
	return i.patterns.patterns[i.idx]
}

type FindPatterns struct {
	patterns []*regexp.Regexp
}

func NewFindPatterns() *FindPatterns {
	return &FindPatterns{
		[]*regexp.Regexp{},
	}
}

func (sp *FindPatterns) AddPattern(s *string) {
	sp.patterns = append(sp.patterns, regexp.MustCompile(*s))
}

func (sp *FindPatterns) IsEmpty() bool {
	return len(sp.patterns) == 0
}

func (sp *FindPatterns) Iterator() *FindPatternsIterator {
	return NewFindPatternsIterator(sp)
}

func (sp *FindPatterns) MatchesAny(s *string) bool {
	for _, p := range sp.patterns {
		if p.MatchString(*s) {
			return true
		}
	}
	return false
}

func (sp *FindPatterns) AnyMatchesAny(ss []*string) bool {
	for _, s := range ss {
		if sp.MatchesAny(s) {
			return true
		}
	}
	return false
}
