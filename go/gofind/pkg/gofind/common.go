package gofind

import (
	"fmt"
	"github.com/pmylund/sortutil"
	"sort"
)

func Log(message string) {
	fmt.Println(message)
}

type set map[string]bool

func MakeStringSet(slice []string) set {
	s := make(map[string]bool)
	for _, v := range slice {
		if v != "" {
			s[v] = true
		}
	}
	return s
}

func MakeStringMap(slice []string) map[string]string {
	s := make(map[string]string)
	for _, v := range slice {
		if v != "" {
			s[v] = v
		}
	}
	return s
}

func union(s1, s2 set) set {
	s := make(map[string]bool)
	for k := range s1 {
		s[k] = true
	}
	for k := range s2 {
		s[k] = true
	}
	return s
}

func Contains(slice []string, s string) bool {
	for _, as := range slice {
		if s == as {
			return true
		}
	}
	return false
}

func containsV(slice []string, s string) bool {
	for _, as := range slice {
		if s == as {
			return true
		}
	}
	return false
}

func GetLongestLen(slice []string) int {
	longestLen := 0
	for _, s := range slice {
		if len(s) > longestLen {
			longestLen = len(s)
		}
	}
	return longestLen
}

func GetMapKeys(m map[string]string) []string {
	var keys []string
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func GetMapValues(m map[string]string) []string {
	var values []string
	for _, v := range m {
		values = append(values, v)
	}
	return values
}

func GetSortedKeys(m map[string]string) []string {
	keys := GetMapKeys(m)
	sort.Strings(keys)
	return keys
}

func getSortedSetValues(s set) []string {
	keys := []string{}
	for k := range s {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

func GetCountMapKeys(m map[string]int) []string {
	var keys []string
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func GetSortedCountKeys(m map[string]int) []string {
	keys := GetCountMapKeys(m)
	sort.Strings(keys)
	return keys
}

func GetCaseInsensitiveSortedCountKeys(m map[string]int) []string {
	mk := make([]string, len(m))
	i := 0
	for k, _ := range m {
		mk[i] = k
		i++
	}
	sortutil.CiAsc(mk)
	return mk
}

func GetMinInt(x int, y int) int {
	if x < y {
		return x
	}
	return y
}
