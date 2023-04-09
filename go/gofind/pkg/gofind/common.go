package gofind

import (
	"fmt"
	"sort"
)

func log(message string) {
	fmt.Println(message)
}

type set map[string]bool

func makeSet(slice []string) set {
	s := make(map[string]bool)
	for _, v := range slice {
		if v != "" {
			s[v] = true
		}
	}
	return s
}

func makeMap(slice []string) map[string]string {
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

func contains(slice []string, s string) bool {
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

func getLongestLen(slice []string) int {
	longestLen := 0
	for _, s := range slice {
		if len(s) > longestLen {
			longestLen = len(s)
		}
	}
	return longestLen
}

func getMapKeys(m map[string]string) []string {
	var keys []string
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func getMapValues(m map[string]string) []string {
	var values []string
	for _, v := range m {
		values = append(values, v)
	}
	return values
}

func getSortedKeys(m map[string]string) []string {
	keys := getMapKeys(m)
	sort.Strings(keys)
	return keys
}

func getCountMapKeys(m map[string]int) []string {
	var keys []string
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func getSortedCountKeys(m map[string]int) []string {
	keys := getCountMapKeys(m)
	sort.Strings(keys)
	return keys
}
