// go get -u github.com/shirou/gopsutil/v3/process
// https://stackoverflow.com/questions/9030680/list-of-currently-running-process-in-go
// https://gist.github.com/arxdsilva/7392013cbba7a7090cbcd120b7f5ca31
// https://www.golanglearn.com/golang-tutorials/how-to-check-golang-array-contains/
// https://gosamples.dev/remove-duplicates-slice/
// https://www.dotnetperls.com/sort-go

package main

import (
	"fmt"
	"sort"
	"strings"

	"github.com/shirou/gopsutil/v3/process"
)

// Compare two slices and return the elements in `a` that are not in `b`.
func compare(a, b []string) []string {
	result := []string{}
	bSet := make(map[string]bool)
	for _, v := range b {
		bSet[v] = true
	}
	for _, v := range a {
		if !bSet[v] {
			result = append(result, v)
		}
	}
	return result
}

// Remove duplicates from a slice.
func unique(s []string) []string {
	inResult := make(map[string]bool)
	var result []string
	for _, str := range s {
		if _, ok := inResult[str]; !ok {
			inResult[str] = true
			result = append(result, str)
		}
	}
	return result
}

// Check if an element exists in a slice.
func isElementExist(s []string, str string) bool {
	for _, v := range s {
		if v == str {
			return true
		}
	}
	return false
}

func main() {
	fmt.Println("Starting process monitor...")

	// First process list capture
	var processList1 []string
	processes1, _ := process.Processes()
	for _, proc := range processes1 {
		name, err := proc.Name()
		if err == nil {
			processList1 = append(processList1, strings.ToLower(name))
		}
	}

	uniqueSlice1 := unique(processList1)
	sort.Strings(uniqueSlice1)
	fmt.Println("First ordered and unique process slice:", uniqueSlice1)
	fmt.Println("Run your malware, then press any key and Enter to proceed to get a second array of processes post-execution:")
	var anykey string
	fmt.Scanln(&anykey)

	// Second process list capture
	var processList2 []string
	processes2, _ := process.Processes()
	for _, proc := range processes2 {
		name, err := proc.Name()
		if err == nil {
			processList2 = append(processList2, strings.ToLower(name))
		}
	}

	uniqueSlice2 := unique(processList2)
	sort.Strings(uniqueSlice2)
	fmt.Println("Second ordered and unique process slice:", uniqueSlice2)

	// Compare slices
	removedProcesses := compare(uniqueSlice1, uniqueSlice2)
	fmt.Println("Processes terminated after malware execution:", removedProcesses)

	newProcesses := compare(uniqueSlice2, uniqueSlice1)
	fmt.Println("Processes started after malware execution:", newProcesses)

	// Optional: Combined list of changes
	combinedChanges := append(removedProcesses, newProcesses...)
	fmt.Println("Total changes in processes (started + terminated):", combinedChanges)

	// Process existence checks
	for _, processName := range combinedChanges {
		fmt.Printf("Process '%s' existed before malware execution: %v\n", processName, isElementExist(uniqueSlice1, processName))
		fmt.Printf("Process '%s' exists after malware execution: %v\n", processName, isElementExist(uniqueSlice2, processName))
	}
}
