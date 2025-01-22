// https://appdividend.com/2019/12/19/how-to-read-files-in-golang-go-file-read-example/
// https://www.geeksforgeeks.org/string-contains-function-in-golang-with-examples/
// go get github.com/dariubs/percent -- package percent is not in goroot admin access.
// https://www.educative.io/answers/how-to-create-a-function-in-golang
// https://appdividend.com/2022/06/07/how-to-convert-golang-int-to-string
// https://www.tutorialkart.com/golang-tutorial/golang-map/

package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

// Percent function calculates the percentage and prints it
func percent(num1 float64, num2 float64) {
	if num2 == 0 {
		fmt.Println("0%")
		return
	}
	value := (num1 / num2) * 100
	str2 := strconv.FormatFloat(value, 'f', 2, 64) // Convert to string with 2 decimal places
	fmt.Println(str2 + "%")
}

func main() {
	// List of sample files (replace with actual files as needed)
	var files = []string{"fakefile1.txt", "fakefile2.txt", "fakefile3.txt", "fakefile4.txt", "fakefiles.txt", "fakefile6.txt"}

	// Language-specific syntax scores
	var Go_Syntax_Score, Javascript_Syntax_Score, Html_Syntax_Score, CSS_Syntax_Score, Java_Syntax_Score, Ruby_Syntax_Score float64

	// Loop through files in reverse order
	for i := len(files) - 1; i >= 0; i-- {
		data, err := ioutil.ReadFile(files[i])
		if err != nil {
			fmt.Println("File reading error:", err)
			continue
		}
		fmt.Println("Contents of file named", files[i], "is:", string(data))

		// Syntax detection based on keywords
		switch {
		case strings.Contains(string(data), "go"):
			Go_Syntax_Score++
		case strings.Contains(string(data), "javascript"):
			Javascript_Syntax_Score++
		case strings.Contains(string(data), "html"):
			Html_Syntax_Score++
		case strings.Contains(string(data), "css"):
			CSS_Syntax_Score++
		case strings.Contains(string(data), "java"):
			Java_Syntax_Score++
		case strings.Contains(string(data), "ruby"):
			Ruby_Syntax_Score++
		default:
			fmt.Println("Zero detections made.")
		}

		// Calculate total score
		total_Syntax_Score := Go_Syntax_Score + Javascript_Syntax_Score + Html_Syntax_Score +
			CSS_Syntax_Score + Java_Syntax_Score + Ruby_Syntax_Score

		fmt.Printf("For the file named %s, a total of %.2f points were scored\n", files[i], total_Syntax_Score)
		fmt.Println("Language | Score | Percent")
		fmt.Print("Go       | ", Go_Syntax_Score, " | ")
		percent(Go_Syntax_Score, total_Syntax_Score)
		fmt.Print("Js       | ", Javascript_Syntax_Score, " | ")
		percent(Javascript_Syntax_Score, total_Syntax_Score)
		fmt.Print("HTML     | ", Html_Syntax_Score, " | ")
		percent(Html_Syntax_Score, total_Syntax_Score)
		fmt.Print("CSS      | ", CSS_Syntax_Score, " | ")
		percent(CSS_Syntax_Score, total_Syntax_Score)
		fmt.Print("Ruby     | ", Ruby_Syntax_Score, " | ")
		percent(Ruby_Syntax_Score, total_Syntax_Score)
		fmt.Print("Java     | ", Java_Syntax_Score, " | ")
		percent(Java_Syntax_Score, total_Syntax_Score)

		// Create a map to store scores
		scoreMap := map[string]float64{
			"Go":         Go_Syntax_Score,
			"Java":       Java_Syntax_Score,
			"Javascript": Javascript_Syntax_Score,
			"Ruby":       Ruby_Syntax_Score,
			"CSS":        CSS_Syntax_Score,
			"HTML":       Html_Syntax_Score,
		}

		// Detect predominant language
		fmt.Println("Analyzing code...")
		switch {
		case scoreMap["Go"] > 0:
			fmt.Println("The code has been analyzed and has a high probability of being Golang.")
		case scoreMap["Java"] > 0:
			fmt.Println("The code has been analyzed and has a high probability of being Java.")
		case scoreMap["Javascript"] > 0:
			fmt.Println("The code has been analyzed and has a high probability of being Javascript.")
		case scoreMap["Ruby"] > 0:
			fmt.Println("The code has been analyzed and has a high probability of being Ruby.")
		case scoreMap["CSS"] > 0:
			fmt.Println("The code has been analyzed and has a high probability of being CSS.")
		case scoreMap["HTML"] > 0:
			fmt.Println("The code has been analyzed and has a high probability of being HTML.")
		default:
			fmt.Println("No predominant syntax detected. More data may be required.")
		}
	}
}
