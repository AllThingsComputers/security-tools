// https://golangdocs.com/golang-docker
// https://golangdocs.com/http-server-in-golang
// https://freshman.tech/snippets/go/http-redirect/
// https://www.programiz.com/golang/variable-scope

package main

import (
	"fmt"
	"io"
	"net/http"
)

var port string = "5555"                   // Global declaration for port
var colonPort = ":" + port                // Port format for ListenAndServe
var consoleMessage = "Listening on port " + port + "...\n" +
	"Go to http://localhost" + colonPort + "/ in your web browser to access it."

// `welcome` handler for the `/page3` route
func welcome(w http.ResponseWriter, r *http.Request) {
	io.WriteString(w, "Welcome to the cybersecurity page!\n")
}

// `hyperlink` handler for the `/` route with simulated download
func hyperlink(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Here we can use this HTTP Web Server created in Golang to run simulated threats on the port of your choice!\n")
	fmt.Fprintf(w, "Simulated download: <a href='/download'>Fake Download Link</a>\n")
	fmt.Fprintf(w, "Explore other pages: <a href='/page3'>Page 3</a> | <a href='/redirect'>Redirect</a>\n")
}

// `page2` handler
func page2(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "This is Page 2! Explore other pages for more simulated functionalities.")
}

// `redirect` handler for a malicious redirect
func redirect(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/page3", http.StatusFound) // Redirects to `/page3`
}

// `download` handler for a simulated drive-by download
func download(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Disposition", "attachment; filename=fakedownload.txt")
	w.Header().Set("Content-Type", "application/octet-stream")
	w.Write([]byte("This is a simulated malicious file download. Be cautious!"))
}

// Server configuration using a ServeMux
func serverMux() {
	mux := http.NewServeMux()

	// Route handlers
	mux.HandleFunc("/", hyperlink)
	mux.HandleFunc("/page2", page2)
	mux.HandleFunc("/page3", welcome)
	mux.HandleFunc("/redirect", redirect)
	mux.HandleFunc("/download", download)

	// Start the server
	fmt.Println(consoleMessage)
	if err := http.ListenAndServe(colonPort, mux); err != nil {
		fmt.Printf("Error starting server: %s\n", err)
	}
}

// Main function
func main() {
	serverMux() // Start the server
}
