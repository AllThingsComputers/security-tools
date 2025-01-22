open System
open System.IO

/// Function to find files in a given directory with a specific pattern
let findFiles (directory: string) (pattern: string) =
    try
        // Ensure the directory exists
        if Directory.Exists(directory) then
            // Search for files matching the pattern
            Directory.GetFiles(directory, pattern, SearchOption.AllDirectories)
        else
            printfn "Directory '%s' does not exist." directory
            [||]
    with
    | :? UnauthorizedAccessException as ex ->
        printfn "Access denied: %s" ex.Message
        [||]
    | ex ->
        printfn "An error occurred: %s" ex.Message
        [||]

/// Function to write the results to a file
let writeToFile (outputPath: string) (lines: string[]) =
    try
        File.WriteAllLines(outputPath, lines)
        printfn "Results written to '%s'" outputPath
    with
    | ex ->
        printfn "Failed to write to file: %s" ex.Message

/// Main logic
[<EntryPoint>]
let main argv =
    // Define the directory to search and the file pattern
    let targetDirectory = @"C:\Users\YourUser\Documents" // Change as needed
    let searchPattern = "*.bat" // Example: search for batch files
    let outputFile = @"C:\Users\YourUser\Documents\searchResults.txt"

    // Find files
    let foundFiles = findFiles targetDirectory searchPattern

    // Display the results
    if foundFiles.Length > 0 then
        printfn "Found %d files:" foundFiles.Length
        Array.iter (printfn "%s") foundFiles

        // Write results to file
        writeToFile outputFile foundFiles
    else
        printfn "No files found matching the pattern '%s' in '%s'" searchPattern targetDirectory

    0 // Return an integer exit code
