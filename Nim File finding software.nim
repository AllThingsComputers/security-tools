# Nim File Finder
# https://nim-by-example.github.io/helloworld/
# https://stackoverflow.com/questions/69559275/how-to-list-items-in-directory-in-nim

import os, sequtils

echo "Hello, welcome to the Nim File Finder!"

# Define the target folder and file extensions to search for
let
  targetFolder = "C:\\Users\\."  # Update the folder path as needed
  targetExt = @[".mp3", ".wav", ".wma"]

# Procedure to scan a folder for files with specific extensions
proc scanFolder(tgPath: string, extLst: seq[string]): seq[string] =
  var
    fileNames: seq[string] = @[]  # Initialize an empty sequence to store file paths
  for kind, obj in walkDir(tgPath):  # Walk through the directory
    if kind == pcFile:  # Only process files
      let (_, _, ext) = splitFile(obj)  # Get the file extension
      if ext in extLst:  # Check if the extension matches the target list
        fileNames.add(obj)  # Add the file to the list
  return fileNames

# Scan the target folder for the specified file extensions
let fileList = scanFolder(targetFolder, targetExt)

# Print the list of matching files
if fileList.len > 0:
  echo "\nFound the following files:"
  for f in fileList:
    echo f
else:
  echo "\nNo matching files found."
