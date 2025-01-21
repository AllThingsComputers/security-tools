# Define paths for Current User and Local Machine startup items
CUStartUpItemsPath = "C:\\Users\\.\\AppData\\Roaming\\Microsoft\\Windows\\Start Menu\\Programs\\Startup"
LMStartUpItemsPath = "C:\\ProgramData\\Microsoft\\Windows\\Start Menu\\Programs\\StartUp"

# Read initial startup items
CUStartUpItems = readdir(CUStartUpItemsPath)
LMStartUpItems = readdir(LMStartUpItemsPath)

# Display initial startup items
println("The initial Current User startup items are:")
println(string.(CUStartUpItems))
println("The initial Local Machine startup items are:")
println(string.(LMStartUpItems))

# Monitor for changes in startup items
println("\nNow we will loop to keep checking for changes in the startup items...")
while true
    # Get current startup items
    currentCUStartUpItems = readdir(CUStartUpItemsPath)
    currentLMStartUpItems = readdir(LMStartUpItemsPath)

    if currentCUStartUpItems == CUStartUpItems && currentLMStartUpItems == LMStartUpItems
        println("Nothing's changed")
        sleep(1)  # Pause for 1 second before checking again
    else
        println("\nSomething's changed!")
        # Display updated startup items
        println("The amended startup item lists are:")
        println("Current User startup items:")
        println(string.(currentCUStartUpItems))
        println("Local Machine startup items:")
        println(string.(currentLMStartUpItems))

        # Update tracked startup items
        CUStartUpItems = currentCUStartUpItems
        LMStartUpItems = currentLMStartUpItems
    end
end
