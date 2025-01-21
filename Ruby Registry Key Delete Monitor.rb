require 'win32/registry'

# Method to check if a registry key exists
def value_exists?(path, key)
  reg_type = Win32::Registry::KEY_READ
  begin
    Win32::Registry::HKEY_LOCAL_MACHINE.open(path, reg_type) do |reg|
      return reg[key] != nil
    end
  rescue
    return false
  end
end

# Registry path and key to monitor
registry_path = "SYSTEM\\CurrentControlSet\\Services\\SharedAccess\\Parameters\\FirewallPolicy\\StandardProfile"
registry_key = "EnableFirewall"

puts "Monitoring the registry key for changes...\n\n"

# Continuous monitoring loop
loop do
  key_exists = value_exists?(registry_path, registry_key)

  case key_exists
  when true
    puts "Registry Key exists: #{registry_path}\\#{registry_key}"
    puts "The registry key has not been deleted.\n"
  when false
    puts "Registry Key missing: #{registry_path}\\#{registry_key}"
    puts "The registry key has been deleted."
    break # Exit the loop once the key is deleted
  end

  sleep(2) # Check every 2 seconds
end
