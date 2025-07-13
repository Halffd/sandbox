# Check if a filename argument is provided
if ARGV.empty?
  puts "Please provide a file name as an argument."
  exit
end

# Open the file specified in the command-line argument
file_name = ARGV[0]
f = File.open(file_name, "r")

# Read all lines
lines = f.readlines

# Get the first line
first_line = lines[0]

# Print the first line
puts first_line

# Close the file
f.close