#!/bin/python
import os
import re
import pyperclip
import srttext
# Define the regex patterns for episode numbers
patterns = [
    re.compile(r'-\ (\d{2})'),  # Matches format like '- 01'
    re.compile(r'S\d{2}E(\d{2})')  # Matches format like 'S01E01'
]

episodes = []
episode_files = {}

# Loop through all files in the current directory
for filename in os.listdir('.'):
    if os.path.isfile(filename):
        for pattern in patterns:
            match = pattern.search(filename)
            if match:
                episode = int(match.group(1)) if pattern == patterns[0] else int(match.group(1))
                episodes.append(episode)
                episode_files[episode] = filename  # Store the filename associated with the episode
                print(f"Found file: {filename} for episode {episode}")  # Log filename
                break  # Exit loop after finding the first match

# Input range from the user
num1 = int(input("Enter the minimum episode number: "))  # Get minimum value
num2 = int(input("Enter the maximum episode number: "))  # Get maximum value

# Prepare to merge files within the specified range
merged_content = []

# Merge files based on the specified range
for episode in range(num1, num2 + 1):
    if episode in episode_files:
        filename = episode_files[episode]
        with open(filename, 'r', errors='ignore') as file:
            content = file.read()
            merged_content.append(content)
            print(f"Copied content from: {filename}")  # Log copied content
srt = f'merged_ep{num1}-{num2}.txt'
# Write the merged content to a new file
with open(srt, 'w', errors='ignore') as output_file:
    output_file.write('\n'.join(merged_content))

# Copy the merged content to the clipboard
#pyperclip.copy('\n'.join(merged_content))
processed = srttext.clean_srt_text(content, merged_content)

# Auto-copy to clipboard (like your original)
pyperclip.copy(processed)
    
print("Merged content copied to clipboard.")

print(f"Merged content from episodes {num1} to {num2} into '{srt}'.")
