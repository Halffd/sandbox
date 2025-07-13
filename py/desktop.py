#!/usr/bin/env python3
import os
import sys

home = os.environ["HOME"]

# Get the name and command from command line arguments
if len(sys.argv) != 3:
    print("Usage: python3 create_desktop_entry.py <name> <command>")
    sys.exit(1)

name = sys.argv[1]
command = sys.argv[2]

# Define the contents of the .desktop file
launcher = [
    "[Desktop Entry]",
    "Version=1.0",
    "Name={}".format(name),
    "Exec={}".format(command),
    "Type=Application",
    "Terminal=false",  # Set to true if the app needs to run in a terminal
    "Icon=",  # Optionally set an icon path
]

# Directory for application launchers
applications_dir = os.path.join(home, ".local", "share", "applications")

# Ensure the applications directory exists
if not os.path.exists(applications_dir):
    os.makedirs(applications_dir)

# Create the .desktop file name
file_name = "{}.desktop".format(name.lower().replace(" ", "_"))
desktop_file = os.path.join(applications_dir, file_name)

# Function to create the .desktop file
def create_desktop_file(file_path):
    if not os.path.exists(file_path):
        with open(file_path, "wt") as out:
            for line in launcher:
                out.write(line + "\n")
        print("Created: {}".format(file_path))
    else:
        print("File exists: {}, choose another name".format(file_path))

# Create the file for application launcher
create_desktop_file(desktop_file)