#!/bin/python
import os
import shutil
from datetime import datetime, timedelta
import subprocess
import sys

def copy_recent_files(src_dir, dest_dir):
    # Get the current time and the time 18 hours ago
    now = datetime.now()
    time_threshold = now - timedelta(hours=98)
    
    # Ensure destination directory exists
    os.makedirs(dest_dir, exist_ok=True)

    # Iterate through all files in the source directory
    for filename in os.listdir(src_dir):
        src_file_path = os.path.join(src_dir, filename)

        # Check if it's a file
        if os.path.isfile(src_file_path):
            # Get the last modified time of the file
            modified_time = datetime.fromtimestamp(os.path.getmtime(src_file_path))
            
            # Check if the file was modified within the last 18 hours
            if modified_time >= time_threshold:
                # Copy the file to the destination directory
                shutil.copy2(src_file_path, dest_dir)
                print(f'Copied: {filename}')

# Example usag/UserDatae
dest_directory = r'/mnt/ssd2/Games/KKS/'
src_directory = r'/home/all/Games/Koikatsu/'

copy_recent_files(src_directory+'UserData/chara/female', dest_directory+'UserData/chara/female')
copy_recent_files(src_directory+'UserData/chara/male', dest_directory+'UserData/chara/male')

if len(sys.argv) <= 1:
    subprocess.Popen([r"cd /mnt/ssd2/Games/KKS && /home/all/scripts/proton.sh KoikatsuSunshine.exe /home/all/Games/KKS"])
