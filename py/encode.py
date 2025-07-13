#!/bin/python
import os
import subprocess
import glob
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

def list_encoding_options():
    print("\nAvailable encoding options:")
    print("1. Chrome Compatible (H.264/AAC in MKV)")
    print("2. High Quality (H.264/AAC in MKV, higher bitrate)")
    print("3. Small Size (H.265/AAC in MKV, efficient compression)")
    print("4. Original Codecs (Copy streams to MKV container)")
    print("5. WebM (VP9/Opus for web compatibility)")
    print("6. Chrome Compatible (H.264/AAC in MP4) - No Attachments")
    sys.exit(0)

# Parse command line arguments
encoding_type = 1  # Default to Chrome compatible MKV
input_directory = '.'  # Default to current directory

if len(sys.argv) > 1:
    # Check if first argument is a request to list options
    if sys.argv[1].isdigit() and int(sys.argv[1]) == 0:
        list_encoding_options()
    elif sys.argv[1].isdigit():
        encoding_type = int(sys.argv[1])
        if encoding_type < 1 or encoding_type > 6:
            print(f"Invalid encoding type: {encoding_type}")
            list_encoding_options()
    
    # If there's a second argument, use it as the directory
    if len(sys.argv) > 2:
        input_directory = sys.argv[2]

output_directory = os.path.join(input_directory, 'converted')

# Configure settings
max_workers = max(1, os.cpu_count() - 1)  # Leave one CPU core free

# Create output directory if it doesn't exist
os.makedirs(output_directory, exist_ok=True)

def get_encoding_settings(encoding_type):
    settings = {}
    
    if encoding_type == 1:  # Chrome Compatible MKV
        settings['output_ext'] = '.mkv'
        settings['vcodec'] = ['-c:v', 'libx264', '-crf', '23', '-preset', 'faster']
        settings['acodec'] = ['-c:a', 'aac', '-b:a', '192k']
        settings['scodec'] = ['-c:s', 'copy']  # Keep subtitle format as is
        settings['extra'] = []
        settings['map_option'] = ['-map', '0']  # Map all streams
    elif encoding_type == 2:  # High Quality MKV
        settings['output_ext'] = '.mkv'
        settings['vcodec'] = ['-c:v', 'libx264', '-crf', '18', '-preset', 'medium']
        settings['acodec'] = ['-c:a', 'aac', '-b:a', '320k']
        settings['scodec'] = ['-c:s', 'copy']
        settings['extra'] = []
        settings['map_option'] = ['-map', '0']
    elif encoding_type == 3:  # Small Size MKV
        settings['output_ext'] = '.mkv'
        settings['vcodec'] = ['-c:v', 'libx265', '-crf', '28', '-preset', 'medium']
        settings['acodec'] = ['-c:a', 'aac', '-b:a', '128k']
        settings['scodec'] = ['-c:s', 'copy']
        settings['extra'] = []
        settings['map_option'] = ['-map', '0']
    elif encoding_type == 4:  # Original Codecs MKV
        settings['output_ext'] = '.mkv'
        settings['vcodec'] = ['-c:v', 'copy']
        settings['acodec'] = ['-c:a', 'copy']
        settings['scodec'] = ['-c:s', 'copy']
        settings['extra'] = []
        settings['map_option'] = ['-map', '0']
    elif encoding_type == 5:  # WebM
        settings['output_ext'] = '.webm'
        settings['vcodec'] = ['-c:v', 'libvpx-vp9', '-crf', '30', '-b:v', '0']
        settings['acodec'] = ['-c:a', 'libopus', '-b:a', '128k']
        settings['scodec'] = ['-c:s', 'copy']
        settings['extra'] = []
        settings['map_option'] = ['-map', '0:v?', '-map', '0:a?', '-map', '0:s?']  # Map only video, audio, and subtitle streams
    elif encoding_type == 6:  # Chrome Compatible MP4 (No Attachments)
        settings['output_ext'] = '.mp4'
        settings['vcodec'] = ['-c:v', 'libx264', '-crf', '23', '-preset', 'faster']
        settings['acodec'] = ['-c:a', 'aac', '-b:a', '192k']
        settings['scodec'] = ['-c:s', 'mov_text']
        settings['extra'] = ['-movflags', '+faststart']
        settings['map_option'] = ['-map', '0:v?', '-map', '0:a?', '-map', '0:s?']  # Skip attachments
    
    return settings

def convert_file(input_file):
    # Get encoding settings
    settings = get_encoding_settings(encoding_type)
    
    # Create output filename with appropriate extension
    base_name = os.path.basename(input_file)
    output_name = os.path.splitext(base_name)[0] + settings['output_ext']
    output_path = os.path.join(output_directory, output_name)
    
    # Skip if output file already exists and is newer than input file
    if os.path.exists(output_path):
        if os.path.getmtime(output_path) > os.path.getmtime(input_file):
            print(f"Skipping {input_file} (output file already exists and is newer)")
            return True
    
    print(f"Starting conversion of: {input_file}")
    
    try:
        # Build the ffmpeg command
        cmd = [
            'ffmpeg',
            '-y',  # Overwrite output files without asking
            '-i', input_file,
        ]
        
        # Add codec settings
        cmd.extend(settings['vcodec'])
        cmd.extend(settings['acodec'])
        cmd.extend(settings['scodec'])
        
        # Add mapping options
        cmd.extend(settings['map_option'])
        
        # Add extra settings
        cmd.extend(settings['extra'])
        
        # Add common settings
        cmd.extend([
            '-max_muxing_queue_size', '9999',
            output_path
        ])
        
        # Run the command with a timeout to prevent hanging
        result = subprocess.run(cmd, check=True, timeout=7200, capture_output=True, text=True)  # 2-hour timeout
        
        # Verify the output file exists and has a reasonable size
        if os.path.exists(output_path) and os.path.getsize(output_path) > 1024*1024:  # >1MB
            print(f"Successfully converted: {output_path}")
            return True
        else:
            print(f"Warning: Output file {output_path} is suspiciously small or missing")
            print(f"Command output: {result.stdout}")
            print(f"Command errors: {result.stderr}")
            return False
            
    except subprocess.CalledProcessError as e:
        print(f"Error converting {input_file}: {e}")
        print(f"Command errors: {e.stderr}")
        return False
    except subprocess.TimeoutExpired:
        print(f"Timeout while converting {input_file}")
        return False
    except Exception as e:
        print(f"Unexpected error with {input_file}: {str(e)}")
        return False

# Get list of all MKV files in the input directory
input_pattern = os.path.join(input_directory, '*.mkv')
input_files = glob.glob(input_pattern)

if not input_files:
    print(f"No MKV files found in directory: {input_directory}")
    exit(1)

# Print encoding information
encoding_names = {
    1: "Chrome Compatible (H.264/AAC in MKV)",
    2: "High Quality (H.264/AAC in MKV, higher bitrate)",
    3: "Small Size (H.265/AAC in MKV)",
    4: "Original Codecs (stream copy to MKV)",
    5: "WebM (VP9/Opus)",
    6: "Chrome Compatible (H.264/AAC in MP4) - No Attachments"
}

print(f"Encoding mode: {encoding_names[encoding_type]}")
print(f"Found {len(input_files)} MKV files to convert in {input_directory}")
print(f"Using {max_workers} parallel processes")
print(f"Output directory: {output_directory}")

# Process files in parallel
with ThreadPoolExecutor(max_workers=max_workers) as executor:
    results = list(executor.map(convert_file, input_files))

# Print summary
successful = results.count(True)
print(f"\nConversion complete: {successful} of {len(input_files)} files successfully converted")
print(f"Output files saved to: {output_directory}")