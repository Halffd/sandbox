#!/usr/bin/env python3
import os
import re
import argparse
import shutil # For file copying (backup)

def usage():
    """Prints usage information and exits."""
    print("Usage: python subtitle_delay.py <regex> <delay_seconds> [--file_type <extension>]")
    print("  <regex>         : Regular expression to match file names (e.g., '.*Episode.*' or '^MyMovie$')")
    print("  <delay_seconds> : Delay to apply in seconds (can be positive or negative, e.g., '1.5' for 1.5 seconds later, or '-0.5' for 0.5 seconds earlier)")
    print("  --file_type     : Optional. The file extension (e.g., 'srt', 'sub'). Defaults to 'srt'.")
    print("\nThis script applies a time delay to all subtitle files matching the given regex and file type.")
    print("A backup of the original file will be created with a .bak extension.")
    exit(1)

def time_to_ms(time_str):
    """
    Converts an SRT time string (HH:MM:SS,ms) to total milliseconds.
    Example: "01:02:03,456" -> total milliseconds
    """
    try:
        parts = re.split(r'[:,]', time_str)
        # Ensure all parts are integers before calculation
        H = int(parts[0])
        M = int(parts[1])
        S = int(parts[2])
        MS = int(parts[3])
        return (H * 3600 + M * 60 + S) * 1000 + MS
    except (ValueError, IndexError) as e:
        print(f"Warning: Could not parse time string '{time_str}'. Skipping this line. Error: {e}")
        return None # Indicate parsing failure

def ms_to_time(total_ms):
    """
    Converts total milliseconds back to an SRT time string (HH:MM:SS,ms).
    Example: 3723456 -> "01:02:03,456"
    """
    sign = ""
    if total_ms < 0:
        sign = "-"
        total_ms = abs(total_ms) # Work with absolute value for calculations

    ms = total_ms % 1000
    total_seconds = total_ms // 1000
    S = total_seconds % 60
    total_minutes = total_seconds // 60
    M = total_minutes % 60
    H = total_minutes // 60

    return f"{sign}{H:02d}:{M:02d}:{S:02d},{ms:03d}"

def main():
    # Setup argument parsing
    parser = argparse.ArgumentParser(
        description="Apply a time delay to subtitle files.",
        add_help=False # We'll handle help manually
    )
    parser.add_argument("regex", help="Regular expression to match file names")
    parser.add_argument("delay_seconds", type=float, help="Delay to apply in seconds (can be positive or negative)")
    parser.add_argument("--file_type", default="srt", help="The file extension (e.g., 'srt', 'sub'). Defaults to 'srt'.")
    parser.add_argument("--help", action="store_true", help="Show this help message and exit.")

    args = parser.parse_args()

    if args.help:
        usage()

    file_regex_pattern = args.regex
    delay_seconds = args.delay_seconds
    file_type = args.file_type

    # Convert delay from seconds (float) to milliseconds (integer)
    delay_ms = int(delay_seconds * 1000)

    print(f"Preparing to apply a delay of {delay_seconds} seconds ({delay_ms} ms) to files matching regex '{file_regex_pattern}' with extension '.{file_type}'.\n")

    # Regex for SRT timestamp lines: HH:MM:SS,ms --> HH:MM:SS,ms
    # Using \s* for flexible whitespace matching around '-->'
    srt_timestamp_pattern = re.compile(r'^(\d{2}:\d{2}:\d{2},\d{3})\s*-->\s*(\d{2}:\d{2}:\d{2},\d{3})$')

    # Iterate through files in the current directory
    processed_count = 0
    for filename in os.listdir('.'):
        # Check if the filename matches the provided regex and file type
        # Changed re.match to re.search to find the pattern anywhere in the filename
        if re.search(file_regex_pattern, filename) and filename.endswith(f".{file_type}") and os.path.isfile(filename):
            print(f"Processing file: {filename}")

            original_filepath = filename
            backup_filepath = f"{filename}.bak"
            temp_filepath = f"{filename}.tmp"

            try:
                # Create a backup of the original file
                shutil.copy2(original_filepath, backup_filepath)
                print(f"  Backup created: {backup_filepath}")

                with open(original_filepath, 'r', encoding='utf-8', errors='ignore') as f_in, \
                     open(temp_filepath, 'w', encoding='utf-8') as f_out:
                    for line in f_in:
                        match = srt_timestamp_pattern.match(line.strip()) # .strip() removes leading/trailing whitespace including newline

                        if match:
                            start_time_str, end_time_str = match.groups()

                            start_ms = time_to_ms(start_time_str)
                            end_ms = time_to_ms(end_time_str)

                            if start_ms is not None and end_ms is not None: # Proceed only if parsing was successful
                                new_start_ms = start_ms + delay_ms
                                new_end_ms = end_ms + delay_ms

                                # Ensure times do not go negative, clamp at 0
                                new_start_ms = max(0, new_start_ms)
                                new_end_ms = max(0, new_end_ms)

                                new_line = f"{ms_to_time(new_start_ms)} --> {ms_to_time(new_end_ms)}\n"
                                f_out.write(new_line)
                            else:
                                # If parsing failed, write original line (with its original newline)
                                f_out.write(line)
                        else:
                            # If not a timestamp line, write it as is (with its original newline)
                            f_out.write(line)

                # Atomically replace the original file with the modified temporary file
                os.replace(temp_filepath, original_filepath)
                print(f"  Successfully applied delay to {filename}.")
                processed_count += 1

            except Exception as e:
                print(f"  Error processing '{filename}': {e}")
                # Clean up temp file if an error occurred during processing
                if os.path.exists(temp_filepath):
                    os.remove(temp_filepath)
            finally:
                print() # Add a blank line for readability

    if processed_count == 0:
        print(f"No files found matching regex '{file_regex_pattern}' with extension '.{file_type}'.")
    else:
        print("Script finished processing all matching files.")

if __name__ == "__main__":
    main()
