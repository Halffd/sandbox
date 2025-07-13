#!/usr/bin/env python3
import sys
import re
import argparse
from pathlib import Path
import pyperclip

def detect_subtitle_format(content):
    """Detect subtitle format from content."""
    content_upper = content.upper()
    
    if 'WEBVTT' in content_upper or content_upper.startswith('WEBVTT'):
        return 'vtt'
    elif '[SCRIPT INFO]' in content_upper or '[V4+ STYLES]' in content_upper:
        return 'ass'
    elif re.search(r'^\d{2}:\d{2}:\d{2}[,.]\d{3} --> \d{2}:\d{2}:\d{2}[,.]\d{3}$', 
                   content, flags=re.MULTILINE):
        return 'srt'
    else:
        return 'unknown'

def vtt_to_srt(content, verbose=False):
    """Convert VTT to SRT format - now with extra cursed VTT handling!"""
    if verbose:
        print("Converting VTT to SRT format...", file=sys.stderr)
    
    lines = content.splitlines()
    srt_lines = []
    subtitle_counter = 1
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        
        # Skip WEBVTT header, metadata, and empty lines
        if (line.startswith('WEBVTT') or line.startswith('NOTE') or 
            line.startswith('STYLE') or line.startswith('REGION') or not line):
            i += 1
            continue
        
        # Look for timestamp lines (with or without positioning)
        if '-->' in line:
            # Extract just the timestamp part, ignore positioning crap
            timestamp_match = re.search(r'(\d{2}:\d{2}:\d{2}\.\d{3}) --> (\d{2}:\d{2}:\d{2}\.\d{3})', line)
            if timestamp_match:
                start_time, end_time = timestamp_match.groups()
                # Convert VTT timestamp format (.xxx) to SRT (,xxx)
                timestamp_line = f"{start_time.replace('.', ',')} --> {end_time.replace('.', ',')}"
                
                # Collect all text for this timestamp
                i += 1
                text_lines = []
                current_timestamp = (start_time, end_time)
                
                # Keep collecting text until we hit another timestamp or empty line
                while i < len(lines):
                    next_line = lines[i].strip()
                    
                    # Check if this is another timestamp line with same timing
                    if '-->' in next_line:
                        next_timestamp_match = re.search(r'(\d{2}:\d{2}:\d{2}\.\d{3}) --> (\d{2}:\d{2}:\d{2}\.\d{3})', next_line)
                        if next_timestamp_match:
                            next_start, next_end = next_timestamp_match.groups()
                            # If same timestamp, continue collecting text
                            if (next_start, next_end) == current_timestamp:
                                i += 1
                                continue
                            else:
                                # Different timestamp, stop collecting
                                break
                    
                    if not next_line:
                        i += 1
                        break
                    
                    # Clean up the text
                    clean_text = next_line
                    # Remove VTT positioning and styling tags
                    clean_text = re.sub(r'position:\d+%', '', clean_text)
                    clean_text = re.sub(r'line:\d+%?', '', clean_text)
                    clean_text = re.sub(r'align:\w+', '', clean_text)
                    clean_text = re.sub(r'size:\d+%', '', clean_text)
                    # Remove HTML-style tags
                    clean_text = re.sub(r'<[^>]*>', '', clean_text)
                    # Remove weird Unicode chars and brackets
                    clean_text = re.sub(r'[​\u200B-\u200D\uFEFF]', '', clean_text)  # Zero-width chars
                    clean_text = re.sub(r'^\s*[​\[\]]+\s*', '', clean_text)
                    clean_text = re.sub(r'\s*[​\[\]]+\s*$', '', clean_text)
                    clean_text = clean_text.strip()
                    
                    if clean_text:
                        text_lines.append(clean_text)
                    
                    i += 1
                
                # Only add subtitle if we have actual text
                if text_lines:
                    srt_lines.append(str(subtitle_counter))
                    srt_lines.append(timestamp_line)
                    srt_lines.extend(text_lines)
                    srt_lines.append('')  # Empty line separator
                    subtitle_counter += 1
                
                continue
        
        i += 1
    
    return '\n'.join(srt_lines)
def ass_to_srt(content, verbose=False):
    """Convert ASS/SSA to SRT format."""
    if verbose:
        print("Converting ASS to SRT format...", file=sys.stderr)
    
    lines = content.splitlines()
    srt_lines = []
    subtitle_counter = 1
    
    # Find the events section
    in_events = False
    for line in lines:
        line = line.strip()
        
        if line.upper().startswith('[EVENTS]'):
            in_events = True
            continue
        elif line.startswith('[') and in_events:
            # New section, stop processing
            break
        elif not in_events:
            continue
        
        # Process dialogue lines
        if line.lower().startswith('dialogue:'):
            parts = line.split(',', 9)  # Split into max 10 parts
            if len(parts) >= 10:
                start_time = parts[1].strip()
                end_time = parts[2].strip()
                text = parts[9].strip()
                
                # Convert ASS time format (H:MM:SS.CC) to SRT (HH:MM:SS,mmm)
                def convert_ass_time(time_str):
                    # ASS format: H:MM:SS.CC where CC is centiseconds
                    match = re.match(r'(\d+):(\d{2}):(\d{2})\.(\d{2})', time_str)
                    if match:
                        h, m, s, cs = match.groups()
                        ms = int(cs) * 10  # Convert centiseconds to milliseconds
                        return f"{int(h):02d}:{m}:{s},{ms:03d}"
                    return time_str
                
                start_srt = convert_ass_time(start_time)
                end_srt = convert_ass_time(end_time)
                
                # Clean up ASS formatting
                text = re.sub(r'\{[^}]*\}', '', text)  # Remove ASS tags
                text = text.replace('\\N', '\n')  # Convert line breaks
                text = text.replace('\\n', '\n')
                
                if text.strip():
                    srt_lines.append(str(subtitle_counter))
                    srt_lines.append(f"{start_srt} --> {end_srt}")
                    srt_lines.append(text.strip())
                    srt_lines.append('')
                    subtitle_counter += 1
    
    return '\n'.join(srt_lines)

def is_srt_content(content):
    """Check if content appears to be in SRT format by looking for timestamp patterns."""
    return bool(re.search(r'^\d{2}:\d{2}:\d{2}[,.]\d{3} --> \d{2}:\d{2}:\d{2}[,.]\d{3}$', 
                         content, flags=re.MULTILINE))

def clean_srt_text(content, verbose=False):
    """Clean SRT formatted text by removing timestamps, numbers, and formatting tags."""
    if verbose:
        print("Processing SRT content...", file=sys.stderr)
        original_line_count = len(content.splitlines())
        print(f"Original line count: {original_line_count}", file=sys.stderr)

    # Remove SRT line numbers and timestamps
    cleaned = re.sub(
        r'^\d+\s*$|^\d{2}:\d{2}:\d{2}[,.]\d{3} --> \d{2}:\d{2}:\d{2}[,.]\d{3}\s*$', 
        '', 
        content, 
        flags=re.MULTILINE
    )
    
    # Remove HTML tags and formatting
    cleaned = re.sub(r'<[^>]+>', '', cleaned)  # HTML tags
    cleaned = re.sub(r'\{[^}]+\}', '', cleaned)  # {formatting}
    cleaned = re.sub(r'\\[a-zA-Z0-9_]+', '', cleaned)  # \commands
    cleaned = re.sub(r'^.*\-\-\>.*$', '', cleaned)  # arrow lines
    
    # Remove empty lines, trim whitespace, AND deduplicate
    seen_lines = set()
    unique_lines = []
    
    for line in cleaned.splitlines():
        stripped_line = line.strip()
        if stripped_line and stripped_line not in seen_lines:
            seen_lines.add(stripped_line)
            unique_lines.append(stripped_line)
    
    cleaned = '\n'.join(unique_lines)
    
    if verbose:
        cleaned_line_count = len(cleaned.splitlines())
        print(f"Cleaned line count: {cleaned_line_count}", file=sys.stderr)
        reduction = original_line_count - cleaned_line_count
        print(f"Removed {reduction} lines of metadata and duplicates", file=sys.stderr)
        duplicate_count = len([line.strip() for line in content.splitlines() if line.strip()]) - len(unique_lines)
        print(f"Removed {duplicate_count} duplicate lines", file=sys.stderr)
    
    return cleaned
def get_input_content(input_source, verbose=False):
    """Get content from either file or stdin."""
    if input_source == '-':
        if verbose:
            print("Reading from standard input...", file=sys.stderr)
        return sys.stdin.read()
    else:
        if verbose:
            print(f"Reading from file: {input_source}", file=sys.stderr)
        try:
            with open(input_source, 'r', encoding='utf-8') as f:
                return f.read()
        except UnicodeDecodeError:
            if verbose:
                print("UTF-8 decode failed, trying with fallback encoding...", file=sys.stderr)
            with open(input_source, 'r', encoding='latin-1') as f:
                return f.read()

def write_output(content, output_dest, verbose=False):
    """Write output to either stdout, clipboard, or file."""
    if output_dest == '-':
        if verbose:
            print("Writing to standard output...", file=sys.stderr)
        print(content)
    elif output_dest.lower() == 'clipboard':
        if verbose:
            print("Attempting to write to clipboard...", file=sys.stderr)
        try:
            import pyperclip
            pyperclip.copy(content)
            if verbose:
                print("Successfully copied to clipboard", file=sys.stderr)
        except ImportError:
            print("Error: pyperclip module required for clipboard support", file=sys.stderr)
            print("Install with: pip install pyperclip", file=sys.stderr)
            sys.exit(1)
    else:
        if verbose:
            print(f"Writing to file: {output_dest}", file=sys.stderr)
        try:
            with open(output_dest, 'w', encoding='utf-8') as f:
                f.write(content)
            if verbose:
                print(f"Successfully wrote to {output_dest}", file=sys.stderr)
        except IOError as e:
            print(f"Error writing to file: {e}", file=sys.stderr)
            sys.exit(1)

def main():
    parser = argparse.ArgumentParser(
        description='Extract clean text from subtitle files (SRT/VTT/ASS)',
        epilog='Examples:\n'
               '  srttext.py input.srt\n'
               '  srttext.py input.vtt output.txt\n'
               '  srttext.py input.ass clipboard\n'
               '  cat input.srt | srttext.py -\n'
               '  srttext.py input.vtt clipboard --keep-srt',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        'input', 
        help='Input file or "-" for stdin'
    )
    parser.add_argument(
        'output', 
        nargs='?', 
        default='-',
        help='Output file, "clipboard", or "-" for stdout (default: -)'
    )
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Show detailed processing information'
    )
    parser.add_argument(
        '--keep-srt',
        action='store_true',
        help='Keep SRT format instead of extracting plain text'
    )
    parser.add_argument(
        '--force',
        action='store_true',
        help='Force processing even if format not detected'
    )
    
    args = parser.parse_args()
    
    if args.verbose:
        print(f"Starting processing with arguments: {vars(args)}", file=sys.stderr)
    
    # Get input content
    content = get_input_content(args.input, args.verbose)
    
    if args.verbose:
        print(f"Read {len(content)} characters from input", file=sys.stderr)
    
    # Detect format and convert
    format_type = detect_subtitle_format(content)
    
    if args.verbose:
        print(f"Detected format: {format_type}", file=sys.stderr)
    
    # Convert to SRT first if needed
    if format_type == 'vtt':
        content = vtt_to_srt(content, args.verbose)
    elif format_type == 'ass':
        content = ass_to_srt(content, args.verbose)
    elif format_type == 'unknown' and not args.force:
        print("Warning: Unknown subtitle format detected", file=sys.stderr)
        if not args.force:
            print("Use --force to process anyway", file=sys.stderr)
    
    # Process content
    if args.keep_srt:
        processed = content
        if args.verbose:
            print("Keeping SRT format as requested", file=sys.stderr)
    else:
        processed = clean_srt_text(content, args.verbose)
    
    # Auto-copy to clipboard (like your original)
    pyperclip.copy(processed)
    
    # Write output
    write_output(processed, args.output, args.verbose)

if __name__ == '__main__':
    main()
