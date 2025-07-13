#!/usr/bin/env python3
import os
import re
import subprocess
import sys
from pathlib import Path

def create_llm_prompt(md_file_path):
    with open(md_file_path, 'r') as file:
        content = file.read()
    
    # Extract file paths from the md file
    file_paths = re.findall(r'@(.*?)$', content, re.MULTILINE)
    
    output_content = []
    
    for path in file_paths:
        path = path.strip()
        
        # Handle tree command
        if path.startswith('tree '):
            directory = path[5:].strip()
            try:
                # Execute tree command
                result = subprocess.run(['tree', directory], capture_output=True, text=True)
                tree_output = result.stdout
                output_content.append(f"Directory structure of {directory}:\n```\n{tree_output}\n```\n")
            except Exception as e:
                output_content.append(f"Error getting tree for {directory}: {str(e)}\n")
        else:
            # Handle file paths
            try:
                if os.path.isfile(path):
                    with open(path, 'r') as f:
                        file_content = f.read()
                    output_content.append(f"File: {path}\n```\n{file_content}\n```\n")
                else:
                    output_content.append(f"File not found: {path}\n")
            except Exception as e:
                output_content.append(f"Error reading {path}: {str(e)}\n")
    
    # Add the instruction
    output_content.append("DO NOT EXPLAIN YOUR CODE, ONLY OUTPUT THE FULLY FIXED FILES IF THEY NEED AN UPDATE.")
    
    # Generate the output markdown
    output_md = "\n".join(output_content)
    
    # Copy to clipboard
    try:
        process = subprocess.Popen(['xclip', '-selection', 'clipboard'], stdin=subprocess.PIPE)
        process.communicate(output_md.encode())
        print("Content copied to clipboard!")
    except Exception as e:
        print(f"Failed to copy to clipboard: {str(e)}")
        # Save to file as fallback
        output_file = Path(md_file_path).with_suffix('.output.md')
        with open(output_file, 'w') as f:
            f.write(output_md)
        print(f"Content saved to {output_file}")
    
    return output_md

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py path/to/input.md")
        sys.exit(1)
    
    md_file = sys.argv[1]
    result = create_llm_prompt(md_file)
    print("LLM prompt created successfully!")