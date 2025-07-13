#!/usr/bin/env python3
"""
KoiKatsu Card Data Manipulator
Because manually copying sliders is for peasants
"""

import struct
import base64
import json
import argparse
from pathlib import Path
import re

class KKCardEditor:
    def __init__(self):
        self.card_data = {}
        self.png_data = b''
        self.metadata_start = None
        
    def load_card(self, filepath):
        """Load KK card and extract embedded data"""
        with open(filepath, 'rb') as f:
            data = f.read()
            
        # Find PNG end marker and KK data start
        png_end = data.find(b'IEND')
        if png_end == -1:
            raise ValueError("Not a valid PNG file")
            
        self.png_data = data[:png_end + 8]  # Include IEND chunk
        
        # KK data starts after PNG
        kk_start = png_end + 8
        if kk_start < len(data):
            self.metadata_start = kk_start
            self._parse_kk_data(data[kk_start:])
            
    def _parse_kk_data(self, data):
        """Parse the KoiKatsu metadata"""
        try:
            # Look for the coordinate data sections
            coord_pattern = rb'coordinate.*?version'
            matches = re.finditer(coord_pattern, data, re.DOTALL)
            
            for match in matches:
                # Extract coordinate block
                coord_data = data[match.start():match.end() + 100]
                self._extract_sliders(coord_data)
                
        except Exception as e:
            print(f"Warning: Could not parse all KK data: {e}")
    
    def _extract_sliders(self, data):
        """Extract slider values from coordinate data"""
        # Face shape values (typically 32-bit floats)
        face_pattern = rb'shapeValueFace'
        body_pattern = rb'shapeValueBody'
        
        # Find slider arrays
        if face_pattern in data:
            face_idx = data.find(face_pattern)
            self.card_data['face_sliders'] = self._extract_float_array(data, face_idx + 20)
            
        if body_pattern in data:
            body_idx = data.find(body_pattern)
            self.card_data['body_sliders'] = self._extract_float_array(data, body_idx + 20)
    
    def _extract_float_array(self, data, start_idx, max_count=50):
        """Extract array of float values"""
        floats = []
        for i in range(max_count):
            try:
                offset = start_idx + (i * 4)
                if offset + 4 > len(data):
                    break
                value = struct.unpack('<f', data[offset:offset + 4])[0]
                # Sanity check - slider values should be between -1 and 1 typically
                if -2.0 <= value <= 2.0:
                    floats.append(value)
                else:
                    break
            except:
                break
        return floats
    
    def extract_textures(self, data):
        """Extract base64 encoded textures"""
        textures = {}
        
        # Look for common texture patterns
        texture_patterns = [
            rb'overlay.*?base64',
            rb'texture.*?base64',
            rb'material.*?base64'
        ]
        
        for pattern in texture_patterns:
            matches = re.finditer(pattern, data, re.DOTALL)
            for match in matches:
                # Extract base64 data following the pattern
                tex_data = self._extract_base64(data, match.end())
                if tex_data:
                    textures[f"texture_{len(textures)}"] = tex_data
                    
        return textures
    
    def _extract_base64(self, data, start_idx):
        """Extract base64 encoded data"""
        # Look for base64 pattern
        b64_pattern = rb'[A-Za-z0-9+/=]{100,}'
        match = re.search(b64_pattern, data[start_idx:start_idx + 10000])
        
        if match:
            try:
                b64_data = match.group(0).decode('ascii')
                # Validate base64
                base64.b64decode(b64_data)
                return b64_data
            except:
                pass
        return None
    
    def copy_sliders_to_card(self, source_card, target_card_path, output_path):
        """Copy slider values from source to target card"""
        # Load target card
        target = KKCardEditor()
        target.load_card(target_card_path)
        
        # Read target card data
        with open(target_card_path, 'rb') as f:
            target_data = bytearray(f.read())
        
        # Replace slider values
        if 'face_sliders' in source_card.card_data:
            target_data = self._replace_sliders(target_data, 'face', source_card.card_data['face_sliders'])
        
        if 'body_sliders' in source_card.card_data:
            target_data = self._replace_sliders(target_data, 'body', source_card.card_data['body_sliders'])
        
        # Write modified card
        with open(output_path, 'wb') as f:
            f.write(target_data)
    
    def _replace_sliders(self, data, slider_type, new_values):
        """Replace slider values in card data"""
        pattern = rb'shapeValue' + slider_type.capitalize().encode()
        
        idx = data.find(pattern)
        if idx != -1:
            # Replace float values
            start_idx = idx + len(pattern) + 20  # Offset to actual data
            
            for i, value in enumerate(new_values):
                offset = start_idx + (i * 4)
                if offset + 4 <= len(data):
                    # Pack as little-endian float
                    data[offset:offset + 4] = struct.pack('<f', value)
        
        return data
    
    def export_data(self, output_file):
        """Export extracted data to JSON"""
        with open(output_file, 'w') as f:
            json.dump(self.card_data, f, indent=2)
    
    def print_stats(self):
        """Print extracted data stats"""
        print(f"Face sliders: {len(self.card_data.get('face_sliders', []))}")
        print(f"Body sliders: {len(self.card_data.get('body_sliders', []))}")
        
        if 'face_sliders' in self.card_data:
            print(f"Face slider range: {min(self.card_data['face_sliders']):.3f} to {max(self.card_data['face_sliders']):.3f}")
        
        if 'body_sliders' in self.card_data:
            print(f"Body slider range: {min(self.card_data['body_sliders']):.3f} to {max(self.card_data['body_sliders']):.3f}")

def main():
    parser = argparse.ArgumentParser(description="KoiKatsu Card Data Manipulator")
    parser.add_argument('action', choices=['extract', 'copy', 'merge'])
    parser.add_argument('--source', required=True, help="Source card file")
    parser.add_argument('--target', help="Target card file (for copy/merge)")
    parser.add_argument('--output', required=True, help="Output file")
    parser.add_argument('--data-only', action='store_true', help="Extract data only, no image manipulation")
    
    args = parser.parse_args()
    
    if args.action == 'extract':
        editor = KKCardEditor()
        editor.load_card(args.source)
        editor.print_stats()
        editor.export_data(args.output)
        print(f"Data extracted to {args.output}")
        
    elif args.action == 'copy':
        if not args.target:
            print("Target card required for copy operation")
            return
            
        source = KKCardEditor()
        source.load_card(args.source)
        
        source.copy_sliders_to_card(source, args.target, args.output)
        print(f"Sliders copied from {args.source} to {args.target}")
        print(f"Result saved as {args.output}")

if __name__ == "__main__":
    main()