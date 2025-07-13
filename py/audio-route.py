#!/usr/bin/env python3

import subprocess
import time
import sys
from datetime import datetime

connected_pairs = set()

def log(msg):
    print(f"[{datetime.now().isoformat(timespec='seconds')}] {msg}")
    sys.stdout.flush()

def list_ports(direction_flag):
    raw = subprocess.check_output(["pw-link", direction_flag]).decode().splitlines()
    ports = []
    for line in raw:
        if ':' not in line:
            continue
        node, port = line.strip().split(":", 1)
        if "monitor" in port or "midi" in node.lower():
            continue
        ports.append((node, port))
    return ports

def get_output_channels():
    return [p for p in list_ports("-o") if p[1].startswith("output_")]

def get_playback_channels():
    return [p for p in list_ports("-i") if p[1].startswith("playback_")]

def channel_name(port):
    """Get _FL or _FR suffix"""
    return port[1][-3:] if len(port[1]) >= 3 else ""

def connect(source, sink):
    src_full = f"{source[0]}:{source[1]}"
    sink_full = f"{sink[0]}:{sink[1]}"
    if (src_full, sink_full) in connected_pairs:
        return
    try:
        subprocess.run(["pw-link", src_full, sink_full],
                       check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        connected_pairs.add((src_full, sink_full))
        log(f"Connected {src_full} → {sink_full}")
    except subprocess.CalledProcessError as e:
        log(f"Failed to connect {src_full} → {sink_full}")

def route_all():
    outputs = get_output_channels()
    inputs = get_playback_channels()

    if not outputs or not inputs:
        log("No outputs or playback inputs found.")
        return

    # Match channels by suffix (_FL, _FR, etc.)
    for src in outputs:
        for sink in inputs:
            if channel_name(src) == channel_name(sink):
                connect(src, sink)

if __name__ == "__main__":
    log("Starting PipeWire auto-router with channel matching...")
    try:
        while True:
            route_all()
            time.sleep(2)
    except KeyboardInterrupt:
        log("Shutting down.")
