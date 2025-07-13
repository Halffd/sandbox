#!/usr/bin/env python3
import subprocess, sys
from datetime import datetime

def log(msg):
    print(f"[{datetime.now().isoformat(timespec='seconds')}] {msg}", file=sys.stderr)

# Step 1: get sink names
out = subprocess.check_output(["pactl", "list", "short", "sinks"]).decode().splitlines()
# Ex: ['0\talsa_output. ... analog-stereo\t ...', '1\talsa_output.usb-Realtek_HECATE_G30_S-00.analog-stereo\t ...']
sinks = [line.split()[1] for line in out]
if len(sinks) < 2:
    log("Need at least two sinks for combining.")
    sys.exit(1)

# Step 2: build slaves list comma-separated
slaves = ",".join(sinks)
# Step 3: load module
cmd = ["pactl", "load-module", "module-combine-sink",
       f"sink_name=combined", f"slaves={slaves}"]
try:
    module_id = subprocess.check_output(cmd).strip().decode()
    log(f"Loaded module-combine-sink id={module_id}, slaves={slaves}")
    # Step 4: set it as default
    subprocess.run(["pactl", "set-default-sink", "combined"], check=True)
    log("Set 'combined' as default sink.")
except subprocess.CalledProcessError as e:
    log(f"Failed to load module-combine-sink: {e}")
    sys.exit(1)
