#!/usr/bin/env python3
'''
Cursor Trial Reset Tool

This script resets the device IDs in Cursor's configuration file to generate a new random device ID.

Repository: https://github.com/ultrasev/cursor-reset
Author: @ultrasev
Created: 10/Dec/2024
'''

import json
import os
import shutil
import uuid
from datetime import datetime
from pathlib import Path
import platform
from requests_html import HTMLSession
import random
import string


def backup_file(file_path: str):
    """Create a timestamped backup of the given file."""
    if os.path.exists(file_path):
        backup_path = f"{file_path}.backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        shutil.copy2(file_path, backup_path)


def get_storage_file():
    """Determine the storage file location based on the operating system."""
    system = platform.system()
    if system == "Windows":
        return Path(os.getenv("APPDATA")) / "Cursor" / "User" / "globalStorage" / "storage.json"
    elif system == "Darwin":  # macOS
        return Path(os.path.expanduser("~")) / "Library" / "Application Support" / "Cursor" / "User" / "globalStorage" / "storage.json"
    elif system == "Linux":
        return Path(os.path.expanduser("~")) / ".config" / "Cursor" / "User" / "globalStorage" / "storage.json"
    else:
        raise OSError(f"Unsupported operating system: {system}")


def reset_cursor_id():
    storage_file = get_storage_file()
    storage_file.parent.mkdir(parents=True, exist_ok=True)
    backup_file(storage_file)

    if not storage_file.exists():
        data = {}
    else:
        with open(storage_file, 'r', encoding='utf-8') as f:
            data = json.load(f)

    machine_id = os.urandom(32).hex()
    mac_machine_id = os.urandom(32).hex()
    dev_device_id = str(uuid.uuid4())

    data["telemetry.machineId"] = machine_id
    data["telemetry.macMachineId"] = mac_machine_id
    data["telemetry.devDeviceId"] = dev_device_id

    with open(storage_file, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2)

    print("🎉 Device IDs have been successfully reset. The new device IDs are: \n")
    print(
        json.dumps(
            {
                "machineId": machine_id,
                "macMachineId": mac_machine_id,
                "devDeviceId": dev_device_id,
            },
            indent=2))

# Function to generate a random name
def generate_random_name(length=8):
    letters = string.ascii_lowercase
    return ''.join(random.choice(letters) for i in range(length))

# Function to generate a temporary email
def generate_temp_email():
    return f"{generate_random_name()}@example.com"  # Replace with a real temp email service

# Create a session
session = HTMLSession()

# Generate random user data
first_name = generate_random_name()
last_name = generate_random_name()
email = generate_temp_email()

# Sign-up URL
signup_url = "https://authenticator.cursor.sh/sign-up"

# Form data
payload = {
    "first_name": first_name,
    "last_name": last_name,
    "email": email
}

# Send POST request to sign up
response = session.post(signup_url, data=payload)

# Check if the sign-up was successful
if response.status_code == 200:
    print(f"Account created successfully! Email: {email}")
else:
    print(f"Failed to create account. Status code: {response.status_code}")


if __name__ == "__main__":
    reset_cursor_id()
