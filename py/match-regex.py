import re

def analyze_text(text):
    match text:
        case str() if re.match(r'^https?://(?:[\w-]+\.)+[\w-]+(?:/[\w./?%&=]*)?$', text):
            return "Valid URL"
        case str() if re.match(r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$', text):
            return "Valid email address"
        case str() if re.match(r'^\+?[0-9]{1,3}[-.\s]?\(?[0-9]{1,4}\)?[-.\s]?[0-9]{1,4}[-.\s]?[0-9]{1,9}$', text):
            return "Valid phone number"
        case _:
            return "Unknown format"

# Test cases
print(analyze_text("https://python.org"))  # Valid URL
print(analyze_text("user@example.com"))    # Valid email address
print(analyze_text("+1-800-555-4567"))     # Valid phone number
print(analyze_text("invalid"))             # Unknown format