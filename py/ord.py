#!/usr/bin/env python3
"""
MEME ENCYXLOPEDIA: THE FORBIDDEN UNICODE CODEX
==============================================

A comprehensive exploration of the most cursed Unicode characters
derivable through pure mathematics.

DISCLAIMER: This code is 100% mathematically correct and 200% cursed.
May cause uncontrollable laughter, existential crisis, or spontaneous
promotion to senior developer.

Author: arc x, Meme Mathematician, 2023-2025
"""

import sys

def banner():
    print("""
    🔥🔥🔥 UNICODE MEME MATHEMATICS 🔥🔥🔥
    The forbidden knowledge of character synthesis
    ---------------------------------------------
    """)

def sus_amogus():
    """The original Amogus character (ඞ) via nested functions"""
    result = chr(sum(range(ord(min(str(not()))))))
    print(f"AMOGUS: {result} via chr(sum(range(ord(min(str(not()))))))")
    return result

def hindi_nine():
    """The Hindi 9 character (९) via double negation"""
    result = chr(sum(range(ord(min(str(not(not())))))))
    print(f"HINDI NINE: {result} via chr(sum(range(ord(min(str(not(not())))))))")
    return result

def egyptian_phallus():
    """The ancient Egyptian phallus hieroglyph (𓂸) via multiple methods"""
    methods = [
        ("BOOBS - CYBERPUNK", chr(80085 - 2077)),
        ("NICE MULTIPLICATION", chr(69 * 1130 + 38)),
        ("BLAZE IT", chr(420 * 185 + 608)),
        ("EGGPLANT DIFFERENCE", chr(ord('🍆') - 49806)),
        ("BINARY", chr(0b10011000011001000)),
        ("MEANING OF LIFE", chr(42 * 1857 + 10))
    ]
    
    print("EGYPTIAN PHALLUS (𓂸) via multiple methods:")
    for name, result in methods:
        print(f"  - {name}: {result}")
    
    return chr(78008)  # The canonical code point

def fun_calculation():
    """Calculate a character using F*U*N / 69"""
    result = chr(ord('F') * ord('U') * ord('N') // 69)
    print(f"F*U*N/69: {result} via chr(ord('F') * ord('U') * ord('N') // 69)")
    return result

def emoji_dimension_gateway(start=0, count=20):
    """Generate a series of emojis using the amogus-based formula"""
    sus = ord('ඞ')  # 3486
    base = sus * 36 + 2516 - 69  # The gateway to emoji-space
    
    print(f"EMOJI DIMENSION via chr(ord('ඞ') * 36 + 2516 - 69 + i):")
    results = []
    
    for i in range(start, start + count):
        emoji = chr(base + i)
        results.append(emoji)
        print(f"  {i:3d}: {emoji}")
    
    return results

def emoji_lookup(name):
    """Look up specific emojis by name from the emoji dimension"""
    emoji_map = {
        "football": 1,      # 🏈
        "swimmer": 3,       # 🏊
        "golfer": 5,        # 🏌
        "middle_finger": 372,  # 🖕
        "poop": 224,        # 💩
        "water_drops": 182, # 💦
        "fire": 165,        # 🔥
        "eggplant": 128,    # 🍆
        "moai": 511,        # 🗿
        "thinking": 612,    # 🤔
        "cry_laugh": 578,   # 😂
        "sus_face": 615,    # 🤨
    }
    
    if name in emoji_map:
        sus = ord('ඞ')
        base = sus * 36 + 2516 - 69
        emoji = chr(base + emoji_map[name])
        print(f"{name.upper()}: {emoji} (offset {emoji_map[name]})")
        return emoji
    else:
        matching = [k for k in emoji_map.keys() if name in k]
        if matching:
            print(f"Did you mean: {', '.join(matching)}?")
        else:
            print(f"Emoji {name} not found in the known universe.")
        return None

def emoji_calculator():
    """Interactive emoji calculator"""
    sus = ord('ඞ')
    base = sus * 36 + 2516 - 69
    
    print("\nEMOJI CALCULATOR")
    print("----------------")
    print("Enter an offset (1-800) to get an emoji, or enter an emoji to get its offset.")
    print("Type 'exit' to return to main menu.\n")
    
    while True:
        user_input = input("Emoji or offset > ")
        if user_input.lower() in ['exit', 'quit', 'q']:
            break
            
        if user_input.isdigit():
            offset = int(user_input)
            if 1 <= offset <= 800:
                emoji = chr(base + offset)
                print(f"Offset {offset} = {emoji}")
            else:
                print("Offset must be between 1 and 800.")
        elif len(user_input) == 1 and 0x1F000 <= ord(user_input) <= 0x1FFFF:
            emoji_offset = ord(user_input) - base
            print(f"Emoji {user_input} = offset {emoji_offset}")
        else:
            print("Invalid input. Enter a number (1-800) or a single emoji character.")

def main_menu():
    """Display and handle the main menu"""
    banner()
    
    while True:
        print("\nMEME CHARACTER ENCYCLOPEDIA")
        print("==========================")
        print("1. Generate Amogus (ඞ)")
        print("2. Generate Hindi Nine (९)")
        print("3. Generate Egyptian Hieroglyph (𓂸)")
        print("4. F*U*N/69 Character")
        print("5. Emoji Dimension Explorer (Show range of emojis)")
        print("6. Emoji Lookup (Get specific meme emojis)")
        print("7. Interactive Emoji Calculator")
        print("8. Exit")
        
        choice = input("\nEnter your choice (1-8): ")
        
        if choice == '1':
            sus_amogus()
        elif choice == '2':
            hindi_nine()
        elif choice == '3':
            egyptian_phallus()
        elif choice == '4':
            fun_calculation()
        elif choice == '5':
            start = input("Enter starting offset (default 0): ")
            count = input("Enter number of emojis to generate (default 20): ")
            try:
                start = int(start) if start else 0
                count = int(count) if count else 20
                emoji_dimension_gateway(start, count)
            except ValueError:
                print("Please enter valid numbers.")
        elif choice == '6':
            name = input("Enter emoji name (e.g., poop, fire, middle_finger): ")
            emoji_lookup(name)
        elif choice == '7':
            emoji_calculator()
        elif choice == '8':
            print("\nExiting the Forbidden Unicode Codex...")
            print("Remember: With great power comes great responsibility.")
            sys.exit(0)
        else:
            print("Invalid choice. Please enter a number between 1 and 8.")
        
        input("\nPress Enter to continue...")

if __name__ == "__main__":
    try:
        main_menu()
    except KeyboardInterrupt:
        print("\n\nEmergency exit from the emoji dimension! Stay safe out there.")
    except Exception as e:
        print(f"\nError: {e}")
        print("The Unicode spirits are angry today. Try again later.")