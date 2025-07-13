import re

# Demonstrating the power of regular expressions in Python
def regex_basics():
    print("=== REGULAR EXPRESSIONS IN PYTHON ===")
    print("Regular expressions (regex) are powerful patterns that can match text")
    
    # Simple pattern matching
    pattern = r"01"
    text = "010101"
    matches = re.findall(pattern, text)
    print(f"Finding '{pattern}' in '{text}': {matches}")
    
    # Demonstrating Kleene closure (*)
    # This is equivalent to the simple closure in the text
    pattern = r"(01)*"
    test_strings = ["", "01", "0101", "010101"]
    for string in test_strings:
        match = re.fullmatch(pattern, string)
        print(f"'{string}' matches '{pattern}': {match is not None}")
    
    # Demonstrating Kleene closure on alphabet
    # This would match any string containing only 0s and 1s
    alphabet_pattern = r"[01]*"
    test_strings = ["", "0", "1", "01", "00", "11", "010", "011", "100"]
    for string in test_strings:
        match = re.fullmatch(alphabet_pattern, string)
        print(f"'{string}' matches alphabet pattern: {match is not None}")

# Implementing regular sets as Python sets
def regular_sets():
    print("\n=== REGULAR SETS IN PYTHON ===")
    
    # Empty set
    empty_set = set()
    print(f"Empty set: {empty_set}")
    
    # Lambda set (empty string)
    lambda_set = {""}
    print(f"Lambda set: {lambda_set}")
    
    # Single character sets
    sigma = {"0", "1"}
    print(f"Alphabet Σ: {sigma}")
    
    # Regular set operations
    L1 = {"0"}
    L2 = {"1"}
    L3 = set(["0" * i for i in range(10)])  # {λ, 0, 00, 000, ...}
    L4 = set(["1" * i for i in range(10)])  # {λ, 1, 11, 111, ...}
    
    # L5 = L3 · L4 (concatenation)
    L5 = {x + y for x in L3 for y in L4}
    
    print(f"L1 = {L1}")
    print(f"L2 = {L2}")
    print(f"L3 = {L3}")
    print(f"L4 = {L4}")
    print(f"L5 (L3·L4) = {L5}")
    
    # Regular expression for L5: 0*1*
    L5_pattern = r"0*1*"
    test_strings = ["", "0", "1", "01", "001", "0011"]
    for string in test_strings:
        match = re.fullmatch(L5_pattern, string)
        print(f"'{string}' matches pattern '{L5_pattern}': {match is not None}")

# Example demonstrating regex operations in Python
def regex_operations():
    print("\n=== REGEX OPERATIONS IN PYTHON ===")
    
    # Union using | (equivalent to + in the text)
    union_pattern = r"cat|dog"
    print(f"'cat' matches '{union_pattern}': {bool(re.match(union_pattern, 'cat'))}")
    print(f"'dog' matches '{union_pattern}': {bool(re.match(union_pattern, 'dog'))}")
    print(f"'bird' matches '{union_pattern}': {bool(re.match(union_pattern, 'bird'))}")
    
    # Concatenation (implied in regex)
    concat_pattern = r"ab"
    print(f"'ab' matches '{concat_pattern}': {bool(re.match(concat_pattern, 'ab'))}")
    
    # Kleene closure (*) - zero or more occurrences
    kleene_pattern = r"a*"
    for test in ["", "a", "aa", "aaa"]:
        print(f"'{test}' matches '{kleene_pattern}': {bool(re.fullmatch(kleene_pattern, test))}")

if __name__ == "__main__":
    regex_basics()
    regular_sets()
    regex_operations()