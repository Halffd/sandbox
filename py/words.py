def generate_self_referencing_sentence():
    # Dictionary to convert numbers to words
    num_to_word = {
        0: "zero", 1: "one", 2: "two", 3: "three", 4: "four", 5: "five",
        6: "six", 7: "seven", 8: "eight", 9: "nine", 10: "ten",
        11: "eleven", 12: "twelve", 13: "thirteen", 14: "fourteen", 15: "fifteen",
        16: "sixteen", 17: "seventeen", 18: "eighteen", 19: "nineteen", 20: "twenty",
        21: "twenty-one", 22: "twenty-two", 23: "twenty-three", 24: "twenty-four", 25: "twenty-five",
        26: "twenty-six", 27: "twenty-seven", 28: "twenty-eight", 29: "twenty-nine", 30: "thirty",
        31: "thirty-one", 32: "thirty-two", 33: "thirty-three", 34: "thirty-four", 35: "thirty-five",
        40: "forty", 50: "fifty", 60: "sixty", 70: "seventy", 80: "eighty", 90: "ninety"
    }
    
    # For numbers greater than 20 not explicitly defined
    def number_to_words(n):
        if n in num_to_word:
            return num_to_word[n]
        if n > 20 and n < 100:
            tens, ones = divmod(n, 10)
            if ones == 0:
                return num_to_word[tens * 10]
            return f"{num_to_word[tens * 10]}-{num_to_word[ones]}"
        return str(n)  # Fallback for larger numbers
    
    # Initial template for the sentence
    template = "This sentence contains {A} A's, {B} B, {C} C's, {D} D's, {E} E's, {F} F's, {G} G's, {H} H's, {I} I's, {J} J, {K} K, {L} L's, {M} M, {N} N's, {O} O's, {P} P, {Q} Q, {R} R's, {S} S's, {T} T's, {U} U, {V} V's, {W} W's, {X} X's, {Y} Y's, and {Z} Z."
    
    # Initial guess for counts (will be refined iteratively)
    counts = {chr(65+i): 0 for i in range(26)}  # Initialize all letters to 0
    
    # Iteratively refine our sentence until it's self-consistent
    max_iterations = 15
    for _ in range(max_iterations):
        # Create the sentence with current counts
        sentence = template.format(
            A=number_to_words(counts['A']), B=number_to_words(counts['B']), 
            C=number_to_words(counts['C']), D=number_to_words(counts['D']),
            E=number_to_words(counts['E']), F=number_to_words(counts['F']),
            G=number_to_words(counts['G']), H=number_to_words(counts['H']),
            I=number_to_words(counts['I']), J=number_to_words(counts['J']),
            K=number_to_words(counts['K']), L=number_to_words(counts['L']),
            M=number_to_words(counts['M']), N=number_to_words(counts['N']),
            O=number_to_words(counts['O']), P=number_to_words(counts['P']),
            Q=number_to_words(counts['Q']), R=number_to_words(counts['R']),
            S=number_to_words(counts['S']), T=number_to_words(counts['T']),
            U=number_to_words(counts['U']), V=number_to_words(counts['V']),
            W=number_to_words(counts['W']), X=number_to_words(counts['X']),
            Y=number_to_words(counts['Y']), Z=number_to_words(counts['Z'])
        )
        
        # Count the actual occurrences in the sentence
        new_counts = {chr(65+i): 0 for i in range(26)}
        for char in sentence.upper():
            if 'A' <= char <= 'Z':
                new_counts[char] += 1
        
        # Check if we've reached a fixed point
        if all(counts[char] == new_counts[char] for char in counts):
            return sentence
        
        counts = new_counts
    
    return "Failed to converge after " + str(max_iterations) + " iterations"

# Generate and print the self-referencing sentence
print(generate_self_referencing_sentence())