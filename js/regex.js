const pattern = /\/[a-zA-z]{2,}\//i;

console.log(pattern.test('/ab/')); // true
console.log(pattern.test('/ABC/')); // true
console.log(pattern.test('/Users/')); // true
console.log(pattern.test('/a/')); // false (only 1 character)
console.log(pattern.test('/123/')); // false (numbers, not letters)
console.log(pattern.test('abc')); // false (no slashes)
const pattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}\b/;

console.log(pattern.test('user@example.com')); // true
console.log(pattern.test('first.last@company-name.org')); // true
console.log(pattern.test('user+tag@domain.co.uk')); // true
console.log(pattern.test('invalid@')); // false (missing domain)
console.log(pattern.test('@domain.com')); // false (missing username)
console.log(pattern.test('user@domain')); // false (missing TLD)
const pattern = /^(?=.*([a-zA-Z])-\1)(?=.*([a-zA-Z])-\2)(?=.*([a-zA-Z])-\3).*$/;

// This string has a-a, b-b, and c-c patterns
console.log(pattern.test('a-a b-b c-c')); // true

// Different order but still has all three patterns
console.log(pattern.test('test c-c words a-a more b-b')); // true

// Missing one pattern (only a-a and b-b)
console.log(pattern.test('a-a test b-b')); // false

// Patterns are present but with characters between the pairs
console.log(pattern.test('a-test-a b-something-b c-x-c')); // false
const pattern = /de[a-zA-Z]{2}_\d{5}_/i;

console.log(pattern.test('deAB_12345_')); // true
console.log(pattern.test('DeXy_98765_')); // true (case-insensitive)
console.log(pattern.test('deC1_23456_')); // false (C1 contains a digit)
console.log(pattern.test('deABC_12345_')); // false (3 letters instead of 2)
console.log(pattern.test('de12_12345_')); // false (numbers instead of letters)
// Natural numbers: {0,1,2,3,4,5,6,7,8,9}{0,1,2,3,4,5,6,7,8,9}*
const naturalNumberPattern = /^[0-9][0-9]*$/;

console.log(naturalNumberPattern.test('0')); // true
console.log(naturalNumberPattern.test('42')); // true
console.log(naturalNumberPattern.test('007')); // true
console.log(naturalNumberPattern.test('')); // false
console.log(naturalNumberPattern.test('3.14')); // false (contains decimal)
const pattern = /^b(a|b)*b$/;

// Test cases
console.log(pattern.test('bb')); // true (simplest case)
console.log(pattern.test('bab')); // true
console.log(pattern.test('bababbab')); // true
console.log(pattern.test('ab')); // false (doesn't start with b)
console.log(pattern.test('ba')); // false (doesn't end with b)
console.log(pattern.test('b')); // false (needs at least two b's)

// Complex pattern using multiple rules:
// - Matches emails with simplified pattern
// - Uses concatenation, grouping, and iteration
const emailPattern = /^[a-zA-Z0-9]+@[a-zA-Z0-9]+\.[a-zA-Z]{2,}$/;

console.log(emailPattern.test('user@example.com')); // true
console.log(emailPattern.test('invalid@email')); // false
// Without parentheses: a + bc matches 'a' or 'bc'
const withoutParens = /a|bc/;
console.log(withoutParens.test('a')); // true
console.log(withoutParens.test('bc')); // true

// With parentheses: (a + b)c matches 'ac' or 'bc'
const withParens = /(a|b)c/;
console.log(withParens.test('ac')); // true
console.log(withParens.test('bc')); // true
console.log(withParens.test('a')); // false
// P = 'a', P* matches any number of 'a's, including none
const starPattern = /a*/;
console.log(starPattern.test('')); // true (0 occurrences)
console.log(starPattern.test('a')); // true (1 occurrence)
console.log(starPattern.test('aaa')); // true (3 occurrences)
console.log(starPattern.test('b')); // true (0 occurrences of 'a')

// More meaningful example
const wordPattern = /\w*/;
console.log(wordPattern.test('hello')); // true
// P = 'hello', Q = 'world'
const concatPattern = /helloworld/;
console.log(concatPattern.test('helloworld')); // true
console.log(concatPattern.test('hello world')); // false (has space)

// More practical example with word boundaries
const betterPattern = /hello\s*world/;
console.log(betterPattern.test('hello world')); // true
// P = 'dog', Q = 'cat'
const unionPattern = /dog|cat/;
console.log(unionPattern.test('dog')); // true
console.log(unionPattern.test('cat')); // true

// P = 'dog', Q = 'cat'
const unionPattern = /dog|cat/;
console.log(unionPattern.test('dog')); // true
console.log(unionPattern.test('cat')); // true
console.log(unionPattern.test('bird')); // false
// Simple character matching
const pattern = /a/;
console.log(pattern.test('apple')); // true
console.log(pattern.test('banana')); // false - 'b' doesn't contain 'a'

// Empty string pattern matches everything
const emptyString = /(?:)/;
console.log(emptyString.test('')); // true
console.log(emptyString.test('anything')); // true

// Empty set (never matches)
const emptySet = /[^\s\S]/;
console.log(emptySet.test('')); // false
console.log(emptySet.test('anything')); // false
