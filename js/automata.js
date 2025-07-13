function simpleAutomaton(input) {
  // Start in state q0
  let state = 'q0';
  
  // Process each character
  for (const char of input) {
    if (state === 'q0' && char === '0') {
      state = 'q0'; // Stay in q0
    } else if (state === 'q0' && char === '1') {
      state = 'q1'; // Move to q1
    } else if (state === 'q1' && char === '0') {
      state = 'q0'; // Move back to q0
    } else if (state === 'q1' && char === '1') {
      state = 'q1'; // Stay in q1
    }
  }
  
  // Check if we ended in an accepting state (q0)
  return state === 'q0';
}

// Test it
console.log(simpleAutomaton('0110')); // true
console.log(simpleAutomaton('01011')); // falseNow let's implement the automaton with output that we just discussed:

function outputAutomaton(input) {
  // Start in state q1
  let state = 'q1';
  let output = '';
  
  // Process each character
  for (const char of input) {
    if (state === 'q1') {
      if (char === '0') {
        state = 'q1'; // Stay in q1
        output += '1'; // q1 outputs 1
      } else if (char === '1') {
        state = 'q2'; // Move to q2
        output += '0'; // q2 outputs 0
      }
    } else if (state === 'q2') {
      if (char === '0') {
        state = 'q2'; // Stay in q2
        output += '0'; // q2 outputs 0
      } else if (char === '1') {
        state = 'q1'; // Move to q1
        output += '1'; // q1 outputs 1
      }
    }
  }
  
  // Final output depends on the state we ended in
  const accepted = state === 'q1';
  
  return {
    accepted,
    output,
    finalState: state
  };
}

// Test it
console.log(outputAutomaton('0110')); 
// { accepted: true, output: '1011', finalState: 'q1' }Implementing a two-way automaton is more complex, as we need to keep track of position:

function twoWayAutomaton(input) {
  // Start in state A at position 0
  let state = 'A';
  let position = 0;
  let steps = [];
  
  // Add boundaries to make edge detection easier
  const paddedInput = input;
  
  // Run until we accept or reject
  while (position >= 0 && position < paddedInput.length) {
    const char = paddedInput[position];
    steps.push(`State ${state}, reading '${char}' at position ${position}`);
    
    // Apply transitions based on current state and character
    if (state === 'A' && char === '1') {
      state = 'B';
      position++; // Move right
    } else if (state === 'B' && char === '0') {
      state = 'B';
      position++; // Move right
    } else if (state === 'B' && char === '1') {
      state = 'C';
      position--; // Move left
    } else if (state === 'C' && char === '0') {
      state = 'A';
      position++; // Move right
    } else {
      // No valid transition or we've reached the end
      break;
    }
  }
  
  // Check if we ended in the accepting state B
  return {
    accepted: state === 'B',
    steps,
    finalState: state
  };
}

// Test it
console.log(twoWayAutomaton('101001').accepted); // true
console.log(twoWayAutomaton('1010').accepted); // false// Regex for "strings ending in 0"
const endsInZeroRegex = /[01]*0$/;

// Test it
console.log(endsInZeroRegex.test('0110')); // true
console.log(endsInZeroRegex.test('01011')); // false

// Regex for "strings with even number of 1s"
const evenOnesRegex = /^(0*(10*10*)*)$/;

// Test it
console.log(evenOnesRegex.test('0110')); // true - has two 1s
console.log(evenOnesRegex.test('01011')); // false - has three 1s// Regex for "strings ending in 0"
