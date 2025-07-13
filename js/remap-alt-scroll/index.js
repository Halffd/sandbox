const ioHook = require('iohook');
const robot = require('robotjs');
// Function to simulate key press
function simulateKeyPress(key) {
robot.keyTap(key);
}

// Listen for mouse wheel events
ioHook.on('mousewheel', event => {
// Check if Alt key is pressed
if (event.altKey) {
if (event.rotation === 1) {
// Scroll up event
console.log('Alt + Scroll Up');
simulateKeyPress('pageup'); // Simulate Page Up key press
} else if (event.rotation === -1) {
// Scroll down event
console.log('Alt + Scroll Down');
simulateKeyPress('pagedown'); // Simulate Page Down key press
}
}
});

// Start iohook to listen for events
ioHook.start();
