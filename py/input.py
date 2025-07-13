#!/bin/python
from pynput import mouse, keyboard

# Create a listener for mouse events
class ScrollBlocker:
    def __init__(self):
        self.alt_pressed = False

    def on_click(self, x, y, button, pressed):
        # Check if Alt is pressed
        if button == mouse.Button.alt:
            self.alt_pressed = pressed

    def on_scroll(self, x, y, dx, dy):
        if self.alt_pressed:
            # Block Alt + Scroll
            return False  # Block the scroll event

    def on_press(self, key):
        try:
            if key == keyboard.Key.alt:
                self.alt_pressed = True
            elif key == keyboard.Key.down:
                # Simulate Ctrl + Down
                keyboard.Controller().press(keyboard.Key.ctrl)
                keyboard.Controller().press(keyboard.Key.down)
                keyboard.Controller().release(keyboard.Key.down)
                keyboard.Controller().release(keyboard.Key.ctrl)
            elif key == keyboard.Key.up:
                # Simulate Ctrl + Up
                keyboard.Controller().press(keyboard.Key.ctrl)
                keyboard.Controller().press(keyboard.Key.up)
                keyboard.Controller().release(keyboard.Key.up)
                keyboard.Controller().release(keyboard.Key.ctrl)
        except AttributeError:
            pass

    def on_release(self, key):
        if key == keyboard.Key.alt:
            self.alt_pressed = False
        if key == keyboard.Key.esc:
            # Stop listener
            return False

# Set up the listener
scroll_blocker = ScrollBlocker()
mouse_listener = mouse.Listener(on_click=scroll_blocker.on_click, on_scroll=scroll_blocker.on_scroll)
keyboard_listener = keyboard.Listener(on_press=scroll_blocker.on_press, on_release=scroll_blocker.on_release)

mouse_listener.start()
keyboard_listener.start()

mouse_listener.join()
keyboard_listener.join()
