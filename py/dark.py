#!/usr/bin/env python3
import os
import sys
from tkinter import *
from ttkthemes import ThemedTk

# Force dark theme (e.g., "equilux" or "arc")
root = ThemedTk(theme="equilux")
root.tk.call("source", "/usr/share/themes/Arc-Dark/gtk-3.0/gtk.css")  # Optional: Load GTK dark CSS
os.execvp(sys.argv[1], sys.argv[1:])
