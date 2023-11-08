#!/bin/bash
cliphist list | wofi --dmenu --location bottom_right | cliphist decode | wl-copy
