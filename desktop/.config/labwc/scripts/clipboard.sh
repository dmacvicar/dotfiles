#!/bin/bash
cliphist list | wofi --dmenu --location top_right | cliphist decode | wl-copy
