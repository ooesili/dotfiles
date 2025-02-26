#!/usr/bin/env python
import colorsys
import sys
import re

def main():
    args = sys.argv[1:]
    if len(args) == 0:
        raise Exception('no arguments given')

    for color in args:
        convert_color(color)

def convert_color(color):
    if color.startswith('#'):
        color = color[1:]
    if len(color) != 6:
        raise Exception(f'invalid hex color: {color}')
    (r, g, b) = (int(c, base=16) for c in re.findall('..', color))
    print(r, g, b)

if __name__ == "__main__":
    main()
