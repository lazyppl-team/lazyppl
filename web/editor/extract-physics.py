#!/usr/bin/env python3
"""Extract the Haskell code blocks from the original physics page."""

from html.parser import HTMLParser
import json
import sys


class CodeBlocks(HTMLParser):
    def __init__(self):
        super().__init__()
        self.in_pre = False
        self.in_code = False
        self.blocks = []

    def handle_starttag(self, tag, attrs):
        if tag == "pre":
            self.in_pre = True
        elif tag == "code" and self.in_pre:
            self.in_code = True
            self.blocks.append("")

    def handle_endtag(self, tag):
        if tag == "code" and self.in_code:
            self.in_code = False
        elif tag == "pre":
            self.in_pre = False

    def handle_data(self, data):
        if self.in_code:
            self.blocks[-1] += data


parser = CodeBlocks()
parser.feed(sys.stdin.read())
if not parser.blocks:
    raise SystemExit("Physics page has no code listing")
json.dump([block.rstrip() for block in parser.blocks], sys.stdout, ensure_ascii=False, indent=2)
sys.stdout.write("\n")
