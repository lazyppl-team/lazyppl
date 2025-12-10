#!/bin/bash
set -euo pipefail

if [ ! -d "website" ]; then
    mkdir -p website
fi

# Ensure website has a copy of static images used by demos (including Physics).
if [ -d "images" ]; then
    mkdir -p website/images
    cp -u images/* website/images/ 2>/dev/null || true
fi

cd src

convert_file() {
    # remove .lhs extension if it exists
    filename=${1%.lhs}
    pandoc -f markdown+lhs+markdown_in_html_blocks \
           -s "$filename.lhs" \
           -t html \
           -o "../website/${filename}.html" \
           --template=../pandoc/lazyppltemplate.html \
           --mathjax \
           --syntax-highlighting=default

    # make sure index.html is in lowercase:
    if [ "$filename" == "Index" ]; then
        mv "../website/Index.html" "../website/index.html"
    fi
}

# if arguments are given, convert each file
if [ "$#" -gt 0 ]; then
    for file in "$@"; do
        convert_file "$file"
    done
else
    # else, convert all lhs files in this repo
    for f in *.lhs; do
        convert_file "$f"
    done

    # Build Physics page from the external lazyppl_physics repo if available.
    # Path is ../../lazyppl_physics because we're in src/ subdirectory
    if [ -f "../../lazyppl_physics/src/Physics.lhs" ]; then
        pandoc -f markdown+lhs+markdown_in_html_blocks \
               -s "../../lazyppl_physics/src/Physics.lhs" \
               -t html \
               -o "../website/Physics.html" \
               --template=../pandoc/lazyppltemplate.html \
               --mathjax \
               --syntax-highlighting=default
    fi
fi
