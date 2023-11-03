#!/bin/bash

if [ ! -d "website" ]; then
    mkdir website
fi

cd src

convert_file() {
    # remove .lhs extension if it exists
    filename=${1%.lhs}
    pandoc -f markdown+lhs -s "$filename.lhs" -t html -o "../website/${filename}.html" --template=../pandoc/lazyppltemplate.html --mathjax --highlight-style tango 
    
    # make sure index.html is in lowercase:
    if [ "$filename" == "Index" ]; then
        mv "../website/Index.html" "../website/index.html"
    fi
}

# if arguments are given, convert each file
if [ "$#" -gt 0 ]; then
    for file in "$@"
    do
        convert_file "$file"
    done
else
    # else, convert all lhs files
    for f in *.lhs
    do 
        convert_file "$f"
    done
fi
