# lazyppl Pandoc / bootstrap

To generate mini website from Literate Haskell, use a command like:

```bash
pandoc -f markdown+lhs -s WienerDemo.lhs -t html -o Wiener.html --template=lazyppltemplate.html
```

For example, to deal with all Literate Haskell files:

```bash
for f in *.lhs; do pandoc -f markdown+lhs -s "${f%.*}.lhs" -t html -o "${f%.*}.html" --template=lazyppltemplate.html ; done
```