# LazyPPL Pandoc / bootstrap

To generate mini website from Literate Haskell, use a command like:

```bash
pandoc -f markdown+lhs -s WienerDemo.lhs -t html -o Wiener.html --template=lazyppltemplate.html
```

For example, to deal with all Literate Haskell files:

```bash
for f in *.lhs; do pandoc -f markdown+lhs -s "${f%.*}.lhs" -t html -o "website/${f%.*}.html" --template=lazyppltemplate.html ; done
```


## Generate the `html` files with the bash script

Alternatively, run `bash generate_website.sh` from the root directory (the script also applies `lazyppltemplate.html` to the output files).

You can run the script without any arguments to convert all `.lhs` files in the `src` directory:

```sh
bash generate_website.sh
```

Or you can give one or more arguments specifying the `.lhs` files you want to convert (you can include or omit the `.lhs` extension in the file names):

```sh
bash generate_website.sh myfile1 myfile2.lhs
```

In this example, the script will convert `myfile1.lhs` and `myfile2.lhs` (in the `src` directory) to HTML files named `myfile1.html` and `myfile2.html` (in the `website` directory).

The script will overwrite any existing HTML files in the website directory that have the same names as the `.lhs` files being converted.