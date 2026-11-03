# LazyPPL Pandoc / Website Generation

To generate a mini website from Literate Haskell, use a command like this from `examples/models`:

```bash
pandoc --defaults=../../pandoc/defaults.yaml WienerDemo.lhs -o ../../website/WienerDemo.html
```

For example, to deal with all Literate Haskell files, run from the repository root:

```bash
for f in examples/models/*.lhs; do
  name="$(basename "${f%.lhs}")"
  if [ "$name" = "Index" ]; then name="index"; fi
  pandoc --defaults=pandoc/defaults.yaml "$f" -o "website/${name}.html"
done
```

## Generate the `html` files with the bash script

The script requires a playground path and source revision, checks its hashes and revision, and stops before changing `website/` if a check fails.
After those checks and earlier steps succeed, it copies the assets and playground into a temporary site, with or without arguments.
After generation and any page-list write succeed, the script attempts to replace `website/` with the completed tree.

Alternatively, run `bash generate_website.sh` from the root directory (the script also applies `lazyppltemplate.html` to the output files).

```sh
export LAZYPPL_PLAYGROUND_DIR="$PWD/web/playground/dist-playground"
export LAZYPPL_SOURCE_REVISION="$(git rev-parse HEAD)"
```

You can run the script without any arguments to convert all `.lhs` files in the `examples/models` directory:

```sh
bash generate_website.sh
```

Or you can give one or more arguments specifying the `.lhs` files you want to convert (you can include or omit the `.lhs` extension in the file names):

```sh
bash generate_website.sh myfile1 myfile2.lhs
```

In this example, the script will convert `myfile1.lhs` and `myfile2.lhs` (in the `examples/models` directory) to HTML files named `myfile1.html` and `myfile2.html` (in the `website` directory).

A successful run replaces the corresponding HTML files in `website/`, using `index.html` for `Index.lhs`.

## The publication tree


```sh
git clone https://github.com/lazyppl-team/lazyppl-team.github.io ../site-base
LAZYPPL_SITE_BASE=../site-base bash generate_website.sh
```

## Snapshots of the example pages

```sh
bash pandoc/golden.sh check
```
