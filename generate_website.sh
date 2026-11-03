#!/bin/bash
set -euo pipefail

for page in "$@"; do
    name="${page%.lhs}"
    if [[ -z "$name" || "$name" == */* || "$name" == *..* || ! -f "examples/models/$name.lhs" ]]; then
        printf 'Error: invalid page argument %q; expected an existing plain page name in examples/models, with optional .lhs.\n' "$page" >&2
        exit 1
    fi
done

# With LAZYPPL_SITE_BASE set to a checkout of the published site (lazyppl-team/lazyppl-team.github.io), website/ becomes
# the complete publication tree: the published site, with what this script generates replacing the same paths.
# This script owns the generated pages, assets/, playground/ (the tested browser artifact), root wasi/ and the files of images/
# (logo.png, favicon.ico and graph-sphere.html). Everything else in the published site stays as it is. See "The publication tree" in pandoc/README.md.
# LAZYPPL_PAGE_LIST names a file that receives the path of every page this run writes, one per line, so that a link
# check can read the pages this repository owns and leave the older pages of the site to their owner.
: "${LAZYPPL_PLAYGROUND_DIR:?Supply the tested playground artifact}" "${LAZYPPL_SOURCE_REVISION:?Supply its source revision}"
playground=$(realpath "$LAZYPPL_PLAYGROUND_DIR")
(cd "$playground" && sha256sum -c SHA256SUMS)
jq -e --arg revision "$LAZYPPL_SOURCE_REVISION" '.source_revision == $revision' "$playground/build.json" >/dev/null
page_list="${LAZYPPL_PAGE_LIST:-}"
if [ ! -d "images" ] || [ ! -f "images/logo.png" ] || [ ! -f "images/favicon.ico" ]; then
    echo "Error: required static assets images/logo.png and images/favicon.ico are missing." >&2
    exit 1
fi
if [ "$#" -eq 0 ] && [ -n "${LAZYPPL_SITE_BASE:-}" ] && [ ! -d "$LAZYPPL_SITE_BASE" ]; then
    echo "Error: LAZYPPL_SITE_BASE=${LAZYPPL_SITE_BASE} is not a directory." >&2
    exit 1
fi

# Build alongside the destination so an input or conversion failure leaves the
# previous site untouched. A partial build starts from the existing site.
site_root="$PWD/website"
site_dir=$(mktemp -d "$PWD/.website-build.XXXXXXXX")
list_tmp=""
trap 'rm -rf -- "$site_dir"; if [ -n "$list_tmp" ]; then rm -f -- "$list_tmp"; fi' EXIT
if [ "$#" -eq 0 ] && [ -n "${LAZYPPL_SITE_BASE:-}" ]; then
    cp -a "$LAZYPPL_SITE_BASE"/. "$site_dir"/
    rm -rf "$site_dir/.git"
elif [ "$#" -gt 0 ] && [ -d "$site_root" ]; then
    cp -a "$site_root"/. "$site_dir"/
fi
if [ -n "$page_list" ]; then
    case "$page_list" in /*) ;; *) page_list="$PWD/$page_list" ;; esac
    list_tmp=$(mktemp "$PWD/.website-pages.XXXXXXXX")
    if [ "$#" -gt 0 ] && [ -f "$page_list" ]; then cp "$page_list" "$list_tmp"; fi
fi
mkdir -p "$site_dir/images"
cp -r images/. "$site_dir/images/"
rm -rf "$site_dir/assets"
mkdir -p "$site_dir/assets"
cp -r pandoc/assets/. "$site_dir/assets/"

# Replace the old browser directory with the exact tested artifact, including its provenance files.
rm -rf "$site_dir/playground"
cp -a "$playground" "$site_dir/playground"
rm -rf "$site_dir/wasi"
cp -a "$playground/wasi" "$site_dir/wasi"
diff -r -- "$playground/wasi" "$site_dir/wasi" >/dev/null

cd examples/models

convert_lhs() {
    local src_lhs="$1"
    local out_html="$2"
    pandoc --defaults=../../pandoc/defaults.yaml "$src_lhs" -o "$out_html"
    if [ -n "$list_tmp" ]; then
        echo "${out_html/#$site_dir/$site_root}" >> "$list_tmp"
    fi
}

convert_file() {
    local filename="${1%.lhs}"
    local target_name="${filename}.html"
    if [ "$filename" = "Index" ]; then
        target_name="index.html"
    fi
    convert_lhs "${filename}.lhs" "${site_dir}/${target_name}"
}

if [ "$#" -gt 0 ]; then
    for file in "$@"; do
        convert_file "$file"
    done
else
    for f in *.lhs; do
        convert_file "$f"
    done

    physics_root="${LAZYPPL_PHYSICS_DIR:-../../../lazyppl_physics}"
    physics_lhs="${physics_root}/src/Physics.lhs"
    if [ ! -f "$physics_lhs" ]; then
        echo "Error: required Physics source not found at ${physics_lhs}" >&2
        exit 1
    fi
    convert_lhs "$physics_lhs" "${site_dir}/Physics.html"
fi

# Write the page list before the site swap, so a failure leaves the old site.
if [ -n "$list_tmp" ]; then
    case "$page_list" in
        "$site_root"/*) mv "$list_tmp" "$site_dir/${page_list#"$site_root"/}" ;;
        *) mv "$list_tmp" "$page_list" ;;
    esac
    list_tmp=""
fi

# Rename the completed tree into place, restoring the old one if that fails.
backup=""
if [ -e "$site_root" ] || [ -L "$site_root" ]; then
    backup=$(mktemp -d "${site_root}.previous.XXXXXXXX")
    rmdir "$backup"
    mv "$site_root" "$backup"
fi
if ! mv "$site_dir" "$site_root"; then
    if [ -n "$backup" ]; then mv "$backup" "$site_root"; fi
    exit 1
fi
if [ -n "$backup" ]; then rm -rf -- "$backup"; fi
