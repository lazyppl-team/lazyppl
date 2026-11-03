# Fonts and icons served with the example pages

The generated pages load no stylesheet, font or icon from another site. `generate_website.sh` copies this folder to `website/assets/`, and `pandoc/lazyppltemplate.html` links the stylesheets below.

Why the files live here: a stylesheet on a CDN can change without notice, the font files that a stylesheet loads cannot carry an integrity hash, and Google Fonts answers each browser with a different stylesheet. Served from the site, the files are fixed by the commit, and the pages also render without a network.

Every file below is a byte-identical copy of a file in the npm package (the font licences are renamed to `LICENSE-<family>.txt`), except `fonts/fonts.css`, which is assembled from the packages' stylesheets as described in its row.

| Folder | Package (npm) | Files kept | Licence |
| --- | --- | --- | --- |
| `fontawesome/` | `@fortawesome/fontawesome-free` 7.3.1 | `css/fontawesome.min.css`, `css/solid.min.css`, `css/brands.min.css`, `webfonts/fa-solid-900.woff2`, `webfonts/fa-brands-400.woff2`, `LICENSE.txt`. The regular and v4-compatibility styles are left out: the pages use solid and brands icons only. | Icons CC BY 4.0, fonts SIL OFL 1.1, code MIT (`LICENSE.txt`) |
| `academicons/` | `academicons` 1.9.5 | `css/academicons.min.css`, `fonts/academicons.ttf`, `README.md`. The stylesheet also lists `.eot`, `.woff` and `.svg` files. A browser takes the first format in that list that it supports, which is the `.ttf`, so the other three are not shipped. | Fonts SIL OFL 1.1, CSS MIT (`README.md`) |
| `fonts/` | `@fontsource-variable/lora` 5.3.0, `@fontsource-variable/source-serif-4` 5.3.1, `@fontsource-variable/jetbrains-mono` 5.3.0 | The `latin` and `latin-ext` variable `.woff2` files and the licence of each family. `fonts.css` holds the `latin` and `latin-ext` `@font-face` blocks of each package's `wght.css` (`opsz.css` for Source Serif 4). It differs from the packages in two places: the family names are `Lora`, `Source Serif 4` and `JetBrains Mono`, which are the names `lazypplstyles.css` uses, and the `url()` paths are relative to `fonts.css`. | SIL OFL 1.1 (`LICENSE-*.txt`) |

Versions are the newest stable releases on 31 October 2026. Package integrity strings, from the npm registry:

- `@fortawesome/fontawesome-free@7.3.1`: `sha512-wmglKKPDIkgV3aWlZzWECCPoGIkYCulzBwxG9+w7rc5BGapZ6cPMpoPOT8k36J0Ni7PPX6c/rsoMWfS4d1MUMg==`
- `academicons@1.9.5`: `sha512-ZXyNuotcqMNLmfc48W3BnRP/RzSs9cxvLrSk9fLgmgxAFDC+ecB9viNn1hzs2eJARre7N6tCfDiVQTia+uegEQ==`
- `@fontsource-variable/lora@5.3.0`: `sha512-2ph8KiZ8Bn141hWuC3ErtpJ+Ui8p2MJQn8TJNRSRmLmnCIOCtHfiBM9R7WLRPeUG/q6hXyHOxWQ8DvRSX+ixvQ==`
- `@fontsource-variable/source-serif-4@5.3.1`: `sha512-4Rtay22SC4lMvw11r/+9kxANB6P3M0X8s1NudBC2v0J1KU2y3DcMoNcT3lBoZ/CqLirzf+StlO6UlJilyrvaUg==`
- `@fontsource-variable/jetbrains-mono@5.3.0`: `sha512-F32xpS2NsGYoQi2ADSkKTgpJj7ozajsGgDJ8woTnqjmIB+dxDIqImjl4pXZVEExu8UFZ2ndhmX18EBS/hdz3Lw==`

To update a package, run `npm pack <package>@<version>`, check the tarball against the integrity string that `npm view <package>@<version> dist.integrity` prints, and copy the files named above.

Two limits to keep in mind:

- The pages use characters from the Latin and Latin Extended ranges. A page that sets text in another script shows those characters in the browser's fallback font until the matching subset is added to `fonts/`.
- The pages use these icon classes: `fa-github` (brands); `fa-book-open`, `fa-layer-group`, `fa-wave-square`, `fa-project-diagram`, `fa-chart-line`, `fa-play-circle`, `fa-cube` (solid); `ai-acm`, `ai-arxiv`. Check that a new icon exists in the style whose stylesheet is linked.
