"""Verify that site generation consumes the exact verified playground directory."""
import hashlib
import json
import os
import shutil
import subprocess
import tempfile
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from threading import Thread
from urllib.error import HTTPError
from urllib.parse import quote
from urllib.request import urlopen

ROOT = Path(__file__).resolve().parents[2]
with tempfile.TemporaryDirectory(prefix="lazyppl-artifact-") as tmp:
    fixture = Path(tmp)
    repo, artifact, base, physics = (fixture / p for p in ["repo", "playground", "site", "physics"])
    repo.mkdir()
    shutil.copy(ROOT / "generate_website.sh", repo)
    for name in ["images", "pandoc", "examples/models"]:
        shutil.copytree(ROOT / name, repo / name)
    tested = Path(os.environ["LAZYPPL_PLAYGROUND_DIR"]).resolve()
    revision = json.loads((tested / "build.json").read_text())["source_revision"]
    stage = ROOT / "web/playground/test/stage-playground.sh"
    subprocess.run(["bash", str(stage), str(tested), str(tested / "wasi"),
                    str(tested / "editor-examples"), str(tested / "native-plan.json"),
                    str(tested / "wasm-plan.json"), str(tested / "toolchain.json"),
                    str(artifact)], check=True)
    comparison = subprocess.run(["diff", "-r", str(tested), str(artifact)], capture_output=True, text=True)
    assert comparison.returncode == 0, f"Publication fixture differs from the staged playground:\n{comparison.stdout}{comparison.stderr}"
    (base / "playground").mkdir(parents=True)
    (base / "playground/retired.js").write_text("Retired asset\n")
    (base / "other.html").write_text("Existing site page\n")
    (physics / "src").mkdir(parents=True)
    (physics / "src/Physics.lhs").write_text("Physics fixture\n===============\n\n> x = 1\n")
    env = dict(os.environ, LAZYPPL_SITE_BASE=str(base), LAZYPPL_PHYSICS_DIR=str(physics), LAZYPPL_PLAYGROUND_DIR=str(artifact), LAZYPPL_SOURCE_REVISION=revision)
    def generate(check=True):
        return subprocess.run(["bash", "generate_website.sh"], cwd=repo, env=env, capture_output=True, text=True, check=check)
    # An invalid base or a missing input must not destroy a previously built site.
    (repo / "website").mkdir()
    sentinel = repo / "website/existing.txt"
    sentinel.write_text("Previous site\n")
    (repo / "website/nested").mkdir()
    (repo / "website/nested/page.html").write_text("Existing page\n")
    (repo / "website/existing.html").write_text("Existing page before traversal\n")
    (repo / "examples/website").mkdir()
    (repo / "examples/website/existing.lhs").write_text("Traversal\n=========\n\n> x = 1\n")
    def site_contents():
        root = repo / "website"
        return {str(p.relative_to(root)): p.read_bytes() for p in root.rglob("*") if p.is_file()}
    original_site = site_contents()
    result = subprocess.run(["bash", "generate_website.sh", "../website/existing.lhs", "Missing.lhs"], cwd=repo, env=env, capture_output=True, text=True)
    assert result.returncode != 0, "A traversal followed by a missing page succeeded"
    after_traversal = site_contents()
    changed = sorted(name for name in original_site.keys() | after_traversal.keys() if original_site.get(name) != after_traversal.get(name))
    assert after_traversal == original_site, f"A traversal changed the existing site: {changed}"
    (repo / "examples/models/nested").mkdir()
    (repo / "examples/models/nested/Valid.lhs").write_text("Nested\n======\n\n> x = 1\n")
    (repo / "examples/models/Bad..Name.lhs").write_text("Dots\n====\n\n> x = 1\n")
    invalid_pages = ["../website/existing.lhs", "../website/existing", "nested/Valid.lhs", "nested/Valid", "Bad..Name.lhs", "Bad..Name", "Missing.lhs", "Missing"]
    for page in invalid_pages:
        result = subprocess.run(["bash", "generate_website.sh", page], cwd=repo, env=env, capture_output=True, text=True)
        assert result.returncode != 0 and page in result.stderr and ".lhs" in result.stderr, f"Invalid page argument was not explained: {page}"
        assert site_contents() == original_site, f"Invalid page changed the existing site: {page}"
    env["LAZYPPL_SITE_BASE"] = str(fixture / "missing-base")
    result = generate(check=False)
    assert result.returncode != 0 and site_contents() == original_site, "An invalid site base changed the existing site"
    env["LAZYPPL_SITE_BASE"] = str(base)
    logo = repo / "images/logo.png"
    logo.rename(repo / "images/logo-away.png")
    result = generate(check=False)
    logo = (repo / "images/logo-away.png").rename(logo)
    assert result.returncode != 0 and site_contents() == original_site, "A missing static asset changed the existing site"
    env["LAZYPPL_PHYSICS_DIR"] = str(fixture / "missing-physics")
    result = generate(check=False)
    assert result.returncode != 0 and site_contents() == original_site, "A failed site build changed the existing site"
    env["LAZYPPL_PHYSICS_DIR"] = str(physics)
    result = subprocess.run(["bash", "generate_website.sh", "Missing.lhs"], cwd=repo, env=env, capture_output=True, text=True)
    assert result.returncode != 0 and site_contents() == original_site, "A failed partial build changed the existing site"
    env["LAZYPPL_PAGE_LIST"] = str(fixture / "missing-list-directory/pages")
    result = generate(check=False)
    assert result.returncode != 0 and site_contents() == original_site, "A failed page-list write changed the existing site"
    env["LAZYPPL_PAGE_LIST"] = str(repo / "website/pages.txt")
    generate()
    pages = (repo / "website/pages.txt").read_text().splitlines()
    assert pages and all(Path(p).is_file() and Path(p).parent == repo / "website" for p in pages), "Page list must name the published pages"
    del env["LAZYPPL_PAGE_LIST"]
    published = repo / "website/playground"
    actual_paths = sorted(str(p.relative_to(published)) for p in published.rglob("*") if p.is_file())
    expected_paths = sorted(str(p.relative_to(artifact)) for p in artifact.rglob("*") if p.is_file())
    print(f"Included playground files: {actual_paths}", flush=True)
    assert actual_paths == expected_paths, "Publication did not include exactly the tested playground directory"
    for path in expected_paths:
        assert (published / path).read_bytes() == (artifact / path).read_bytes(), path
    assert (repo / "website/other.html").read_text() == "Existing site page\n"
    print("PASS: executable, JS, validators, manifest and existing site bytes retained exactly", flush=True)
    result = subprocess.run(["bash", "generate_website.sh", "GraphDemo.lhs"], cwd=repo, env=env, capture_output=True, text=True)
    assert result.returncode == 0 and (repo / "website/GraphDemo.html").is_file(), f"A valid partial page failed: {result.stderr}"
    (repo / "website/GraphDemo.html").write_text("Previous GraphDemo page\n")
    before_extensionless = site_contents()
    result = subprocess.run(["bash", "generate_website.sh", "GraphDemo"], cwd=repo, env=env, capture_output=True, text=True)
    assert result.returncode == 0, f"A valid extensionless page failed: {result.stderr}"
    after_extensionless = site_contents()
    assert after_extensionless["GraphDemo.html"] != before_extensionless["GraphDemo.html"], "The extensionless page was not generated"
    assert {k: v for k, v in after_extensionless.items() if k != "GraphDemo.html"} == {k: v for k, v in before_extensionless.items() if k != "GraphDemo.html"}, "An extensionless page changed another site file"
    env["LAZYPPL_SOURCE_REVISION"] = "0" * 40 if revision != "0" * 40 else "1" * 40
    result = generate(check=False)
    assert result.returncode != 0, "Publication accepted an artifact from a different source revision"
    env["LAZYPPL_SOURCE_REVISION"] = revision
    with (artifact / "validators.js").open("a") as out:
        out.write("\n// Tampered after testing\n")
    result = generate(check=False)
    print(f"Tampered artifact exit={result.returncode}: {result.stdout}{result.stderr}", flush=True)
    assert result.returncode != 0, "Publication accepted a playground that changed after testing"
    env.pop("LAZYPPL_PLAYGROUND_DIR")
    result = generate(check=False)
    assert result.returncode != 0, "Publication accepted an absent tested playground"
    print("PASS: wrong-source, missing and modified tested artifacts stop generation", flush=True)

    env["LAZYPPL_PLAYGROUND_DIR"] = str(tested)
    env["LAZYPPL_SOURCE_REVISION"] = os.environ["LAZYPPL_SOURCE_REVISION"]
    expected_wasi = {p.relative_to(tested / "wasi").as_posix(): p.read_bytes() for p in (tested / "wasi").rglob("*") if p.is_file()}
    assert expected_wasi, "The tested playground has no WASI files"
    browser_check = r"""
import assert from 'node:assert/strict';
import { chromium } from '@playwright/test';
const origin = process.argv[1];
const browser = await chromium.launch({ headless: true });
try {
  const page = await browser.newPage();
  const failures = [];
  page.on('response', response => {
    if (new URL(response.url()).pathname.startsWith('/wasi/') && !response.ok()) {
      failures.push(`${response.status()} ${response.url()}`);
    }
  });
  page.on('requestfailed', request => {
    if (new URL(request.url()).pathname.startsWith('/wasi/')) failures.push(`failed ${request.url()}`);
  });
  await page.goto(`${origin}/playground/index.html`);
  await page.waitForFunction(() => ['done', 'error'].includes(document.querySelector('#poisson-result').dataset.state), null, { timeout: 30000 });
  const output = page.locator('#poisson-result');
  assert.equal(await output.getAttribute('data-state'), 'done', `Poisson failed: ${await output.textContent()}; WASI requests: ${failures.join(', ')}`);
  const rows = JSON.parse(await page.locator('#poisson-data').textContent());
  assert.ok(Array.isArray(rows) && rows.length > 0, 'Poisson produced no samples');
  assert.deepEqual(failures, [], 'A WASI module request failed');
  console.log(`PASS: real Poisson model at ${origin}/playground/`);
} finally {
  await browser.close();
}
"""
    site_errors = []
    for scenario in ("clean", "stale"):
        if scenario == "stale":
            (base / "wasi").mkdir()
            (base / "wasi/index.js").write_text("stale WASI implementation\n")
            (base / "wasi/stale.js").write_text("stale extra file\n")
        generate()
        published_wasi = repo / "website/wasi"
        actual_wasi = {p.relative_to(published_wasi).as_posix(): p.read_bytes() for p in published_wasi.rglob("*") if p.is_file()}
        if actual_wasi != expected_wasi:
            site_errors.append(f"{scenario}: published root WASI file set or bytes differ from the tested bundle")
        handler = partial(SimpleHTTPRequestHandler, directory=str(repo / "website"))
        server = ThreadingHTTPServer(("127.0.0.1", 0), handler)
        thread = Thread(target=server.serve_forever, daemon=True)
        thread.start()
        origin = f"http://127.0.0.1:{server.server_port}"
        try:
            browser = subprocess.run(["node", "--input-type=module", "-e", browser_check, origin], cwd=ROOT / "web/playground/test", capture_output=True, text=True, timeout=60)
            if browser.returncode != 0:
                site_errors.append(f"{scenario}: browser model failed: {browser.stderr.strip()}")
            else:
                print(f"{scenario}: {browser.stdout.strip()}", flush=True)
            for name, expected in expected_wasi.items():
                try:
                    with urlopen(f"{origin}/wasi/{quote(name, safe='/')}", timeout=5) as response:
                        if hashlib.sha256(response.read()).digest() != hashlib.sha256(expected).digest():
                            site_errors.append(f"{scenario}: served /wasi/{name} hash differs from the tested bundle")
                except HTTPError as error:
                    site_errors.append(f"{scenario}: /wasi/{name} returned HTTP {error.code}")
        finally:
            server.shutdown()
            server.server_close()
            thread.join()
    assert not site_errors, "\n".join(site_errors)
    print("PASS: clean and stale sites serve the tested WASI files and run the real model", flush=True)
