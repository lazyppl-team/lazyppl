# PyYAML 6.0.3 is the latest release (PyPI, read 1 Nov 2026).
# /// script
# requires-python = ">=3.12"
# dependencies = ["pyyaml==6.0.3"]
# ///
"""Exercise the workflow publisher against local bare repositories only.

The publisher is invoked exactly as website.yml invokes it. Run with
`pipx run` or `uv run`, which read the dependencies above.
"""
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
import yaml

ROOT = Path(__file__).resolve().parents[2]
workflow = yaml.safe_load((ROOT / ".github/workflows/website.yml").read_text())
publish = next(s for s in workflow["jobs"]["deploy"]["steps"] if s["name"] == "Publish website")
secrets = set(re.findall(r"secrets\.([A-Za-z_][A-Za-z0-9_]*)", (ROOT / ".github/workflows/website.yml").read_text()))
assert secrets == {"SITE_DEPLOY_KEY"}, f"Unexpected website workflow secrets: {secrets}"
assert publish["env"]["DESTINATION_URL"] == "git@github.com:lazyppl-team/lazyppl-team.github.io.git", "Publication destination must use SSH"
assert "GH_TOKEN" not in publish["env"], "Publication must not use a personal token"
site_checkout = next(s for s in workflow["jobs"]["deploy"]["steps"] if s.get("with", {}).get("repository") == "lazyppl-team/lazyppl-team.github.io")
assert site_checkout["with"]["ssh-key"] == "${{ secrets.SITE_DEPLOY_KEY }}", "Site checkout must use its deploy key"
assert site_checkout["with"]["path"] == "site" and site_checkout["with"]["persist-credentials"] is True
assert "token" not in site_checkout["with"], "Site checkout must use SSH instead of a token"

def run(*args, cwd=None, check=True, env=None):
    return subprocess.run(args, cwd=cwd, env=env, text=True, capture_output=True, check=check)

def git(root, *args):
    return run("git", "-C", str(root), *args).stdout.strip()

for concurrent in [False, True]:
    with tempfile.TemporaryDirectory(prefix="lazyppl-publish-") as tmp:
        root = Path(tmp)
        dest, seed, artifact = (root / p for p in ["destination.git", "seed", "artifact"])
        git(root, "init", "--bare", "-q", str(dest))
        git(root, "init", "-q", "--initial-branch=master", str(seed))
        git(seed, "config", "user.name", "Publication fixture")
        git(seed, "config", "user.email", "fixture@example.invalid")
        (seed / "other.html").write_text("S: checked contents\n")
        git(seed, "add", "other.html")
        git(seed, "commit", "-qm", "S")
        git(seed, "push", str(dest), "HEAD:refs/heads/master")
        checked = git(seed, "rev-parse", "HEAD")
        artifact.mkdir()
        (artifact / "other.html").write_text("S: checked contents\n")
        (artifact / "new.html").write_text("Verified artifact\n")
        (artifact / ".gitignore").write_text("*.bin\n")
        (artifact / "included.bin").write_bytes(b"\x00\x01\xff")
        # The preflight observes S; B arrives strictly after that observation.
        assert git(root, "ls-remote", str(dest), "refs/heads/master").split()[0] == checked
        if concurrent:
            (seed / "other.html").write_text("B: newer site edit\n")
            git(seed, "add", "other.html")
            git(seed, "commit", "-qm", "B")
            git(seed, "push", str(dest), "HEAD:refs/heads/master")
        before = git(seed, "rev-parse", "HEAD")
        (root / "lazyppl/.github/workflows").mkdir(parents=True)
        shutil.copy(ROOT / ".github/workflows/publish-website.sh", root / "lazyppl/.github/workflows/publish-website.sh")
        site = root / "site"
        git(root, "init", "-q", str(site))
        env = dict(os.environ, DESTINATION_URL=str(dest), PUBLISH_BRANCH="master", CHECKED_COMMIT=checked,
                   ARTIFACT_DIR=str(artifact), PUBLICATION_MESSAGE="Publish verified artifact")
        missing_key = run("bash", "-e", "-c", publish["run"], cwd=root, env=env, check=False)
        assert missing_key.returncode != 0, "Publisher must refuse without checkout SSH configuration"
        assert git(root, "ls-remote", str(dest), "refs/heads/master").split()[0] == before
        git(site, "config", "core.sshCommand", "ssh")
        result = run("bash", "-e", "-c", publish["run"], cwd=root, env=env, check=False)
        after = git(root, "ls-remote", str(dest), "refs/heads/master").split()[0]
        contents = git(dest, "show", "master:other.html")
        print(f"concurrent={concurrent}; publish exit={result.returncode}; other.html={contents}", flush=True)
        if concurrent:
            print(result.stderr, flush=True)
            assert after == before and result.returncode != 0, "Publisher overwrote commit B after preflight"
        else:
            assert result.returncode == 0, result.stdout + result.stderr
            paths = git(dest, "ls-tree", "-r", "--name-only", "master").splitlines()
            assert paths == sorted(p.name for p in artifact.iterdir()), paths
            for name in paths:
                actual = subprocess.check_output(["git", "--git-dir", str(dest), "show", f"master:{name}"])
                assert actual == (artifact / name).read_bytes(), name
            print("PASS: published tree equals the complete checked artifact, including ignored and hidden files", flush=True)
print("PASS: concurrent destination commit survives and publication stops")
