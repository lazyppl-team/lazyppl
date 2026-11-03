"""Run the actual snapshot gate in an isolated Git repository with real Pandoc."""
import shutil
import subprocess
import tempfile
from pathlib import Path

SOURCE = Path(__file__).resolve().parent

def run(*args, cwd, check=True):
    return subprocess.run(args, cwd=cwd, text=True, capture_output=True, check=check)

with tempfile.TemporaryDirectory(prefix="lazyppl-golden-") as tmp:
    root = Path(tmp)
    shutil.copytree(SOURCE, root / "pandoc")
    (root / "examples/models").mkdir(parents=True)
    (root / "test/golden").mkdir(parents=True)
    (root / "examples/models/One.lhs").write_text("One\n===\n\n> x = 1\n")
    run("git", "init", "-q", cwd=root)
    run("git", "config", "user.name", "Snapshot fixture", cwd=root)
    run("git", "config", "user.email", "fixture@example.invalid", cwd=root)
    run("bash", "pandoc/golden.sh", "update", cwd=root)
    run("git", "add", "pandoc", "examples", "test", cwd=root)
    run("git", "commit", "-qm", "Initial snapshot", cwd=root)
    run("bash", "pandoc/golden.sh", "check", cwd=root)
    print("PASS: clean committed baseline")
    (root / "examples/models/New.lhs").write_text("New\n===\n\n> y = 2\n")
    run("git", "add", "examples/models/New.lhs", cwd=root)
    run("git", "commit", "-qm", "Add page", cwd=root)

    def rejected(label):
        result = run("bash", "pandoc/golden.sh", "check", cwd=root, check=False)
        print(f"{label}: exit={result.returncode}; {result.stdout}{result.stderr}")
        assert result.returncode != 0, f"{label}: gate accepted an uncommitted snapshot tree"

    rejected("Missing snapshot")
    run("bash", "pandoc/golden.sh", "update", cwd=root)
    print(run("git", "status", "--short", cwd=root).stdout)
    rejected("Matching untracked snapshot")
    run("git", "add", "test/golden/New.txt", cwd=root)
    rejected("Matching staged but uncommitted snapshot")
    run("git", "commit", "-qm", "Add snapshot", cwd=root)
    run("bash", "pandoc/golden.sh", "check", cwd=root)
    (root / "test/golden/One.txt").write_text("changed\n")
    rejected("Modified committed snapshot")
    (root / "test/golden/One.txt").write_bytes(run("git", "show", "HEAD:test/golden/One.txt", cwd=root).stdout.encode())
    (root / "test/golden/Orphan.txt").write_text("orphan\n")
    rejected("Extra snapshot")
    print("PASS: snapshot gate rejects every uncommitted baseline")
