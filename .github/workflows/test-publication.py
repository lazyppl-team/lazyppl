# PyYAML 6.0.3 is the latest release (PyPI, read 1 Nov 2026).
# /// script
# requires-python = ">=3.12"
# dependencies = ["pyyaml==6.0.3"]
# ///
"""Guard the removed site publisher.

website.yml used to deploy to lazyppl-team/lazyppl-team.github.io. That
deploy path is gone: the live site has a publisher of its own, and one
manual run of the old deploy job would have replaced the live site with
the tree this repository generates. This script fails if the publishing
route comes back: a deploy job or dispatch input, a secret reference, a
contents: write grant, a push command, or the publish script.

Credential persistence is not checked here: zizmor's artipacked audit
flags every actions/checkout step that does not set
persist-credentials: false, and CI runs zizmor --persona pedantic on
every push.

Run with `pipx run` or `uv run`, which read the dependencies above.
"""
import re
from pathlib import Path
import yaml

ROOT = Path(__file__).resolve().parents[2]
path = ROOT / ".github/workflows/website.yml"
text = path.read_text()
workflow = yaml.safe_load(text)

trigger = workflow.get("on", workflow.get(True))
dispatch = trigger.get("workflow_dispatch") if isinstance(trigger, dict) else None
dispatch = dispatch if isinstance(dispatch, dict) else {}
assert "inputs" not in dispatch, "website.yml must not add workflow_dispatch inputs"

assert "deploy" not in workflow["jobs"], "website.yml must not bring back a deploy job"

secrets = set(re.findall(r"secrets\.([A-Za-z_][A-Za-z0-9_]*)", text))
assert secrets == set(), f"website.yml must reference no secrets: {secrets}"

# No token scope of this workflow may write contents, at any level: a job's
# permissions block overrides the workflow's, and a missing workflow-level
# block would fall back to repository defaults.
assert "permissions" in workflow, "website.yml must set workflow-level permissions"

def grants_contents_write(permissions):
    if permissions == "write-all":
        return True
    return isinstance(permissions, dict) and permissions.get("contents") == "write"

assert not grants_contents_write(workflow["permissions"]), \
    "website.yml must not grant contents: write at workflow level"
for name, job in workflow["jobs"].items():
    if "permissions" in job:
        assert not grants_contents_write(job["permissions"]), \
            f"website.yml must not grant contents: write in job {name}"

assert "git push" not in text, "website.yml must not push"

assert not (ROOT / ".github/workflows/publish-website.sh").exists(), \
    "publish-website.sh must stay removed"

print("PASS: website.yml keeps no deploy input, deploy job, secret, contents: write grant or push")
print("PASS: publish-website.sh is absent; zizmor's artipacked audit covers credential persistence")
