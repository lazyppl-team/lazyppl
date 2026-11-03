#!/bin/bash
# Publish the complete checked artifact with its checked destination as the sole parent.
# Git's non-forced update rejects a destination change, even after a successful fetch.
set -euo pipefail
: "${DESTINATION_URL:?}" "${PUBLISH_BRANCH:?}" "${CHECKED_COMMIT:?}" "${ARTIFACT_DIR:?}" "${PUBLICATION_MESSAGE:?}"
[[ "$CHECKED_COMMIT" =~ ^[0-9a-f]{40}$ ]]
git check-ref-format "refs/heads/$PUBLISH_BRANCH"
artifact=$(realpath "$ARTIFACT_DIR")
[ -d "$artifact" ] && [ ! -e "$artifact/.git" ]
publication=$(mktemp -d)
trap 'rm -r -- "$publication"' EXIT
git init --bare --quiet "$publication"
export GIT_DIR="$publication" GIT_WORK_TREE="$artifact"
git config user.name 'Younesse Kaddar'
git config user.email 'younesse.kaddar@chch.ox.ac.uk'
git fetch --no-tags "$DESTINATION_URL" "$CHECKED_COMMIT"
# ls-files without exclusion flags includes dotfiles and ignored files in the artifact.
# A new, empty index means files absent from the artifact cannot survive publication.
git ls-files --others -z | git update-index --add -z --stdin
tree=$(git write-tree)
commit=$(printf '%s\n' "$PUBLICATION_MESSAGE" | git commit-tree "$tree" -p "$CHECKED_COMMIT")
git push "$DESTINATION_URL" "$commit:refs/heads/$PUBLISH_BRANCH"
