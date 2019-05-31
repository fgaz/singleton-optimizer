#!/usr/bin/env bash

set -e

envfile="$(mktemp -u)"
cabal new-install --package-env="${envfile}" --lib singleton-optimizer
ghc -package-env "${envfile}" $@
rm "${envfile}"

