#!/usr/bin/env bash

repo='adrianrl99/space-invaders.rkt'
tag=$1

if [[ -z "$tag" ]]; then
  echo "Tag missing"
fi

gh release create "$tag" -t "$tag" -n "$tag" -R "$repo" "./space-invaders.tar.gz"
