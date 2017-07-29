#!/urs/bin/env bash

SITE="generated/site"
DEPLOY="deploy"

alert() {
  printf "  $1"
}

update() {
  printf "$1"
}

successful() {
  printf "\033[00;32msuccessful\033[0m.\n"
}

failed() {
  printf "\033[0;31mfailed\033[0m.\n"
  echo "  Site not deployed."
  exit
}

alert "Removed old site files..."
rm -rf "$DEPLOY"/* || failed
successful

alert "Building site..."
cabal run site build > /dev/null || failed
successful

alert "Copying files for deploying..."
cp -r "$SITE"/* $DEPLOY &> /dev/null || failed
successful

MESS=$(git log -1 HEAD --pretty=format:%s)
HASH=$(git log -1 HEAD --pretty=format:%h)

cd $DEPLOY

alert "Adding files to git..."
git add --all . || failed

update "committing..."
git commit -m "$MESS, generated from $HASH" &> /dev/null || failed
successful

alert "Deploying..."
git push origin master -q
successful

echo "  Site deployed."
