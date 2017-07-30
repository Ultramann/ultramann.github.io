#!/urs/bin/env bash

REMOTE="https://github.com/Ultramann/ultramann.github.io.git"
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
if [ ! -d $DEPLOY ]; then
  update "no deploy folder. Cloning..."
  git clone "$REMOTE" deploy &> /dev/null || failed
fi
rm -rf "$DEPLOY"/*
successful

alert "Building site..."
cabal run site build > /dev/null || failed
successful

alert "Copying files for deploying..."
cp -r "$SITE"/* $DEPLOY &> /dev/null || failed
successful

MESSAGE=$(git log -1 HEAD --pretty=format:%s)
HASH=$(git log -1 HEAD --pretty=format:%h)

cd $DEPLOY

alert "Adding files to git..."
git add --all . || failed

update "committing..."
git commit -m "$MESSAGE, generated from $HASH" &> /dev/null || failed
successful

alert "Deploying..."
git push origin master -q
successful

echo "  Site deployed."
