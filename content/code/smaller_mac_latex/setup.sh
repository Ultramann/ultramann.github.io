#!/bin/bash

install_package () {
  sudo tlmgr install $1
}

sudo tlmgr update --self
sudo tlmgr update --all

packages_to_install="comment tabulary minted fvextra ifplatform xstring framed relsize"

for package in $packages_to_install; do
  install_package $package
done

echo 'export PATH="$PATH:/Library/TeX/texbin"' >> ~/.bash_profile

source ~/.bash_profile
