#!/bin/bash
set -euo pipefail

# shell script
# sudo port install shellcheck

# golang
export GOPATH=$HOME/go
sudo port install go
go get -u \
   github.com/alecthomas/gometalinter \
   github.com/nsf/gocode \
   github.com/rogpeppe/godef \
   golang.org/x/tools/cmd/goimports \
   golang.org/x/tools/cmd/gorename \
   golang.org/x/tools/cmd/gomvpkg \
   golang.org/x/tools/cmd/guru \
   github.com/jstemmer/gotags

gocode set autobuild true

gometalinter --install

# python
sudo port install python27
sudo port select --set python python27
sudo port install py-flake8 py-pylint
sudo port select --set flake8 flake8-27
sudo port select --set pylint pylint27
sudo port install py-virtualenvwrapper
sudo port select --set virtualenv virtualenv27

sudo port install the_silver_searcher

# javascript
sudo port install nodejs6
# npm install standard -g
# npm install tern -g
