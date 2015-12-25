#!/bin/bash
# Install external dependencies, require brew and golang installed

set -eux

# shell script
brew install shellcheck

# golang
brew install go
export GOPATH=$HOME/.emacs.d/go
go get -u \
   github.com/alecthomas/gometalinter \
   github.com/nsf/gocode \
   github.com/rogpeppe/godef \
   golang.org/x/tools/cmd/goimports \
   golang.org/x/tools/cmd/gorename \
   golang.org/x/tools/cmd/gomvpkg

gometalinter --install --update

# python
brew install python
pip install --upgrade pip setuptools flake8 pylint virtualenvwrapper

brew install the_silver_searcher
