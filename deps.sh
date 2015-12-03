#!/bin/bash
# Install external dependencies, require brew and golang installed

set -eux

# shell script
brew install shellcheck

# golang
brew install go
go get -u github.com/dougm/goflymake \
   github.com/golang/lint/golint \
   github.com/kisielk/errcheck \
   github.com/nsf/gocode \
   github.com/rogpeppe/godef \
   golang.org/x/tools/cmd/goimports \
   golang.org/x/tools/cmd/gorename

# python
brew install python
pip install --upgrade pip setuptools flake8 pylint virtualenvwrapper

brew install the_silver_searcher
