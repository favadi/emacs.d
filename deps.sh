#!/bin/bash
# Install external dependencies, require brew and golang installed

set -eux

# shell script
brew install shellcheck

# golang
brew install go
go get -u github.com/dougm/goflymake
go get -u github.com/golang/lint/golint
go get -u github.com/kisielk/errcheck
go get -u github.com/nsf/gocode
go get -u github.com/rogpeppe/godef
go get -u golang.org/x/tools/cmd/goimports

# python
brew install python
pip install --upgrade pip setuptools flake8 pylint virtualenvwrapper

brew install the_silver_searcher
