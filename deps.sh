#!/bin/bash
# Install external dependencies, require brew and golang installed

set -eux

# shell script
brew install shellcheck

# golang
brew install go
go get -v -u github.com/nsf/gocode
go get -v -u github.com/rogpeppe/godef
go get -v -u github.com/dougm/goflymake
go get -v -u github.com/golang/lint/golint
go get -v -u github.com/kisielk/errcheck

# python
brew install python
pip install --upgrade pip setuptools flake8 pylint virtualenvwrapper
