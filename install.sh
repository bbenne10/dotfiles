#!/bin/sh
find . -name "*.org" -exec emacs --batch -l org --eval "(org-babel-tangle-file \"{}\")" \; 2>&1 | grep "^Tangled"
