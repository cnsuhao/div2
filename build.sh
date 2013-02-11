#!/bin/bash

cd `dirname $0`

pdflatex -halt-on-error -shell-escape portfolio.tex
pdflatex -halt-on-error -shell-escape portfolio.tex
pdflatex -halt-on-error -shell-escape portfolio.tex

if [ "$?" -eq 0 ]; then
    rm portfolio.log
    rm portfolio.toc
    find . -name "*.aux" -exec rm \{} \;
    find . -name "*.pyg" -exec rm \{} \;
fi
