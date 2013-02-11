#!/bin/bash

cd `dirname $0`

pdflatex -halt-on-error -shell-escape portfolio.tex
pdflatex -halt-on-error -shell-escape portfolio.tex
pdflatex -halt-on-error -shell-escape portfolio.tex

if [ "$?" -eq 0 ]; then
    rm portfolio.log
fi

find . -name "*.aux" -exec rm \{} \;
rm portfolio.toc
find . -name "*.pyg" -exec rm \{} \;

