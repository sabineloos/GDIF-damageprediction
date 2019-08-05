#!/usr/bin/env bash

Rscript -e "rmarkdown::render(input = 'DDF_nb.Rmd', 'html_document',output_dir = '../results', clean = TRUE)"
