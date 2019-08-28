#!/usr/bin/env bash

Rscript -e "rmarkdown::render(input = 'GDIF_nb.Rmd', 'html_document',output_dir = '../docs/GDIF_nb.html', clean = TRUE)"
