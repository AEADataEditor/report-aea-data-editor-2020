#!/bin/bash

outfile=pandp-vilhuber-2020-$(date +%F).zip

zip -rp $outfile *tex AEA*pdf *bib images/ tables/*tex *.cls *.sty data/replication*txt *.bst
