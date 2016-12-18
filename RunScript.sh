#!/bin/bash
set -e

exitstatus=0

for file in "$@"
do
    Rscript -e "lintr::lint(\"$file\")"
    Rscript -e "source(\"$file\", echo=TRUE)"
done

exit $exitstatus
