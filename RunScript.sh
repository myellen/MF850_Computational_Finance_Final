#!/bin/bash
set -e

exitstatus=0

for file in *.R
do
    Rscript -e "source(\"$file\", echo=TRUE)"
done

exit $exitstatus
