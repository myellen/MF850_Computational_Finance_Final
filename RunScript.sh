#!/bin/bash
set -e

Rscript -e "source(\"test.R\", echo=TRUE)"
#Rscript -e "f1(1,2)"

#Rscript "test.R"
Rscript --verbose "cm_v1.R"
Rscript "data_exploration-12-5.R"
Rscript "GPUAcceleratedClustering.R"
Rscript "NeuralNet.R"

