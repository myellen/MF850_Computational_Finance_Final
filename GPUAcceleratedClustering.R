# GPU-Accelerated R in the Cloud with Teraproc Cluster-as-a-Service

library("rpud")
#
# function to populate a data matrix
#
test.data <- function(dim, num, seed=10) {
  set.seed(seed) 
  matrix(rnorm(dim * num), nrow=num)
}

run_cpu <- function(matrix) {
  dcpu <- dist(matrix)
  hclust(dcpu)
}

run_gpu <- function(matrix) {
  dgpu <- rpuDist(matrix)
  rpuHclust(dgpu)
}

#
# create a matrix with 20,000 observations each with 100 data elements
#
m <- test.data(100, 20000)

#
# Run dist and hclust to calculate hierarchical clusters using CPU
#

print("Calculating hclust with Sandy Bridge CPU")
print(system.time(cpuhclust <-run_cpu(m)))

#
# Run dist and hclust to calculate hierarchical clusters using GPU
#

print("Calculating hclust with NVIDIA GEFORCE 960M GPU")
print(system.time(gpuhclust <- run_gpu(m)))
plot(gpuhclust,hang = -1)