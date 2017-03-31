source("scripts/read_data.R")

M <- cov(geno)
N <- geno %*% t(geno)

output <- eigen(M)
output$vectors[,1:2]


