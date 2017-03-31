library("adegenet")
library("hierfstat")
library("pegas")
library("dplyr")
library("xtable")
source("scripts/read_data.R")

#plains_only <- subspecies[-c(1:3, 63)] %>% as.character

geno_sorted[geno_sorted == 2] <- "11"
geno_sorted[geno_sorted == 1] <- "01"
geno_sorted[geno_sorted == 0] <- "00"


africaZebras <- df2genind(geno_sorted,
                          ploidy = 2,
                          sep="",
                          pop = species)

result <- pairwise.fst(africaZebras, res.type = "matrix") %>% round(digits = 3)
result[result %>% upper.tri(diag = T)] <- NA
result %>% xtable



africaZebras <- df2genind(geno_sorted[species == "plains",],
                          ploidy = 2,
                          sep="",
                          pop = factor(as.character(subspecies[species == "plains"]),
                                       levels = levels(subspecies)[1:5]))

result <- pairwise.fst(africaZebras, res.type = "matrix") %>% round(digits = 3)
result[result %>% upper.tri(diag = T)] <- NA
result %>% xtable


# borensis <- geno_sorted[subspecies == "borensis",]
# burchelli <- geno_sorted[subspecies == "burchelli",]
# both <- rbind(borensis, burchelli)
# 
# 
# ht <- function(myColumn) {
#   hom1 <- sum(myColumn == "00") 
#   het <- sum(myColumn == "01") 
#   hom2 <- sum(myColumn == "11") 
#   p <- (hom1 * 2 + het) / (2 * length(myColumn))
#   q <- (hom2 * 2 + het) / (2 * length(myColumn))
#   result <- 2 * p * q
#   return(result)
# }
# 
# hs <- function(column1, column2) {
#   (ht(column1)  * length(column1) + ht(column2) * length(column2)) /
#     (length(column1) + length(column2))
# }
# 
# hs(borensis[,598], burchelli[,598])
# 
# fst <- function(both, one, second) {
#   result = (ht(both) - hs(one, second)) / ht(both)
# }
# 
# ht_res <- apply(X = both, MARGIN = 2, FUN = ht) %>% mean
# hs_res <- 1:680 %>% as.list %>% lapply(FUN = function(i) hs(borensis[,i], burchelli[, i])) %>% unlist %>% mean
# 
# (ht_res - hs_res) / ht_res

