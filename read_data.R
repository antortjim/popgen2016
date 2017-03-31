setwd("~/MEGA/Master/PG/project/")
library("snpMatrix")
library("ggmap")
library("MASS")
library("magrittr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("cowplot")
#library("cowplot")
library("ggthemes")
library("gplots")
theme_set(theme_minimal())
source("scripts/functions.R")
library("RColorBrewer")

myPalette <- brewer.pal(n = 5, name = "Dark2")


width <- 15
height <- 7.82

zebras <- read.table("zebra_package/ourPlainsQuaggaGrevys_popInfo.txt", header = T)
zebras_original <- zebras

zebras %>% group_by(country) %>% summarise(count = n())
zebras %>% group_by(subspecies) %>% summarise(count = n())

bim <- read.table("zebra_package/ourPlainsQuaggaGrevys_maxMissing0.2_maf.bim",
                  col.names = c("chr", "id", "morgan", "coord", "v1", "v2"))

longlat <- read.table("zebra_package/longlat.txt")
lon <- longlat$lon
lat <- longlat$lat

input_data <- "zebra_package/plainsQuaggaGrevys2017_maxMissing0.2_maf_sorted"
prefix <- "plainsQuaggaGrevys2017_maxMissing0.2_maf_sorted"

#input_data <- "../practica/mon27/pruneddata"
data <- read.plink(input_data)
geno <- as.integer(data@.Data) %>% matrix(nrow = nrow(data@.Data)) %>% t
# Recode so that 0 1 2 indicates the # of the allele in .bim
geno[geno == 0] <- NA
geno <- geno - 1
#table(geno)
unfiltered_geno <- geno # save unfiltered geno 
#geno <- temp

ROH <- select_SNPs(geno) # select meaningful SNPs only
bim_ROH <- bim[ROH$selector,] %>% arrange(chr, coord)
ROH <- cbind(zebras, ROH %>% t %>% as.data.frame) %>% dplyr::arrange(species, subspecies) # bind zebras info and geno databas

result <- select_SNPs(geno, bim) # select meaningful SNPs only
geno <- result$geno # return filtered database
selector <- result$selector #  return logical vector showing which are the meaningful SNPs
bim <- bim[selector,]       # subset the bim file with this logical vector

bim_sorted <- bim %>% arrange(chr, coord) # sort the bim file according to chromosome and coordinate
# that way SNPs follow the genome
selector2 <- bim_sorted$id2               # generate new selector
# showing how to select the SNPs
# in the original databse so that they follow the right order

full_dataset <- cbind(zebras, geno %>% t %>% as.data.frame) %>% dplyr::arrange(species, subspecies) # bind zebras info and geno databas

# Sort geno by species and subspecies by extracting the geno columns in the sorted full_dataset
# Full dataset is sorted by species, subspecies
geno <- dplyr::select(full_dataset, -(ID:species)) %>% as.matrix() %>% t
full_lonlat <- cbind(lon, lat, full_dataset[-63,])
clusters <- readRDS(".clusters.RDS")
full_lonlat$clusters <- as.factor(clusters[-63]) # exclude quagga

zebras <- full_dataset %>% dplyr::select(ID:species)
subspecies <- zebras[, 5]
species <- zebras[, 6]

homo <- !(geno == 1)

subPlains <- c("plains", levels(subspecies)) %>% as.list
geno_sorted <- full_dataset %>% .[,7:ncol(full_dataset)]

