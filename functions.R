library("geosphere")

find_hull <- function(df, x, y) {
  df[chull(df[,x], df[,y]), ]
}


select_SNPs <- function(geno, bim=NULL) {
  temp <- bim
  temp$row_id <- rownames(bim)
  # Taken and modified from Apes practical lesson
  # Select SNPs that have been typed for all individuals
  # Select the rows that where there's no NA
  
  # Keep sites that are polymorphic i.e
  # The number of genotypes available for that SNP is bigger than one
  # Genotypes where all individuals have the same genotype are discarded
  polymorphic <- apply(X = geno, FUN = function(x) { length(unique(na.omit(x))) }, MARGIN = 1) > 1
  
  if(!is.null(bim)) {
    not_trustworthy_1 <- temp %>% filter(v1 == "G" | v1 == "A", v2 == "A" | v2 == "G") %>% .$row_id
    not_trustworthy_2 <- temp %>% filter(v1 == "C" | v1 == "T", v2 == "T" | v2 == "C") %>% .$row_id
    not_trustworthy <- rep(TRUE, times = nrow(bim))
    row_ids <- c(not_trustworthy_1, not_trustworthy_2) %>% as.integer
    not_trustworthy[row_ids] <- FALSE
    selector <- complete.cases(geno) & polymorphic & not_trustworthy
  } else {
    
    selector <- complete.cases(geno) & polymorphic
  }
  
    geno <- geno[selector,]
  
  return(list(geno = geno, selector = selector))
}



eigenstrat<-function(geno){
  
  
  # number of SNPs kept
  snp <- nrow(geno) 
  
  # number of individuals
  ind <- ncol(geno)
  
  # frequency of the allele in the .bim file (for all SNPs)
  freq <- rowSums(geno) / (2 * ind)
  avg <- freq * 2
  
  # remove the mean and divide by the standard deviation (standarize)
  M <- (geno - avg) / sqrt(freq * (1 - freq)) # all snps have the same mean and the same SE
  
  X <- t(M) %*% M # covariance matrix
  
  X <- X / (sum(diag(X)) / (snp - 1)) # modifies the eigenvalues but not the eigenvector
  E <- eigen(X)
  
  mu <- (sqrt(snp - 1) + sqrt(ind)) ^ 2 / snp #?
  sigma <- (sqrt(snp - 1) + sqrt(ind)) / snp * (1 / sqrt(snp - 1) + 1 / sqrt(ind)) ^ (1/3) #?
  E$TW <- (E$values[1] * ind / sum(E$values) - mu) / sigma
  E$mu <- mu
  E$sigma <- sigma
  class(E) <-"eigenstrat"
  return(E)
}

plot.eigenstrat <- function(x, col = 1, ...)
{
  plot(x$vectors[, 1:2], col = col,...)
}

print.eigenstrat <- function(x) {
  
  cat("statistic", x$TW, "\n")
}

ROH_measurer <- function(homo, bim_sorted, species, subspecies, unit) {
  rownames(homo) <- NULL
  chr <- bim_sorted$chr
  data <- cbind(bim_sorted, homo)
  df <- data.frame()
  
  for (c in unique(chr)) {
    print(c)
    mySubset <- dplyr::filter(data, chr == c)
    homo <- dplyr::select(mySubset, -(chr:v2))
    
  
    for (i in 1:ncol(homo)) {
     print(i)
     material <- c(F, homo[,i], F) # so that all blocks have a false after it
                                # and we can use this false to mark the end of
                                # the last interval of trues
     coord <- mySubset$coord
     
     if(!any(material)) {
       next
     }
     run <- rle(material)$lengths
     idx <- (cumsum(run) - run)
     starting <- idx[seq(from = 2, to = length(idx), by = 2)]
     idx <- idx[-1] - 1
     stopping <- idx[seq(from = 2, to = length(idx), by = 2)]
    
     
     intervals <- matrix(c(coord[starting], coord[stopping]),
                         ncol = 2, byrow = F)
     lens <- intervals[,2] - intervals[,1]
     
     Mlens <- lens / unit

     df <- rbind(df, data.frame(individual = rep(i, length(lens)),
                                chr = rep(c, length(lens)),
                                species = rep(species[i], length(lens)),
                                subspecies = rep(subspecies[i], length(lens)),
                                len = Mlens))
     
  
    }
  }
  return(df)
}

#df <- ROH_measurer(homo, bim_sorted, species, subspecies)


ROH_plotter <- function(full_dataset, bim_sorted, unit) {
  library("ggplot2")
  geno <- full_dataset %>% dplyr::select(-(ID:species)) %>%
    as.matrix %>% t
  
  species <- full_dataset$species
  subspecies <- full_dataset$subspecies
  
  homo <- geno == 0 | geno == 2
 
  # Not taking into account actual coordinates of the SNPs (not uniformly distributed) 
  runs <- homo %>% apply(FUN = rle, MARGIN = 2)
  
  # taking it into account
  df <- ROH_measurer(homo, bim_sorted, full_dataset$species, full_dataset$subspecies, unit)

  # roh <- runs %>% lapply(FUN = function(x) x$lengths[x$values == T] %>% tabulate(nbins = 100)) %>%
  #   unlist() %>% matrix(ncol = 100, nrow = 63, byrow = T) %>% t %>% as.data.frame %>% plyr::rbind.fill()
  # 
  # individual <- colnames(roh)
  # 
  # dummy1 <- runs %>% lapply(FUN = function(x) x$lengths[x$values == T])
  # dummy2 <- dummy1 %>% lapply(length) %>% unlist()
  # 
  # df <- data.frame()
  # acum <- 1
  # for (i in 1:length(dummy1)) {
  #   message(i)
  #   current_row <- acum:(acum+dummy2[i] - 1)
  #   df[current_row, "species"] <- species[i]
  #   df[current_row, "subspecies"] <- subspecies[i]
  #   df[current_row, "individual"] <- individual[i]
  #   df[current_row, "len"] <- dummy1[[i]]
  #   acum <- acum + dummy2[i]
  # }
  # 
  # df$individual <- factor(df$individual, levels = df$individual %>% unique)
  
  
  p <- ggplot(data = df, aes(x = individual, y = len)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:63,
                     labels = subspecies) +
    theme(axis.text = element_text(angle = 90, hjust = 1, size = 8),
          axis.title = element_text(size = 30, margin = margin(0, 20, 0, 0))) +
    labs(x = "")
  print(p)
  
  return(df)
}

distance <- function(x1, x2) {
  
  
  x1 <- c(x1$lon, x1$lat)
  x2 <- c(x2$lon, x2$lat)
  
  result <- distGeo(x1, x2) / 1000 # distance in kilometers
  
  return(result)
}

# Function for estimating the expected heterozygosity (2pq)
het <- function(x) {
  2 * x * (1 - x)
}
# Function for computing pi SNPs wise
# 2pq * SNPs / length_sequence
compute_pi <- function(df) {
  het(df$MAF) * (nrow(df) / (tail(df, 1)$position - head(df, 1)$position))
}

simpleCap <- function(x) {
  s <- strsplit(x, split = " ")[[1]]
  result <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  return(result)
}

