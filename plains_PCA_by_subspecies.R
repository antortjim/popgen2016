source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")
subPlains <- c("boehmi", "borensis", "burchelli", "chapmani", "crawshayi", "grevys", "quagga")

# Plot PCA from plains only
# regenerate geno (called M here) so that it features the SNPs that are sensible to degradation
# (A-G C-T because the quagga is not used in this analysis)
M <- unfiltered_geno[,full_dataset$ID][,species == "plains"] # order the columns according to ID
#(grevys, plains alphabetically and quagga) and select plains only

M <- select_SNPs(M)$geno

E <- eigenstrat(M)
saveRDS(object = E, file = ".E.RDS")

df <- data.frame(species = "plains",
                 subspecies = subspecies[species == "plains"],
                 idx = (1:ncol(geno))[species == "plains"],
                 PCA1 = E$vectors[,1], PCA2 = E$vectors[,2])



p <- ggplot(data = df, aes(x = PCA1, y = PCA2,
                           col = subspecies, fill = subspecies)) +
  geom_point() +
  scale_fill_manual(values = myPalette) +
  scale_color_manual(values = myPalette) +
  coord_flip() + scale_x_reverse() + scale_y_reverse()

# if (!interactive()) {
#   p
#   ggsave(filename = "figures/plains_PCA.png",
#          plot = p,
#          height = height, width = width)
# }

# Add contour
hulls <- plyr::ddply(df, "subspecies",
                     function(X) find_hull(df = X, x = "PCA1", y = "PCA2"))

plains_countour_PCA <- p +
  geom_polygon(data = hulls, alpha = 0.5) +
  coord_flip() + scale_x_reverse() + scale_y_reverse() +
  guides(col = FALSE)

# if (!interactive()) {
#     ggsave(plot = plains_countour_PCA,
#          filename = "figures/plains_PCA_contour.png",
#          height = height, width = width)
# }

saveRDS(object = plains_countour_PCA,
        file = ".plains_contour_PCA")
