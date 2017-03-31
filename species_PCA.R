source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")
subPlains <- c("boehmi", "borensis", "burchelli", "chapmani", "crawshayi", "grevys", "quagga")


# Eigen vector decomposition
# shit1 <- geno[,-(1:2)]
# shit1 <- select_SNPs(shit1)$geno

# E <- eigenstrat(shit1) # eigenstrat receives a matrix where rows = SNPs and columns = individuals
#E$vectors

# plot(1:61, abs(E$vectors[,1]))
# points(1:61, abs(E$vectors[,2]), pch = 19)
E <- eigenstrat(geno)

my_cols <- species
levels(my_cols) <- c("lightblue","Dark red","lightgreen")


# Plot PCA of all zebras
df <- data.frame(species = species,
                 subspecies = subspecies, idx = 1:63,
                 PCA1 = E$vectors[,1], PCA2 = E$vectors[,2])


ggplot(data = df, aes(x = PCA1, y = PCA2, col = species)) +
  geom_point() +
  scale_color_discrete(name = "Species",
                       breaks = c("grevys", "plains", "quagga"),
                       labels = c("Grevy's", "Plains", "Quagga")) +
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 25))


if (!interactive()) {
p  
ggsave(filename = "figures/PCA_species.png",
       plot = p, 
       height = height, width = width)
}



# rm(p)
# myData <- kde2d(df$PCA1,
#                 df$PCA2)
# 
# x <- rep(myData$x, times = 25)
# y <- rep(myData$y, each = 25)
# z <- myData$z %>% as.numeric
# myResult <- data.frame(x = x, y = y, z = z)
# 
# p <- ggplot(data = myResult, aes(x = x, y = y, z = z)) +
#   geom_contour(bins = 2)
# p
# 
# subsp <- c("boehmi", "borensis", "burchelli", "chapmani", "crawshayi")
# s <- "borensis"
# 
# mySubset <- kde2d(filter(df, subspecies == s)$PCA1,
#                   filter(df, subspecies == s)$PCA2)
# 
# 
# x <- rep(mySubset$x, times = 25)
# y <- rep(mySubset$y, each = 25)
# z <- mySubset$z %>% as.numeric
# result1 <- data.frame(x = x, y = y, z = z)
# 
# p <- ggplot(data = result, aes(x = x, y = y, z = z)) +
#   geom_contour(bins = 2)
# 
# p + layer(result1,
#   mapping = aes(x = x, y = y, z = z),
#   geom = "contour", params = list(bins =2)
#   
# )
# 
# 
# for (s in subsp) {
# mySubset <- kde2d(filter(df, subspecies == s)$PCA1,
#                   filter(df, subspecies == s)$PCA2)
# 
# x <- rep(df$x, times = 25)
# y <- rep(df$y, each = 25)
# z <- df$z %>% as.numeric
# result <- data.frame(x = x, y = y, density = z)
# 
# p <- ggplot(data = result, aes(x = x, y = y, z = density)) +
#   geom_contour(bins = 2)
# }
# p
# 
# 
# 
# area <- p +  geom_polygon(data = hulls, alpha = 0.5)
# area  

## K means
# df <- full_lonlat %>% filter(species == "plains") %>% .[,c("lon", "lat")]
# clustering <- kmeans(df, centers = 5)
# temp <- cbind(cluster = clustering$cluster %>% as.factor,
#                      PCA1 = E$vectors[,1], PCA2 = E$vectors[,2],
#                      full_lonlat %>% filter(species == "plains"))
# 
# 
# hulls <- plyr::ddply(temp,
#                      "cluster",
#                      function(X) find_hull(df = X, x = "PCA1", y = "PCA2"))
# 
# 
# PCA_geography <- ggplot(data = temp,
#        mapping = aes(x = PCA1, y = PCA2, fill = cluster, col = cluster)) +
#   geom_point() +
# scale_fill_manual(values = myPalette) +
#   scale_color_manual(values = myPalette) +
#   geom_polygon(data = hulls,
#                mapping = aes(x = PCA1, y = PCA2, col = cluster, fill = cluster),
#                alpha = 0.5)
# 
# hulls <- plyr::ddply(temp,
#                      "cluster",
#                      function(X) find_hull(df = X, x = "lon", y = "lat"))
# 
# myAfrica <- ggmap(Africa) +
#   geom_point(data = temp,
#              mapping = aes(x = lon, y = lat, fill = cluster, col = cluster)) +
#   scale_fill_manual(values = myPalette) +
#   scale_color_manual(values = myPalette) +
#   geom_polygon(data = hulls,
#                mapping = aes(x = lon, y = lat, col = cluster, fill = cluster),
#                alpha = 0.5) +
#   
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank())
# 
# for (i in 1:length(fortified_countries)) {
#   myAfrica <- myAfrica + geom_polygon(data = fortified_countries[[i]],
#                                       aes(long, lat, group = group),
#                                       color = "white", linetype = "longdash", size = 0.3, alpha = 0.5)
# } 
# 
# 
# combined <- plot_grid(PCA_geography,
#                       myAfrica,
#                       labels = c("A", "B"),
#                       ncol = 2)
# 
# combined




