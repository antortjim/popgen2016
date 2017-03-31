source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")

#subPlains <- c("boehmi", "borensis", "burchelli", "chapmani", "crawshayi", "grevys", "quagga")


## Plot contours on Lon and Lat with Africa as the Background :)
hulls <- plyr::ddply(filter(full_lonlat, species == "plains"),
                     "subspecies",
                     function(X) find_hull(df = X, x = "lon", y = "lat"))
hulls$subspecies <- factor(hulls$subspecies,
                           levels = levels(hulls$subspecies)[1:5])
  
Africa <- readRDS(".Africa.RDS")
Africa_watercolor <- readRDS(".Africa_watercolor.RDS")


fortified_countries <- readRDS(".fortified_countries.RDS")

myAfrica <- ggmap(Africa_watercolor) +
  geom_point(data = filter(full_lonlat,
                           species == "plains"
  ),
  mapping = aes(x = lon, y = lat, col = subspecies)) +
  #scale_fill_manual(values = myPalette) +
  scale_color_manual(values = myPalette) +
  geom_polygon(data = hulls,
               mapping = aes(x = lon, y = lat, col = subspecies, fill = subspecies),
               alpha = 0.85) +
  scale_fill_manual(name = "Subspecies   ",
                      breaks = levels(hulls$subspecies),
                      labels = c("Boehmi", "Borensis", "Burchelli", "Chapmani", "Crawshayi"),
                      values = myPalette) + guides(col = FALSE)

for (i in 1:length(fortified_countries)) {
  myAfrica <- myAfrica + geom_polygon(data = fortified_countries[[i]],
                                      aes(long, lat, group = group),
                                      col = "white", alpha = 0,
                                      linetype = "longdash", size = 0.3)
}

myAfrica <- myAfrica +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position="bottom",
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30))



plains_contour_PCA <- readRDS(file = ".plains_contour_PCA")

legend_b <- get_legend(myAfrica)

combined <- plot_grid(plains_contour_PCA + theme(legend.position = "none"),
                      myAfrica + theme(legend.position = "none"),
                      ncol = 2)

combined <- plot_grid(combined, legend_b, ncol = 1, rel_heights = c(1, .2),
                      rel_widths = c(1, 0.7))

saveRDS(combined, "by_subspecies.RDS")

# ggsave(filename = "figures/grouped_subspecies.png",
#        plot = combined,
#        height = height, width = width)
