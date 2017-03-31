source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")
subPlains <- c("boehmi", "borensis", "burchelli", "chapmani", "crawshayi", "grevys", "quagga")

# Plot contours on Longitude and Latitude data

hulls <- plyr::ddply(filter(full_lonlat, species == "plains"),
                     "subspecies",
                     function(X) find_hull(df = X, x = "lon", y = "lat"))



p <- ggplot(data = filter(full_lonlat, species == "plains"),
            aes(x = lon, y = lat, col = subspecies, fill = subspecies)) +
  geom_point() +
  scale_fill_manual(values = myPalette) +
  scale_color_manual(values = myPalette)
plains_countour_geography <- p + geom_polygon(data = hulls, alpha = 0.5)

if (!interactive()) {
  ggsave(plot = plains_countour_geography,
         filename = "figures/plains_geography_contour.png",
         height = height, width = width)
}
combined <- plot_grid(plains_countour_genotype,
                      plains_countour_geography,
                      labels = c("A", "B"),
                      ncol = 2)
if (!interactive()) {
  ggsave(filename = "figures/grouping_subspecies.png",
         plot = combined,
         height = height, width = 2 * width)
}
