# Country
source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")
subPlains <- c("boehmi", "borensis", "burchelli", "chapmani", "crawshayi", "grevys", "quagga")


#myPalette <- c(myPalette, "gray")
myPalette <- brewer.pal(n = 6, name = "Paired") # instead of 5 subsp, 6 countries!!!
E <- readRDS(".E.RDS")

temp <- cbind(PCA1 = E$vectors[,1], PCA2 = E$vectors[,2],
              full_lonlat %>% filter(species == "plains"))

# Sort countries according to latitude (so that gradient works)
temp$country <- factor(temp$country,
                       levels = c("Uganda", "Kenya", "Tanzania",
                                  "Zambia", "Botswana", "Namibia") %>% rev)

hulls <- plyr::ddply(temp,
                     "country",
                     function(X) find_hull(df = X, x = "PCA1", y = "PCA2"))

PCA_geography <- ggplot(data = temp,
                        mapping = aes(x = PCA1, y = PCA2,
                                      fill = country, col = country)) +
  geom_point() +
  scale_fill_manual(values = myPalette) +
  scale_color_manual(values = myPalette) +
  geom_polygon(data = hulls,
               mapping = aes(x = PCA1, y = PCA2,
                             col = country, fill = country),
               alpha = 0.5) +
  coord_flip() + scale_x_reverse() + scale_y_reverse() + theme(legend.position = "none")


# Add contour
hulls <- plyr::ddply(temp,
                     "country",
                     function(X) find_hull(df = X, x = "lon", y = "lat"))
Africa_watercolor <- readRDS(".Africa_watercolor.RDS")
myAfrica <- ggmap(Africa_watercolor) +
  geom_point(data = temp,
             mapping = aes(x = lon, y = lat, fill = country, col = country)) +
  scale_fill_manual(name = "Country  ",
                    values = myPalette) +
  scale_color_manual(values = myPalette) +
  guides(col = FALSE) +
  geom_polygon(data = hulls,
               mapping = aes(x = lon, y = lat, col = country, fill = country),
               alpha = 0.85) +

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

# Add  countries silhouettes
fortified_countries <- readRDS(".fortified_countries.RDS")
for (i in 1:length(fortified_countries)) {
  myAfrica <- myAfrica + geom_polygon(data = fortified_countries[[i]],
                                      aes(long, lat, group = group),
                                      color = "white", linetype = "longdash",
                                      size = 0.3, alpha = 0)
} 

legend_b <- get_legend(myAfrica)
combined <- plot_grid(PCA_geography + theme(legend.position = "none"),
                      myAfrica + theme(legend.position = "none"),
                      ncol = 2)

combined <- plot_grid(combined, legend_b,
                      ncol = 1, rel_heights = c(1, .2), rel_widths = c(1, 0.7))


by_subsp <- readRDS(file = "by_subspecies.RDS")



final <- plot_grid(by_subsp, combined, nrow = 2) +
  annotate("text", x = 0.02, y = 0.97, size= 12, label="A") +
  annotate("text", x =0.02, y = 0.6, size= 12, label="B") 

ggsave(filename = "figures/subspecies_PCA.png",
       plot = final,
       height = 2 * height, width = width)

# combined <- plot_grid(combined, legend_b, ncol = 1, rel_heights = c(1, .2))
# if (!interactive()) {
#   ggsave(filename = "figures/grouped_country.png",
#          plot = combined,
#          height = height, width = width)
# }
