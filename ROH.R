source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")
library("xtable")



df <- ROH_plotter(full_dataset, bim_sorted, unit = 1e6)
clean_df <-  df %>% filter(len > 0)

df2 <- clean_df %>% group_by(subspecies, len) %>% dplyr::summarise(v = n()) %>% as.data.frame
df3 <- data.frame()
for(i in 1:nrow(df2)) {
  for(j in 1:df2[i,]$v) {
    df3 <- rbind(df3, df2[i, 1:2])
  }
}

df3

ggplot(data = df3, aes(x = subspecies, y = len, fill = subspecies)) +
  geom_boxplot()


df2 <- clean_df %>% group_by(species, len) %>% dplyr::summarise(v = n()) %>% as.data.frame
df3 <- data.frame()
# Instead of having a variable that counts how many N times a similar tract shows up,
# copy the row N times and remove that variable
for(i in 1:nrow(df2)) {
  for(j in 1:df2[i,]$v) {
    df3 <- rbind(df3, df2[i, 1:2])
  }
}

df3

ggplot(data = df3, aes(x = species, y = len, fill = species)) +
  geom_boxplot()

ggplot(data = df3, aes(x = len, fill = species, col = species)) +
  geom_density(alpha = 0.25, size = 2)


ggplot(data = df3, aes(x = len, y = ..density.., fill = species, col = species)) +
  geom_histogram(alpha = 0.25) + facet_wrap(~ species) +
  theme_bw() +
  scale_x_continuous(name = "ROH length (Mb)") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, margin = margin(0, 20, 0, 0)),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        strip.text.x = element_text(size = 23)) +
  guides(color = FALSE, fill = FALSE) +
  labs(y = "Density")
# ggtitle("Density") +
# theme(plot.title = element_text(hjust = -0.1, vjust = -0.2))

#plot.margin = rep(grid::unit(0.75,"in"),4))

ROH_expectation <- df3 %>% group_by(species) %>% summarise(mu = mean(len))

xList <- xtableList(list(ROH_expectation))


ggsave("figures/ROH.png")


homocigosity_table <- plyr::rbind.fill(lapply(X = 1:nrow(geno) %>% as.list,
                                              FUN = function(i) data.frame(SNP = i,
                                                                           het = tabulate(bin = homo[i,] %>% as.numeric %>% as.factor, nbins = 2)[1],
                                                                           homo = tabulate(bin = homo[i,] %>% as.numeric %>% as.factor, nbins = 2)[2])))

homocigosity_table$chr <- bim_sorted$chr

homocigosity_table$chr %>% table
homocigosity_table$homocigosity <- homocigosity_table$homo / 63

df <- homocigosity_table %>% arrange(homocigosity)
df$id <- 1:nrow(geno)

ggplot(data = df,
       mapping = aes(x = id, y = homocigosity)) +
  geom_line()


homo[,63]

homo[,-63]

#df %>% ggplot(aes(x, y)) +
#   geom_tile(aes(fill = z), width = 1) +
#   scale_fill_manual(values = myPalette)
