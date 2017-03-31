source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")

diversity <- geno %>%
  as.data.frame %>%
  summarise_each(funs(var(.))) %>% as.numeric()




df <- data.frame(d = diversity, species = species,
                 subspecies = subspecies, idx = 1:63)


ggplot(data = df) + geom_point(aes(x = idx, y = d, col = species)) +
  scale_y_continuous(name = "Variance") +
  scale_x_continuous(name = "Zebra")

ggplot(data = df, aes(x = d, col = species)) + geom_density(lwd = 2)

ggplot(data = df, aes(x = species, y = d, fill = species)) +
  geom_boxplot() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(x = "Species", y = "Genotypic Variance")
ggsave("figures/genotypic_variance.png")
