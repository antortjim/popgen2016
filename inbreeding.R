#R code:
setwd("~/MEGA/Master/PG/project/")
source("scripts/read_data.R")
library("ggplot2")
library("RColorBrewer")
myPalette <- c(rev(brewer.pal(n = 9, name = "Blues")[4:9]), "Red", "Green")



names <- read.table('inbreeding/names.txt', h = T)
spec <- names$species
sub <- names$subspecies
f_values <- cbind(read.table('inbreeding/no.split.zebra.het', h = T),
                             species = names$species, subspecies = names$subspecies)

plains <- f_values %>% filter(species == "plains")
plains$subspecies <- "plains"
f_values <- rbind(f_values, plains)
f_values$subspecies <- factor(f_values$subspecies,
                              levels = c("plains", "boehmi", "borensis",
                                         "burchelli", "chapmani", "crawshayi",
                                         "grevys", "quagga"))
f_values$species <- factor(f_values$species,
                           levels = c("plains", "grevys", "quagga"))

f_values %>% group_by(subspecies) %>% summarise(F = mean(F))

ggplot(data = f_values,
       mapping = aes(x = subspecies, y = F, fill = subspecies)) +
  geom_boxplot() +
  labs(x = "") +
  theme_bw() + guides(fill = FALSE) +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20, margin = margin(0, 20, 0, 0))) +
  #facet_grid(~ species, scales = "free", space = "free") +
  scale_fill_manual(values = myPalette)

F <- f_values %>% group_by(subspecies) %>% summarize(F = median(F))
pi <- readRDS("pi.RDS")

cbind(F, pi[,2] * 1000) %>% gather(parameter, value, F:pi) %>%
  ggplot(mapping = aes(x = subspecies, y = value,
                       col = parameter, group = parameter)) +
  geom_line()


ggsave("figures/inbreeding.png", width = width, height = height)
