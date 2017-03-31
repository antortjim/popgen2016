# Read in each of the frequency files
setwd("MEGA/Master/PG/project/")
source("scripts/read_data.R")
setwd("zebra.maps.peds/")

# Read all .frq files and join them into a data frame
# Each subspecies is indicated
df <- lapply(subPlains, function(x) {
  paste(x, "_noNA.frq", sep = "") %>% read.table(h = T)
}) %>% plyr::rbind.fill()



# Remove all fixed alleles in each population
df <- df %>% filter(MAF != 0) %>% filter(MAF != 1)

# Add columns with the position on the chromosome and the pi-values for each polymorphic SNP

df$position <- as.numeric(gsub('[0-9]+_', '', df$SNP))



#df %>% group_by() %>% summarise(count = n())
df$pi <- plyr::dlply(df, "subspecies", function(x) compute_pi(x)) %>% unlist

to_ggplot2 <- df %>%
  filter(pi != Inf) %>%
  group_by(subspecies) %>% summarize(pi = mean(pi))

saveRDS(object = to_ggplot2, file = "pi.RDS")

ggplot(to_ggplot2,
       mapping = aes(x = subspecies, y = pi, fill = subspecies)) +
  geom_bar(stat = "identity") +
  labs(y = expression(pi), x = "") +
  guides(fill = FALSE) +
  
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 30, margin = margin(0, 20, 0, 0))) +
  
  scale_x_discrete(limits = subPlains %>% unlist,
                   labels = subPlains %>% lapply(simpleCap))
ggsave("../figures/nucleotide_diversity.png")
