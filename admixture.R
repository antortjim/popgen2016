source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")

idx <- which(zebras_original$species == "grevys")

exclude <- matrix(c(idx, rep(1, 3)), byrow = F, ncol = 2)
write.table(exclude, file = "zebra_package/exclude_grevys.txt", sep = "\t",
            col.names = FALSE, row.names = FALSE)



## Admixture
admixture <- "~/MEGA/Master/PG/opt/admixture"


# setwd("zebra_package")
# system("rm cvoutput")
# for (i in c(2, 3, 6)) {
#   command <- paste(admixture, " --cv ", prefix, ".bed ",
#                    i, " >> cvoutput", sep = "")
#   message(command)
#   system(command)
# }
#  setwd("..")

cv_error <- system('grep -i "CV error" cvoutput', intern = T) %>%
  strsplit(split = ": ") %>% unlist %>% matrix(nrow = 4, ncol = 2, byrow = T) %>%
  .[,2] %>% as.numeric

df <- data.frame(K = 2:5, e = cv_error)
ggplot(data = df, aes(x = K, y = e, group = 1)) +
  theme_classic() +
  geom_point() +
  geom_line(col = "red")

k <- c(2, 3, 6)


Q <- (paste(input_data, k, "Q", sep = ".") %>% mapply(FUN = function(x) read.table(x)[full_dataset$ID,]))
 

index <- 1:4 %>% as.list

par(mfrow=c(3,1))
png("figures/base_admixture.png")
for(i in c(1, 2, 3)) {
  barplot(t(as.matrix(Q[[i]])), col = c("lightgreen","Dark red","lightblue","yellow","pink", "red")[1:(i+1)],
          border = NA, main = paste("K", k[i], sep = " = "),
          names.arg = subspecies, cex.names = 0.8, las = 2, ylab ="ancestry")
}
dev.off()

# Trasnform Q to ggplot standards
P <- index %>% lapply(function(x) cbind(id = Q[[x]] %>% rownames %>% as.factor %>% as.numeric() %>% sort %>% as.factor, Q[[x]]))
P <- index %>% lapply(function(x) gather(P[[x]], frac, prop, V1:ncol(P[[x]]), factor_key = TRUE))


myPalette <- c("lightgreen","Dark red","lightblue","yellow","pink")

# pl <- list()
# for(i in 1:4) {
#   message(i)
#   title = paste("K", i + 1, sep = " = ")
#   p <- ggplot(data = P[[i]]) +
#     geom_bar(aes(x = id, y = prop, fill = frac), stat = "identity") +
#     ggtitle(title) + 
#     scale_x_discrete(breaks = 1:63,
#                      labels = subspecies) +
#     scale_y_continuous(name = "ancestry", breaks = seq(0, 1, .25)) +
#     #theme_void() +
#     guides(fill = FALSE) +
#     scale_fill_manual(values = myPalette[1:(i+1)]) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
#     theme(axis.title.y = element_text(size = rel(.8))) +
#     labs(x = "")
#   
#   pl[[i]] <- p
#   }
# admixture <- plot_grid(pl[[1]], pl[[2]], pl[[3]], pl[[4]], nrow = 4, align = "v")
# admixture


P <- index %>% lapply(function(x) cbind(P[[x]], K = x+1))
P <- plyr::rbind.fill(P)

p <- ggplot(data = P) +
  geom_bar(aes(x = id, y = prop, fill = frac), stat = "identity") +
  facet_wrap(~K, nrow = 4) +
  scale_x_discrete(breaks = 1:63,
                   labels = subspecies) +
  scale_y_continuous(name = "ancestry", breaks = seq(0, 1, .25)) +
  #theme_void() +
  guides(fill = FALSE) +
  scale_fill_manual(values = myPalette) +
  theme_bw() +
  theme(axis.text = element_text(angle = 90, hjust = 1, size = 8),
        axis.title = element_text(size = 30, margin = margin(0, 20, 0, 0))) +
  labs(x = "")
p
ggsave("figures/ggplot_admixture.png")