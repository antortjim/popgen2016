source("/home/antortjim/MEGA/Master/PG/project/scripts/read_data.R")

colnames(geno) <- subspecies
geno_sorted <- geno[selector2,]
rownames(geno_sorted) <- bim_sorted$id
png(filename = "figures/heatmap.png")
myHeatmap <- heatmap.2(geno_sorted,
                         Rowv = FALSE,
                         scale = "column",
                         labRow = NA,
                         dendrogram = "column")
dev.off()

zebras[myHeatmap$colInd,]
clusters <- cutree(myHeatmap$colDendrogram %>% as.hclust, k = 10)
plot(myHeatmap$colDendrogram)

saveRDS(object = clusters,
        file = ".clusters.RDS")
for(i in 1:10) {
  print(zebras[cutree(tree = myHeatmap$colDendrogram %>% as.hclust, k = 10) == i, ])
  print(" ")
  print(" ")
}


geno_t <- geno %>% t

state <- !(geno == 1)
# TRUE = HOMOCYGOUS

state <- sgeno %>% as.numeric() %>% matrix(nrow = nrow(state), ncol = ncol(state),
                                           dimnames = list(NULL, full_dataset$subspecies))


# 1 == homocygous
# 0 == heterocygous

df <- data.frame(
  x = rep(1:ncol(state), times = nrow(state)),
  y = rep(1:nrow(state), each = ncol(state)),
  #z = state[nrow(state):1,] %>% as.numeric() %>% as.factor
  z = state %>% as.numeric() %>% as.factor
)


myPalette <- c("#FFFF00", "#FF0000")
# ggplot(df, aes(x, y)) +
#   geom_tile(aes(fill = z), width = 1) +
#   scale_fill_manual(values = myPalette)

