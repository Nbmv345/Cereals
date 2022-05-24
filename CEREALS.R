cereals.df <- read.csv("Cereals.csv")

pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating))
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores, 5)
pcs <- prcomp(na.omit(cereals.df[,-c(1:3)]))
summary(pcs)
pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)
