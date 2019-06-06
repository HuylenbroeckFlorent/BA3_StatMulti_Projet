install.packages("xlsx")
library(xlsx)
table = read.xlsx("../pourcentage_chercheurs_belgique.xlsx", sheetIndex = 1)
rownames(table) = table$geo.time
table = table[,-1]
boxplot(table)
boxplot(scale(table,center = T,scale = T))
cor_table = cor(table)
table.pca = dudi.pca(df = table, scannf = FALSE, nf = 1)
s.corcircle(table.pca$tab)
