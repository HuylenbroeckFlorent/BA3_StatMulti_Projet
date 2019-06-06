#ACP
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

#Classification

dist = dist(table)
ward = hclust(dist,method = "ward.D")
plot(ward,main="Méthode Ward")
rect.hclust(ward, border = c("green","red"),h = 1)
single = hclust(dist,method = 'single')
plot(single,main="Méthode single")
rect.hclust(single,border = c("green","red"),h = 1)
complete = hclust(dist,method = 'complete')
plot(complete, main = "Methode Complète")
rect.hclust(complete,border = c("green","red"),h = 1)
plot(dist)
plot(ward)
s.class(table)
cutree(ward,h = 2)
