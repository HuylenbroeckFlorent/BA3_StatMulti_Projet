#ACP
if (!require("xlsx")){
  install.packages("xlsx")
  require(xlsx)
}
library(xlsx)
library(ade4)
library(MASS)
library(corrplot)
table = read.xlsx("../pourcentage_chercheurs_belgique.xlsx", sheetIndex = 1)
rownames(table) = table$geo.time
table = table[,-1]
pdf("Boxplots.pdf")
boxplot(table,main="Boxplot des données")
boxplot(scale(table,center = T,scale = T),main = "Boxplot des données centrées réduites")
dev.off()

cor_table = cor(table)
pdf("Corrplot.pdf")
corrplot(cor_table,type = "upper", order="hclust", tl.col="black",tl.srt=45,main="Corrplot des données de la matrice de corrélation")
dev.off()
diag_cor_table = eigen(cor_table)

table.pca = dudi.pca(df = table, scannf = FALSE, nf = 2)
pdf("Tendance des données.pdf")
score(table.pca)
dev.off()
pdf("Cercle de corrélation_Axes.pdf")
s.corcircle(table.pca$c1)
dev.off()
pdf("Biplot.pdf")
scatter(table.pca)
dev.off()

#Classification

dist = dist(table)
ward = hclust(dist,method = "ward.D")
pdf("Groupement méthode de Ward.pdf")
plot(ward,main="Méthode Ward")
rect.hclust(ward, border = c("green","red"),h = 1)
dev.off()
single = hclust(dist,method = 'single')
pdf("Groupement méthode single.pdf")
plot(single,main="Méthode single")
rect.hclust(single,border = c("green","red"),h = 1)
dev.off()
complete = hclust(dist,method = 'complete')
pdf("Groupement méthode complète.pdf")
plot(complete, main = "Methode Complète")
rect.hclust(complete,border = c("green","red"),h = 1)
dev.off()