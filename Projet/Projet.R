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

table.pca = dudi.pca(df = table, scannf = FALSE, nf = 1,scale=T,center=T)
screeplot(table.pca,type="bar",main="Inertie en fonction de la composante de PCA")
barplot(table.pca$eig/sum(table.pca$eig)*100,names.arg = c("C1","C2","C3","C4","C5","C6","C7","C8","C9"),ylim = c(0,100),ylab="Pourcentage variance",xlab="Composante",main = "Pourcentage de variance expliquée en fonction de la composante")
score(table.pca)
table.pca$c1
table.pca$co
table.pca$li
inertia.dudi(table.pca,row.inertia = T)$row.abs


#Classification

dist = dist(table)
ward = hclust(dist,method = "ward.D")
pdf("Groupement méthode de Ward.pdf")
plot(ward,main="Méthode Ward")
rect.hclust(ward, border = c("green","red"),k = 5)
dev.off()
inertie = sort(ward$height,decreasing = T)
pdf("Inertie_Ward.pdf")
plot(inertie)
dev.off()
