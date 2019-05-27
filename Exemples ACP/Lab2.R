#1.1
#1)
library(MASS)
#3)
pdf("Lab2.pdf")
w = mvrnorm(n = 1000,rep(0,2),Sigma = cor_mat)
x = w[,1]
y = w[,2]
#4)
plot(x,y,main = "1.1.4",asp = 1)
points(mean(x),mean(y),col = "red")
#5)
lm = lm(x~y)
abline(coefficients(lm),col = "yellow")
#6)
var(w)
cov(w)
#7)
cor(w)
w_ = scale(w)
var(w_)
cov(w_)
cor(w_)
#8)
eig = eigen(cov(w))
#9)
u = eigen(cov(w_))$vectors[,1]
v = eigen(cov(w_))$vectors[,2]
p=u[2]/u[1]
q=v[2]/v[1]
py=mean(y)-p*mean(x)
qy=mean(y)-q*mean(x)
abline(a = py,b = p,col = "blue")
abline(qy,q,col="green")

#10)
newW = as.matrix(w) %*% eig$vectors
plot(newW)
#2.1
#read.table("chemin/relatif/fichier",h=T)
#1)
library(ade4)
#2)
pca1 = read.table("../Files/pca1.txt",h=T)
#3)
rownames(pca1) = pca1$IDENT
pca1 = pca1[,-1]
freq1 = pca1$FREQ
pca1 = pca1[,-length(pca1)]
#4)
boxplot(pca1)
boxplot(scale(pca1,T,scale = TRUE))
#5)
cor_pca1 = cor(pca1) 
max(abs(cor_pca1[abs(cor_pca1)<1.0 & abs(cor_pca1)>=0.5]))
#6)
diag_cor_pca1 = eigen(cor_pca1)
summary(diag_cor_pca1)
diag_cor_pca1
#7)
##a)
pca1.pca = dudi.pca(pca1)
pca1.pca$eig
diag_cor_pca1$values
#Les valeurs propres sont exactement les mêmes
##b)
plot(pca1.pca$co)
plot(pca1.pca$c1)
##c)
s.corcircle(pca1.pca$co)
##d)
plot(pca1.pca$li)
#8)
inertia = inertia.dudi(pca1.pca,row.inertia = T,col.inertia = T)
#cos_square = inertia$row.rel || inertia$col.rel
#9)
scatter(pca1.pca)
#10)
for (i in 1:15) {
  plot(pca1[,i]~pca1.pca$li[,1],main = paste("plot n°",toString(i),sep = " "))
  abline(lm(pca1[,i] ~ pca1.pca$li[,1]))
}
#11)
score(pca1.pca)
#12)
supcol(pca1.pca,Xsup = freq1)
#13)
?reconst
reconst(pca1.pca,nf = 2)
#14)
func <- function(ACP,deg){
  
}


#2.2
results = deug$tab
finals = deug$result
mean = deug$cent
#1)
data("deug")
#2)
boxplot(results,main = "Résultats")
#3)
factor(finals,c("D","C-","C","B-","B","A","A+"),order = T)
#4)
plot(table(finals))
#5)
mean = sort(mean,T)
barplot(mean,main = "Ordre d'importance des matières")
#6)
results.pca = dudi.pca(results,scannf = F,nf = 2)
#7)
scat = scatter(results.pca)
#8)

dev.off()
