install.packages("xlsx")
table = read.xlsx("../pourcentage_chercheurs_belgique.xlsx", sheetIndex = 1)
rownames(table) = table$geo.time
table = table[,-1]
boxplot(table)
