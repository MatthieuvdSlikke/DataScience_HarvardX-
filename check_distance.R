library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

#computes the Euclidean distance between each observation and stores it in the object d
d <- dist(tissue_gene_expression$x)

#compare distances between observations 1 and 2, 39 and 40, 73 and 74
abs(d[1]-d[2])
abs(d[39]-d[40])
abs(d[73]-d[74])

abs(d[1]-d[74])
abs(d[1]-d[39])
abs(d[39]-d[74])

#image to see if the pattern you observed in Q2 is general.
image(as.matrix(d))