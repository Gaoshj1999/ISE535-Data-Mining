setwd("C:/Users/GAOSHIJIE/Desktop")
df = read.csv("segment.csv") 
#head(df)
#1
#a Two way table for subscribers and home owners
two_way_table = table(df$subscribe, df$ownHome)
two_way_table

#b Hypothesis H_0: m_1 = m_2, where m_i is the proportion of home owner and 
#home renter, let significance level to be alpha = 0.05, as we don't know which 
#one is bigger, so H_a: m_1 not equal to m_2, which is a two-side test

sum = apply(two_way_table, 2, sum) #sum of ownNo and ownYes
sum_sub = apply(two_way_table, 1, sum) #sum of subNo and subYes

p_1 = two_way_table[2, 1] / sum[1] #proportion of subYes/ownNO
p_2 = two_way_table[2, 2] / sum[2] #proportion of subYes/ownYes

p_p = sum_sub[2] / (sum[1]+sum[2]) #pool proportion of subYes
p_1
p_2
p_p
#observed statistic after normalized
z = (p_1 - p_2) / sqrt(p_p*(1-p_p)*(1/sum[1] + 1/sum[2])) 

p_value = 1 - pnorm(z) #p_value/2
p_value 
#As p_value/2 = 0.3927199 > 0.025, so we fail to reject H_0, which means
#We think subscribers is the same between home owners and home renters at
#a significance level of alpha = 0.05(alpha = 0.1 will also accept H_0)

#2
df = read.csv("brands.csv") 
#head(df)

#a Average rating of each brand on each attribute and store
df1 = aggregate(cbind(perform, leader, latest, fun, serious, bargain,
                      value, trendy, rebuy)~brand, df, mean)
rownames(df1) = df1$brand
df1$brand = NULL
df1

#b Display a heatmap using the average ratings from df1 
library(gplots, warn.conflicts = FALSE)
library(RColorBrewer)
heatmap.2(as.matrix(df1),col=brewer.pal(9, "GnBu"),
          trace="none", key=FALSE, dend="none",main="")
#Through heatmap, brand b and c are highly rated on attributes leader, 
#and serious

#c
df$brand = NULL
head(df)
prcomp1 = prcomp(df, center = TRUE, scale = TRUE) #do pca on df
#and prcomp finishes centering the data in its function
prcomp1
summary(prcomp1)#get info of Standard deviation, Proportion of Variance and
#Cumulative Proportion. This item can not transfer into df

var_eig = prcomp1$sdev^2 #get variances = eigenvalues

PVE = var_eig/sum(var_eig) #proportion of var_eig
CPVE = cumsum(PVE) #cumulative of PVE

#draw the line
plot(CPVE, xlab = "PC", ylab = "Cumulative PVE", type = "l", ylim = c(0, 1))
text(CPVE, labels = round(CPVE, 2), cex = 0.75, pos = 1, offset = 0.5, 
     col = "red") #label for each var_eig
grid(col = "black") #draw the grid
#five principle components explain at least 80% of the variation

#d  Construct a biplot from prcomp1
library(factoextra)
fviz_pca_biplot(prcomp1, label = "var")

#e Find principal components from df1. Use prcomp2 = prcomp(df1,scale=T)
prcomp2 = prcomp(df1,scale=T)
#prcomp2$x
prcomp2

#f
fviz_pca_biplot(prcomp2)
#Average position of the brand on each attribute can be seen by biplot, 
#the brands with the same direction of one particular attribute is highly rated,
#on the opposite, the brands with the opposite direction of one particular
#attribute is lowly rated, for more specific, the comparison is about the length
#for the projection of each points on the vector of each attribute

#brands b,c are highly rated on attributes leader, and serious
#brands f,g are highly rated on bargain, and value