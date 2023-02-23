setwd("C:/Users/GAOSHIJIE/Desktop")
library(openxlsx)
d = read.xlsx("cities1.xlsx", sheet = 1)

#head(d)
library(cluster)
library(ggplot2)
library(factoextra)

#preprocessing
d$Crime_Trend = NULL
d$Unemployment_Threat = NULL
rownames(d) = d$Metropolitan_Area
d$Metropolitan_Area = NULL
#head(d)

df = scale(d) #make all attributes scaled
#head(df)

#1 K-MEANS CLUSTERING

#(1)
set.seed(123)

#TWCV for K = 1:16
twcv = function(k) kmeans(df, k, nstart = 25)$tot.withinss
k = 1:16
twcv_values = sapply(k, twcv)
twcv_values

#elbow chart
plot(k, twcv_values, type = "b", pch = 19, xlab = "Number of clusters K", 
     ylab = "TWCV", main = "Elbow chart")
#Through elbow chart, we think K = 4, 6, 8 might be the optimal number of clusters

#(2)
#We try K = 4, 6, 8
#Use K-means to find the clusters with K = 4
k4 = kmeans(df, centers = 4, nstart = 25)

#Cluster plot
fviz_cluster(k4, data = df, geom = "point") 

#Number of cities in each cluster
table(k4$cluster)

#Use K-means to find the clusters with K = 6
k6 = kmeans(df, centers = 6, nstart = 25)

#Cluster plot
fviz_cluster(k6, data = df, geom = "point") 

#Number of cities in each cluster
table(k6$cluster)

#Use K-means to find the clusters with K = 8
k8 = kmeans(df, centers = 8, nstart = 25)

#Cluster plot
fviz_cluster(k8, data = df, geom = "point") 

#Number of cities in each cluster
table(k8$cluster)
#K = 6 and 8 have too much overlap, 
#So we decide K = 4 be the optimal number of clusters as TWCV starts 
#to decrease slowly and there are not too much overlap between each cluster

#(3)

#biplot
#add cluster to dataframe
df1 = df #copy df1, make df1 = df + column of cluster

cluster_number = as.data.frame(k4$cluster)
df1 = cbind(df1,cluster_number)
colnames(df1)[19] = "cluster"

m1 = prcomp(df)
fviz_pca_biplot(m1, label = "var", habillage = df1$cluster)

#median of each numerical column(on unscaled dataset)
aggregate(d, list(k4$cluster), median)

# Conclusion:
# Cluster 1 has highest rate in Transportation, Jobs, Education, Climate, Arts,
# Recreation, Population_2000, Total_Violent,Total_Property, Past_Job_Growth, 
# Fcast_Future_Job_Growth, Fcast_Blue_Collar_Jobs, Fcast_White_Collar_Jobs, 
# Fcast_high_Jobs, Fcast_Average_Jobs, lowest rate in Cost_Living, Crime
# and Health_Care are in middle among four clusters

# Cluster 2 has highest rate in Crime, lowest rate in Jobs, Climate, 
# Total_Violent, Total_Property, Past_Job_Growth, Fcast_Future_Job_Growth, 
# Fcast_Blue_Collar_Jobs, Fcast_high_Jobs and Cost_Living, Transportation,
# Education, Arts, Health_Care, Recreation, Population_2000,
# Fcast_White_Collar_Jobs, Fcast_Average_Jobs are in middle among four clusters

# Cluster 3 has highest rate in Cost_Living, lowest rate in Transportation, 
# Education, Arts, Health_Care, Recreation, Population_2000, 
# Fcast_White_Collar_Jobs, Fcast_Average_Jobs and Jobs, Climate, Crime,
# Total_Violent, Total_Property, Past_Job_Growth, Fcast_Future_Job_Growth,
# Fcast_Blue_Collar_Jobs, Fcast_High_Jobs are in middle among four clusters

# Cluster 4 has highest rate in Health_Care and Cost_Living, 
# Transportation, Jobs, Education, Climate, Crime, Arts, Recreation, 
# Population_2000, Total_Violent, Total_Property, Past_Job_Growth, 
# Fcast_Future_Job_Growth, Fcast_Blue_Collar_Jobs, Fcast_White_Collar_Jobs, 
# Fcast_High_Jobs, Fcast_Average_Jobs are in middle among four clusters


#2 HIERARCHICAL CLUSTERING with K = 4
distance = dist(df)

#(1) Ward.D linkage
#dendrogram 
h1 = hclust(distance, method = "ward.D")
plot(h1, cex = 0.1, xlab = "", ylab = "", sub = "", 
     main = "Ward Linkage Dengrogram")
rect.hclust(h1, k = 4, border = "red")

cut1 = cutree(h1, k=4)
#number of cities in each cluster
table(cut1)

#cluster plot
fviz_cluster(list(data = df, cluster = cut1),main="Ward Linkage",
             palette = "Set2",show.clust.cent = F, geom = "point",
             repel = T, # Avoid label overplotting (slow) 
             ggtheme = theme_minimal()
)

# CCPC for ward.D
c1 = cophenetic(h1)
cor(distance, c1)

#(2) Complete linkage
#dendrogram 
h2 = hclust(distance, method = "complete")
plot(h1, cex = 0.1, xlab = "", ylab = "", sub = "", 
     main = "Complete Linkage Dengrogram")
rect.hclust(h2, k = 4, border = "red")

cut2 = cutree(h2, k=4)
#number of cities in each cluster
table(cut2)

#cluster plot
fviz_cluster(list(data = df, cluster = cut2),main="Complete Linkage",
             palette = "Set2",show.clust.cent = F, geom = "point",
             repel = T, # Avoid label overplotting (slow) 
             ggtheme = theme_minimal()
)

# CCPC for complete linkage
c2 = cophenetic(h2)
cor(distance, c2)

#(3) Average linkage 
#dendrogram 
h3 = hclust(distance, method = "average")
plot(h3, cex = 0.1, xlab = "", ylab = "", sub = "", 
     main = "Average Linkage Dengrogram")
rect.hclust(h3, k = 4, border = "red")

cut3 = cutree(h3, k=4)
#number of cities in each cluster
table(cut3)

#cluster plot
fviz_cluster(list(data = df, cluster = cut3),main="Average Linkage",
             palette = "Set2",show.clust.cent = F, geom = "point",
             repel = T, # Avoid label overplotting (slow) 
             ggtheme = theme_minimal()
)

# CCPC for average linkage.
c3 = cophenetic(h3)
cor(distance, c3)

#(4)
#I prefer average linkage to do hierarchical clustering since the
#clustering using average linkage has the minimum overlap between each clusters
#and it also has the largest CCPC

#biplot
#add cluster to dataframe
df2 = df #copy df2, make df2 = df + column of cluster

cluster_number = as.data.frame(cut3)
df2 = cbind(df2,cluster_number)
colnames(df2)[19] = "cluster"

m2 = prcomp(df)
fviz_pca_biplot(m2, label = "var", habillage = df2$cluster)

#median of each numerical column(on unscaled dataset)
aggregate(d, list(cut3), median)
# Conclusion: 
# Cluster 1 has the highest rate in Cost_Living, Crime, 
# lowest rate in Transportation, Education, Arts, Health_care, 
# Recreation, Population_2000, Total_Violent, Total_Property, 
# Fcast_White_Collar_Jobs, Fcast_Average_Jobs and Jobs, Climate,
# Past_Job_Growth, Fcast_Future_Job_Growth, Fcast_Blue_Collar_Jobs, 
# Fcast_High_Jobs are in middle among four clusters

# Cluster 2 has the highest rate in Jobs, Total_Property, Past_Job_Growth, 
# Fcast_Future_Job_Growth, Fcast_High_Jobs, lowest rate in and Cost_Living,
# Transportation, Education, Climate, Crime, Arts, Health_Care, Recreation,
# Population_2000, Total_Violent, Fcast_Blue_Collar_Jobs,  
# Fcast_White_Collar_Jobs, Fcast_Average_Jobs are in middle among four clusters

# Cluster 3 has the highest rate in Transportation, Education, Health_Care, 
# Recreation, Fcast_Blue_Collar_Jobs, Fcast_White_Collar_Jobs, 
# Fcast_Average_Jobs, lowest rate in Climate and Cost_Living, Jobs, Crime, 
# Arts, Population_2000, Total_Violent, Total_Property, Past_Job_Growth, 
# Fcast_Future_Job_Growth, Fcast_High_Jobs are in middle among four clusters

# Cluster 4 has the highest rate in Climate, Arts, Population_2000, 
# Total_Violent, lowest rate in Cost_Living, Jobs, Crime, Past_Job_Growth,
# Fcast_Future_Job_Growth, Fcast_Blue_Collar_Jobs, Fcast_High_Jobs
# and Transportation, Education, Health_Care, Recreation,Total_Property,
# Fcast_White_Collar_Jobs, Fcast_Average_Jobs are in middle among four clusters