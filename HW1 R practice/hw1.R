setwd("C:/Users/GAOSHIJIE/Desktop")
data = read.csv("VIT.csv")
#dim(data)
#head(data)
data["price"] = data["price"] / 1000
#head(data)
data[c("toilets", "garage", "elevator", "storage")] <- lapply(data[c("toilets", "garage", "elevator", "storage")], factor)
#data$toilets = as.factor(data$toilets)
#str(data)

table(data$garage) #1

boxplot(price~floor, data) #2

table(data$rooms, data$garage) #3

#4 mean price per type of room & garage
aux = list(data$rooms, data$garage)
signif(tapply(data$price, aux, mean), 2) #up to 2 sig. digits

#5 find max and min price s.t. 80<=area<=90
d = data[data$area <= 90,]
d = d[d$area >= 80,]
d_max = max(d$price)
d_min = min(d$price)
d_max
d_min

#6 Draw a scatterplot for price and most cor variable with price
data_num = data[,sapply(data, is.numeric)] #choose numeric var
cor_matrix = as.data.frame(cor(data_num, method = "spearman",  use = "everything")) #calculate cormatrx
cor_most = order(cor_matrix$price, decreasing = TRUE)[2] #choose max cor except price
var_most_name = names(cor_matrix)[cor_most] #var_name
var_most_name
plot(price~area, data)

#7 Calculate avg diff price between S4 and S2
S4_mean = apply(data[data$street == "S4",]["price"], 2, mean) #get avg price of S4
S2_mean = apply(data[data$street == "S2",]["price"], 2, mean) #get avg price of S2
diff = S4_mean - S2_mean
diff

#8 
plot(price~age, data) #plot all points

diffage_mean = aggregate(price ~ age, data, mean) #calculate mean for different age
m1 = lm(price~age, diffage_mean) #linear regression
coefficients(m1) #get coef
abline(m1, col = "red") #draw the regression line

#9
plot(price~area, data) #plot all points
m2 = lm(price~area, data)
coefficients(m2) #get coef
abline(m2 ,col = "red") #draw the regression line
