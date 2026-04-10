install.packages("ggplot2")
install.packages("dplyr")
install.packages("cluster")
install.packages("factoextra")
install.packages("caTools")
install.packages("caret")

library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(caret)

getwd()

df<-read.csv("~/Downloads/used_car_price_dataset_extended.csv")
head(df)

summary(df)
names(df)

df$fuel_type <- as.factor(df$fuel_type)
df$brand <- as.factor(df$brand)
df$transmission <- as.factor(df$transmission)
df$color <- as.factor(df$color)
df$service_history <- as.factor(df$service_history)
df$insurance_valid <- as.factor(df$insurance_valid)


library(factoextra)
library(caret)

df <- read.csv("~/Downloads/used_car_price_dataset_extended.csv")
head(df)
names(df)
#convert categories coloums to factors 

df$fuel_type <- as.factor(df$fuel_type)
df$brand <- as.factor(df$brand)
df$transmission <- as.factor(df$transmission)
df$color <- as.factor(df$color)
df$service_history <- as.factor(df$service_history)
df$insurance_valid <- as.factor(df$insurance_valid)

#run regression model 
reg_model <- lm(price_usd ~ make_year + mileage_kmpl + engine_cc +
                  fuel_type + owner_count + brand + transmission +
                  accidents_reported + insurance_valid,
                data = df)

summary(reg_model)
#create the clustering dataset 

cluster_df<-df[, c("make_year","mileage_kmpl","engine_cc","owner_count",
                   "accidents_reported","price_usd")]

head(cluster_df)


cluster_df_scaled<-scale(cluster_df)
#elbow method

wss<-numeric(10)
for (i in 1:10) {
  km<-kmeans(cluster_df_scaled,centers = i,nstart = 20)
  wss[i]<-km$tot.withinss
}
plot(1:10,wss,type = "b",
     main = "Elbow Method",
     xlab = "Number of Clusters",
     ylab = "Within-Cluster SS")

#appply k-means 

set.seed(123)
km<-kmeans(cluster_df_scaled,centers = 3,nstart = 25)

km$size
km$centers

df$cluster<-as.factor(km$cluster)
head(df)

summary(reg_model)

km<-kmeans(cluster_df_scaled,centers = 3)
df$cluster<-as.factor(km$cluster)

#visualizations
dev.off()

graphics.off()
fviz_cluster(km,data = cluster_df_scaled)

pca <- prcomp(cluster_df_scaled)
plot(pca$x[,1], pca$x[,2], col = km$cluster, pch = 19)


#regression plot predicted vs actual

pred<-predict(reg_model)
plot(df$price_usd,pred,
     xlab = "Actual Price",
     ylab = "Predicted price",
     main = "Actual vs predicted price")
abline(0,1,col="red",lwd=2)

#Residual plot
plot(reg_model$residuals,
     main = "Residual plots",
     ylab = "Residuals",
     xlab = "Index")
abline(h=0,col="red",lwd=2)

#cluster visualization
install.packages("factoextra")

fviz_cluster(km,data = cluster_df_scaled,
             main = "K means cluster visualization")
summary(reg_model)

#scatter plot with clusters
plot(df$engine_cc,df$price_usd,col=df$cluster,pch=19,
     xlab = "Engine cc",
     ylab = "Price usd",
     main = "Cluster car price")

#calculate RMSE and MAE
pred <- predict(reg_model)
actual <- df$price_usd

# RMSE
rmse <- sqrt(mean((pred - actual)^2))
rmse

# MAE
mae <- mean(abs(pred - actual))
mae

mape <- mean(abs((actual - pred) / actual)) * 100
mape

#Bar graph :fuel type distribution 

barplot(table(df$fuel_type),
        main = "Distribution of fuel types",
        xlab = "fuel type",
        ylab = "Number of cars ",
        col = "skyblue")


barplot(table(df$transmission),
        main = "Transmission Type distribution",
        xlab = "Transmission",
        ylab = "Number of cars",
        col="lightgreen")





