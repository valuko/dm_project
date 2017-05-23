#Which ingredients occur most of all in the highest rating meals? 

install.packages('scatterplot3d')
library(scatterplot3d)
library(ggplot2)
library(e1071)
install.packages("Rcmdr")
library(Rcmdr)
library(RcmdrMisc)

setwd("C:/workspace/Second Sem/Data Mining")

#Read the ven and non-veg ingredients with their 5star ratings
veg_nonveg = read.csv("veg_nonveg_new.csv", header = TRUE,sep=",")
colnames(veg_nonveg)

veg_nonveg_df <- data.frame(veg_nonveg$R5v,veg_nonveg$R5nv)
veg_nonveg_df

#Perform kmeans clustering
veg_nonveg_df.features = veg_nonveg_df
results <- kmeans(veg_nonveg_df.features,2)

results$cluster
results$size

#Plot
clusplot(veg_nonveg_df, results$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#Plot3d
cldata <- data.frame(veg_nonveg_df, Cluster= results$cluster)
cldata

scatter3d(cldata$veg_nonveg.R5v, cldata$veg_nonveg.R5nv, cldata$Cluster, point.col = cldata$Cluster)
