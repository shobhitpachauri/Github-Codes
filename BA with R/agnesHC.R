#Hierarchial clustering using AGNES (Agglomerative Nesting).
#It is an unsupervised clustering method that intially assumes that each 
#data point as a unique cluster, and gradually merging them together to
#form the desired clusters

#Step 1: we import the dataset EmpData
myData <- read.csv("/Users/anxiousviking/Documents/Code/Ba with R/Project_Dataset.csv")
View(myData)

#Step 2: we perform Feature Selection i.e., we take only the necessary attributes for clustering
df <- myData[,c("Diplomat","Sentinel","Analyst","Explorer","Feedback","NPS")]

#Step 3a: we use the library 'cluster' that helps us in creating and
#handling cluster objects
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)

#Step 3b: Calculate the distances
d<- daisy(df, metric = "gower")

#Step 4a: Use AGNES function to perform Hierarchical Clustering
#the method we're using is Single linkage
aResultSingle <- agnes(d, diss = TRUE, method = "single")
aResultSingle
plot(aResultSingle)
aClustersSingle <- cutree(aResultSingle, k = 4)
df<- data.frame(df, aClustersSingle)
silhouette_width_single <- silhouette(aClustersSingle, d)
cat("Average Silhouette Width for Single Linkage:", mean(silhouette_width_single[, "sil_width"]), "\n")
View(df)
df$aClustersSingle <- ifelse(df$aClustersSingle == "1", "Account Manager", 
                             ifelse(df$aClustersSingle == "2", "Business Development Representative", 
                                    ifelse(df$aClustersSingle == "3", "Sales Representative", "Tech Sales Specialist")))
summary(subset(df, aClustersSingle == "Account Manager"))
summary(subset(df, aClustersSingle == "Business Development Representtive"))
summary(subset(df, aClustersSingle == "Sales Representative"))
summary(subset(df, aClustersSingle == "Tech Sales Specialist"))
summary((as.factor(aClustersSingle)))


#Step 4b: we're using Complete linkage
aResultComplete <- agnes(d, diss = TRUE, method = "complete")
aResultComplete
plot(aResultComplete)
aClustersComplete <- cutree(aResultComplete, k = 4)
df<- data.frame(df, aClustersComplete)
silhouette_width_complete <- silhouette(aClustersComplete, d)
cat("Average Silhouette Width for Complete Linkage:", mean(silhouette_width_complete[, "sil_width"]), "\n")
View(df)
df$aClustersComplete <- ifelse(df$aClustersComplete == "1", "Account Manager", 
                             ifelse(df$aClustersComplete == "2", "Business Development Representative", 
                                    ifelse(df$aClustersComplete == "3", "Sales Representative", "Tech Sales Specialist")))
summary(subset(df, aClustersComplete == "Account Manager"))
summary(subset(df, aClustersComplete == "Business Development Representtive"))
summary(subset(df, aClustersComplete == "Sales Representative"))
summary(subset(df, aClustersComplete == "Tech Sales Specialist"))
summary((as.factor(aClustersComplete)))

#Step 4c: we're using Average linkage
aResultAvg <- agnes(d, diss = TRUE, method = "average")
aResultAvg
plot(aResultAvg)
aClustersAvg <- cutree(aResultAvg, k = 4)
df<- data.frame(df, aClustersAvg)
silhouette_width_average <- silhouette(aClustersAvg, d)
cat("Average Silhouette Width for Average Linkage:", mean(silhouette_width_average[, "sil_width"]), "\n")
View(df)
df$aClustersAvg <- ifelse(df$aClustersAvg == "1", "Account Manager", 
                               ifelse(df$aClustersAvg == "2", "Business Development Representative", 
                                      ifelse(df$aClustersAvg == "3", "Sales Representative", "Tech Sales Specialist")))
summary(subset(df, aClustersAvg == "Account Manager"))
summary(subset(df, aClustersAvg == "Business Development Representtive"))
summary(subset(df, aClustersAvg == "Sales Representative"))
summary(subset(df, aClustersAvg == "Tech Sales Specialist"))
summary((as.factor(aClustersAvg)))

#Step 4d: we're using Ward's method of error sum of squares i.e., squared
#difference between individual observations and cluster mean
aResultWard <- agnes(d, diss = TRUE, method = "ward")
aResultWard
plot(aResultWard)
aClustersWard <- cutree(aResultWard, k = 4)
df<- data.frame(df, aClustersWard)
silhouette_width_ward <- silhouette(aClustersWard, d)
cat("Average Silhouette Width for Ward's Method Linkage:", mean(silhouette_width_ward[, "sil_width"]), "\n")
View(df)
df$aClustersWard <- ifelse(df$aClustersWard == "1", "Account Manager", 
                          ifelse(df$aClustersWard == "2", "Business Development Representative", 
                                 ifelse(df$aClustersWard == "3", "Sales Representative", "Tech Sales Specialist")))
summary(subset(df, aClustersWard == "Account Manager"))
summary(subset(df, aClustersWard == "Business Development Representtive"))
summary(subset(df, aClustersWard == "Sales Representative"))
summary(subset(df, aClustersWard == "Tech Sales Specialist"))
summary((as.factor(aClustersWard)))

write.csv(df, file='/Users/rohanmuru/R Studio Files/Sem 1/Buan with R/Project/AgnesDataset.csv')
