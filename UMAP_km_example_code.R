##UMAP and k-means clustering for exported MSI abundances
##Author: Alena Joignant
##Last updated: 09282023

##1/6

#Load required packages.
#Install each using install.packages() first if not already installed.
library(umap)
library(plotly)
library(gridExtra)
#Change the working directory to wherever your input files are and results will go.
setwd("C:/Users/")

#Load the MS data. This may be different depending on the format of your file.
#This code also assumes that the first column in the data are well IDs and there is only one header.
data <- read.csv("DATA_NAME.csv", header = TRUE)
data.labels <- data[,1]
data.expected <- data[,2]
ncol(data)
data.values <- data[,3:ncol(data)]
data.values <- scale(data.values)

#Sometimes if there's no change in a column it will give NaN...replace with 0. 
data.values[is.na(data.values)] <- 0 

custom.config <- umap.defaults
custom.confid$min_dist <- 0.001
custom.config$n_neighbors <- 5
#UMAP, random seed, euclidean (all defaults besides number of neighbors)
data.umap <- umap(data.values, config = custom.config)



##2/6



#Begin k-means clustering
wss <- 0
for (i in 1:15) {
  km.out <- kmeans(data.umap$layout, centers = i, nstart = 20)
  wss[i] <- km.out$tot.withinss
}
#Create the scree plot.
#Pick the number of clusters that corresponds to the "elbow".
#The scree plot will not pop up, you need to go to your working directory and open the file. 
png(filename = "NAME_DATE_scree.png",
    width = 800, height = 800)
plot(1:15, wss, type = "b",
    xlab = "Number of Clusters",
    ylab = "Number in Groups Sum of Squares")
dev.off()
##STOP, edit the following code to reflect the number of clusters you want to do.
## If you know there are two groups, use k=2. 
## If the number of groups is unknown, use the scree plot to decide. 



##3/6



#Edit this code and output filenames to use the correct number of centers for your data!!

km.out <- kmeans(data.umap$layout, centers = 4, nstart = 20)
df <- data.frame(data.umap$layout)
df$cluster <- factor(km.out$cluster)
df$labels <- data.labels


k_scatter <- ggplot(data = df, aes(x = X1, y=X2, color = cluster))+
  geom_point(aes(color = cluster), size = 3)+
  theme_light()+
  labs(title= "k=4 Clustering of Data",
       x = "Component 1",
       y = "Component 2")
k_scatter
png(filename = "k4_NAME_DATE_scatter.png",
    width = 800, height = 800)
print(k_scatter)  
dev.off()  


##4/6


#If you have expected values you can 1) plot the true clusters and 2) create a confusion matrix. 

truevalues <- data.expected
df$expected <- as.factor(truevalues)

#At this point we should now have a data frame that has the UMAP projections in X1 and X2, a column of clusters,
#a column of labels (like well address), and a column of expected values.

t_scatter <- ggplot(data = df, aes(x = X1, y=X2, color = expected))+
  geom_point(aes(color = expected), size = 3)+
  theme_light()+
  labs(title= "True Identity Clustering of Data",
       x = "Component 1",
       y = "Component 2")
t_scatter
png(filename="true_NAME_DATE_scatter.png",
    width = 800, height = 800)
print(t_scatter) 
dev.off() #save image



##5/6


#confusion matrix
expected <- df$expected
predicted <- df$cluster

confusion <- table(expected, predicted)
t <- tableGrob(confusion)
png(filename = "NAME_DATE_confusion.png",
    width = 800, height = 800)
grid.arrange(t)
dev.off()

##6/6

#write the results file to the working directory
write.csv(df, "NAME_DATE_k4_results.csv", row.names = FALSE)



##References:
##https://rpubs.com/williamsurles/310847
##https://jtr13.github.io/cc21fall1/efficient-dimension-reduction-with-umap.html 

  





