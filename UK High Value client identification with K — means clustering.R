install.packages("DataExplorer")
install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")
install.packages("gridExtra")
install.packages("purrr")
install.packages("tidyverse")
install.packages('RGtk2')
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(cluster)
library(factoextra)
library(gridExtra)
library(purrr)
library(tidyverse)
library(RGtk2)

data=read.csv('C:\\Users\\HP\\Desktop\\R Projects\\6\\Ecommerce.csv')
head(data) #541909 obs. of 9 variables

str(data) 
colSums(is.na(data))

# Remove column 'X' which has all the rows with NA
data<-select(data,-c(X)) # 541909 obs. of 8 variables

# Remove rows of CustomerID which has NA data (removed 135080 rows)
data<-na.omit(data) #406829 obs. of 8 variables
head(data)

data$InvoiceDate <- as.Date(data$InvoiceDate,"%d-%B-%y")
view(data)

# Computing the line total 
data <- data %>% mutate(LineTotal = Quantity * UnitPrice) #406829 obs. of 9 variables
head(data)

# Country Summary
countrySummary <- data %>% 
  group_by(Country) %>% 
  summarise(revenue = sum(LineTotal), transactions = n_distinct(InvoiceNo)) %>% 
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>% 
  ungroup() %>% 
  arrange(desc(revenue)) 

head(countrySummary)

# Total revenue generated and Total Items purchased by each customer 
customerData <- data %>% 
  group_by(CustomerID, Country) %>% 
  summarise(TotalRevenue = sum(LineTotal), TotalItemsSold = sum(Quantity))

head(customerData)

# Few customers are residents of two countries in the same year.  
# Such data contributes to very less percentage of data and can be removed to ensure correct functionality for working with data.  
# removed duplicate data #4364 obs, 4 variables
n_occur <- data.frame(table(customerData$CustomerID)) 
single_ResidentsIds = (n_occur[n_occur$Freq == 1,])$Var
customerData <- subset(customerData, (customerData$CustomerID %in% single_ResidentsIds)) 
remove(n_occur) 
remove(single_ResidentsIds) 


set.seed(123) # To ensure the same result every time 

# Categorical variable - Country 
length(unique(data$Country))

# Removing CustomerId and Country Columns 
customerData <- customerData[-c(1:2)] 

# Visualizing outliers 
boxplot(customerData)$out
#boxplot(customerData,horizontal=TRUE)$out

# Eliminating outliers-1-TotalRevenue
iqr <- IQR(customerData$TotalRevenue) 
Q <- quantile(customerData$TotalRevenue, probs=c(.25, .75), na.rm = FALSE) 
eliminated <- subset(customerData, customerData$TotalRevenue > (Q[1] - 1.5*iqr) & customerData$TotalRevenue < (Q[2]+1.5*iqr)) 

# Eliminating outliers-2-TotalRevenue
iqr <- IQR(eliminated$TotalItemsSold) 
Q <- quantile(eliminated$TotalItemsSold, probs=c(.25, .75), na.rm = FALSE) 
customerData <- subset(eliminated, eliminated$TotalItemsSold > (Q[1] - 1.5*iqr) & eliminated$TotalItemsSold < (Q[2]+1.5*iqr)) 
remove(eliminated) 

# Scaling the data before applying the clustering algorithms 
customerData <- scale(customerData)  
head(customerData)


#=======================================================
#set.seed(266) 


# Grouping the data in to 3 clusters using k-means 
#k3 <- kmeans(customerData, centers = 3, nstart = 25) 
#str(k3)

# Plot for 3 groups 
#fviz_cluster(k3, data = customerData)

#k3$size

# Executing kmeans for 2,4,5 number of clusters 
#k2 <- kmeans(customerData, centers = 2, nstart = 25) 
#k4 <- kmeans(customerData, centers = 4, nstart = 25) 
#k5 <- kmeans(customerData, centers = 5, nstart = 25) 

# plots to compare 
#p1 <- fviz_cluster(k2, geom = "point",  data = customerData) + ggtitle("k = 2") 
#p2 <- fviz_cluster(k3, geom = "point",  data = customerData) + ggtitle("k = 3") 
#p3 <- fviz_cluster(k4, geom = "point",  data = customerData) + ggtitle("k = 4") 
#p4 <- fviz_cluster(k5, geom = "point",  data = customerData) + ggtitle("k = 5") 

#grid.arrange(p1, p2, p3, p4, nrow = 2)

#===================================================================
# Determining optimal number of clusters using Elbow Method 
set.seed(123) 

# function to compute total within-cluster sum of square  
wss <- function(k) { 
  kmeans(customerData, k, nstart = 10 )$tot.withinss 
} 

# Compute and plot wss for k = 1 to k = 15 
k.values <- 1:15 

# extract wss for 2-15 clusters 
wss_values <- map_dbl(k.values, wss)

print(wss_values)

plot(k.values, wss_values, 
     type="b", pch = 19, frame = FALSE,  
     xlab="Number of clusters K", 
     ylab="Total within-clusters sum of squares")

#========================================================================
## AGGLOROMATIVE HIERARCHICAL CLUSTERING 

# Dissimilarity matrix 
d <- dist(customerData, method = "euclidean") 

# Hierarchical clustering using Complete Linkage 
hc1 <- hclust(d, method = "complete" ) 

# Plot the obtained dendrogram 
plot(hc1, cex = 0.6, hang = -1)

## DIVISIVE HIERARCHICAL CLUSTERING 

# compute divisive hierarchical clustering 
hc2 <- diana(customerData) 

# Divise coefficient; amount of clustering structure found 
hc2$dc

# plot dendrogram
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# methods to assess 
m <- c( "average", "single", "complete", "ward") 
names(m) <- c( "average", "single", "complete", "ward") 

# function to compute coefficient 
ac <- function(x) { 
  agnes(customerData, method = x)$ac 
} 

map_dbl(m, ac)

# Ward's method 
hc3 <- hclust(d, method = "ward.D2" ) 

# Cut tree into 3 groups 
sub_grp <- cutree(hc3, k = 3) 
str(sub_grp)
head(sub_grp)
table(sub_grp)

fviz_cluster(list(data = customerData, cluster = sub_grp))

# Dendogram with border around 3 clusters 
plot(hc3, cex = 0.6) 
rect.hclust(hc3, k = 3, border = 5:10)

#============================================================

# Determining Optimal clusters in Hierarchical Clustering using Elbow method 
fviz_nbclust(customerData, FUN = hcut, method = "wss")

# Compute WSS for given k value -> Hierarchical Clustering 

wssForHierarchical <- function(k){ 
  sub_grp <- cutree(hc3, k) 
  df=as.data.frame(customerData) 
  
  df2=df %>% 
    mutate(cluster = sub_grp)%>% 
    group_by(cluster)%>% 
    summarize(TR=mean(TotalRevenue), TS=mean(TotalItemsSold)) 
  
  df1=df %>% 
    mutate(cluster = sub_grp) 
  
  df3=left_join(df1,df2,by="cluster") 
  
  D=(df3$TotalRevenue-df3$TR)^2+(df3$TotalItemsSold-df3$TS)^2 
  
  df4=cbind(df3,D) 
  WSS=sum(df4$D) 
  WSS 
} 

# Calculating WSS at hcut=3 
wssForHierarchical(3)