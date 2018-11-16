#---TEAM 3 FINAL R SCRIPT---#
#---AShley Kitrell, Stephon Lofton, Chris Morton, Natalie Phillips, Peyton Thompson---#

#install packages
  install.packages("dplyr")
  install.packages("magrittr")
  install.packages("tidyverse")
  install.packages("base")
  install.packages("stringi")
  install.packages("stringr")
  install.packages("plotly")
  install.packages("factoextra")
  install.packages("cluster")
  install.packages("NbClust")
  install.packages("dendextend")
  install.packages("ape")
  install.packages("phylogram")
  install.packages("arules")
  library(dplyr)
  library(magrittr)
  library(tidyverse)
  library(ggplot2)
  library(base)
  library(stringi)
  library(stringr)
  library(plotly)
  library(factoextra)
  library(cluster)
  library(NbClust)
  library(dendextend)
  library(ape)
  library(phylogram)
  library(arules)

#----Load Files---_#
  degree <- read.csv("Degrees_Final.csv", header = T)
  head(degree)
  row.names(degree) <-degree$Undergraduate.Major
  degree1 <- degree[, -c(1)]
  head(degree1)
  
  university <- read.csv("University_FInal.csv", header = T)
  head(university)
  rownames(university) <- university$School.Name
  University <- university[,-c(1)]
  head(University)

  
########################################
#----Analysis on Salaries By DEGREE----#
########################################

#----PCA----#
  options(digits=3, scipen = 999)
  summary(degree1)
  
  pca.degree <- prcomp(degree1, scale = T)
  summary(pca.degree)
  
  #exclude unnecessary variables
  degree2 <- degree1[,-c(4:7)]
  head(degree2)

  
#----CLUSTER----#
  
  #----Dendrogram----#
  degree.norm <- scale(degree2)
  d2 <- dist(degree.norm)
  c2 <- hclust(d2)
  
  plot(c2)
  plot(c2, hang=-1, ann=FALSE)
  
  fviz_dend(c2, cex = 0.7, k = 5,
           color_labels_by_k = FALSE, rect = TRUE, main="Degrees Dendrogram", rect_border = "maroon")
  
  g5 <- cutree(c2, 5)
  
  #----Elbow----#
  
  fviz_nbclust(degree.norm, kmeans, method = "wss") + geom_vline(xintercept = 5, linetype = 2)
  
  #----KMeans----#
  kd <- kmeans(degree.norm, 3)
  kd$cluster
  clusplot(degree.norm, kd$cluster, main = 'Degrees Cluster Plot', 
           color= T, shade = T, labels = 2, lines = 1) 



#############################################################
#---- Analysis on Salaries by University REGION and TYPE----# 
#############################################################
  head(University)
  u1 <- University[, -c(1:4,12:36)]
  head(u1)

#----PCA----#
 
   u1$Mid.Career.10th.Percentile.Salary[which(is.na(u1$Mid.Career.10th.Percentile.Salary))] <- mean(u1$Mid.Career.10th.Percentile.Salary, na.rm = T)
  head(u1)
  
  u1$Mid.Career.90th.Percentile.Salary[which(is.na(u1$Mid.Career.90th.Percentile.Salary))] <- mean(u1$Mid.Career.90th.Percentile.Salary, na.rm = T)
  head(u1)
  
  options(digits=3, scipen = 999)
  summary(u1)
  
  pca.u1 <- prcomp(u1, scale = T)
  summary(pca.u1) 
  
  u2 <- u1[,-c(4:7)]
  head(u2)

  
  
#----CLUSTER----#
  
  #----Dendrogram----#
  uni.norm <- scale(u2)
  du <- dist(uni.norm)
  cu <- hclust(du)
  
  plot(cu, hang=-1, abbreviate(row.names(uni.norm)), cex = .9)      
  rect.hclust(cu, h=4, border = "blue")
  
  gu4 <- cutree(cu, 4)
  gu4
  
  #----phylo plot----#
  colors = c("blue", "green", "yellow", "pink", "orange")
  clus4 = cutree(cu, 5)
  plot(as.phylo(cu), type = "fan", tip.color = colors[clus4], edge.color = "gray",
       label.offset = 1, cex = 0.8)
  
  #----Elbow----#
  
  fviz_nbclust(uni.norm, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)
  
#----ASSOCIATION RULE----#

  ubt <- university[,-c(1)]
  ubt <- na.omit(ubt)
  head(ubt)
  
  #assoc for start and medium sal to reg and typ
  binTest.mat <- as.matrix(ubt[, 12:27]) 
  head(binTest.mat)
  bt.trans <- as(binTest.mat, "transactions")
  itemFrequencyPlot(bt.trans)
  bt.apr <- apriori(bt.trans, parameter = list(support = .15, conf = .15, target = "rules"))
  inspect(bt.apr)
  inspect((sort(bt.apr, by = "support")))
  
  inspect(head(sort(bt.apr, by = "lift"), n = 10))
  
#####################
#----NEW METHODS----#
#####################
#Using Salary by University Region and Type
   
  #----create tables----#
  clustab <- data.frame(University, gu4)
  clustab <- clustab[,c(1, 3:6,37)] 
    
  write.csv(clustab, "Cluster_Info.csv")
    
  #REGION GROUPS
  cluster.region <- clustab %>% group_by(gu4, Region) %>% 
  summarise(Avg_Start = mean(Starting.Median.Salary),
  Avg_Mid  = mean(Mid.Career.Median.Salary), total = n())
    
  write.csv(cluster.region, "Cluster_Region.csv")
    
  #TYPE GROUPS
  cluster.type <- clustab %>% group_by(gu4, Type) %>% 
  summarise(Avg_Start = mean(Starting.Median.Salary), 
  Avg_Mid  = mean(Mid.Career.Median.Salary), total = n())
    
  write.csv(cluster.type, "Cluster_Type.csv")
    
  #average salaries per group 
  cluster.groups <- clustab %>% group_by(gu4) %>% 
  summarise(Avg_Start = mean(Starting.Median.Salary), 
  Avg_Mid = mean(Mid.Career.Median.Salary), total = n())
    
  write.csv(cluster.groups, "Cluster_Groups.csv")
    
  #University Facets
    ggplot(university, aes(School.Name, Starting.Median.Salary, color = Type)) + 
    geom_point() + 
    stat_smooth() + 
    facet_wrap(~Region) + ggtitle("University Starting Median Salaries by Region")
  
    ggplot(university, aes(School.Name, Starting.Median.Salary, color = Region)) + 
    geom_point() + 
    stat_smooth() + 
    facet_wrap(~Type) + ggtitle("University Starting Median Salaries by Type")
  
    ggplot(university, aes(School.Name, Mid.Career.Median.Salary, color = Type)) + 
    geom_point() + 
    stat_smooth() + 
    facet_wrap(~Region)  + ggtitle("University Mid-Career Median Salaries by Region")
  
    ggplot(university, aes(School.Name, Mid.Career.Median.Salary, color = Region)) + 
    geom_point() + 
    stat_smooth() + 
    facet_wrap(~Type) + ggtitle("University Mid-Career Median Salaries by Type")
    
    
    #Salary Comparison Facets
    ggplot(university, aes(Starting.Median.Salary, Mid.Career.Median.Salary, color = Region)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    facet_wrap(~Type) + ggtitle("Starting and Mid-Career Median Salaries Linear Model", subtitle =    "Grouped by University Type")
    
    ggplot(university, aes(Starting.Median.Salary, Mid.Career.Median.Salary, color = Region)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    facet_wrap(~Region) + ggtitle("Starting and Mid-Career Median Salaries by Regions")
    
    ggplot(university, aes(Starting.Median.Salary, Mid.Career.Median.Salary, color = Type)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    facet_wrap(~Region) + ggtitle("Starting and Mid-Career Median Salaries Linear Model", subtitle = "Grouped by University Region")
    
    ggplot(university, aes(Starting.Median.Salary, Mid.Career.Median.Salary, color = Type)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    facet_wrap(~Type) + ggtitle("Starting and Mid-Career Median Salaries by Types")
    
#####################
#----BOX PLOTS----#
#####################
#Comparing Salary Medians by University Region and Type
    
    #----create tables----#
    
    
    #REGION GROUPS
    
    
    #TYPE GROUPS
    