#Aet the direcotry
setwd(choose.dir())
getwd("D:/Dropbox/Public/Temp/Analytics/BA/Clustering and factoring/Cluster and Factor in R/Latest")

#Importing Data
telco<-read.csv(choose.files())

#Exploring data
View(telco)
str(telco)
names(telco)


# user written function for creating descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

vars <- c( "callwait","tollmon","equipmon","internet","age","income",
           "cardmon","reside","wiremon","longmon","multline")

diag_stats<-t(data.frame(apply(telco[vars], 2, mystats)))

write.csv(diag_stats, "diag_stats.csv")

#Outliers
telco$age[telco$age>79.3604490207186] <- 79.3604490207186
telco$income[telco$income>398.667494547117] <- 398.667494547117
telco$reside[telco$reside>6.63837791521762] <- 6.63837791521762
telco$longmon[telco$longmon>42.8135588924664] <- 42.8135588924664
telco$tollmon[telco$tollmon>63.9803662332116] <- 63.9803662332116
telco$equipmon[telco$equipmon>71.4254164129816] <- 71.4254164129816
telco$cardmon[telco$cardmon>56.0344890078125] <- 56.0344890078125
telco$wiremon[telco$wiremon>70.7421768412741] <- 70.7421768412741
telco$multline[telco$multline>1.9738734508362] <- 1.9738734508362
telco$internet[telco$internet>1.81550795482418] <- 1.81550795482418
telco$callwait[telco$callwait>1.98507507319641] <- 1.98507507319641


#View(telco)

inputdata_final <-telco[vars]

#Prepare final Data
#standardizing the data
inputdata_final = scale(inputdata_final)
#View(inputdata_final)
#building clusters using k-means clustering 
cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)

telco_new<-cbind(telco,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(telco_new)

#Graph based on k-means - Optional
require(cluster)

clusplot(inputdata_final, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

###Profiling

#install.packages("tables")
require(tables)
tt<-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
              ~ Heading()*length*All(telco["custcat"]), data=telco_new),
tabular( 1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)
              ~ Heading()*mean*All(telco[vars]), data=telco_new))
tt1<-as.data.frame.matrix(tt)
#View(tt1)

rownames(tt1)<-c("ALL", "KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
colnames(tt1)<-c("Segment_Size","region","tenure" ,"age" ,"marital" ,"address" , "income","ed" ,"employ",
                 "retire","gender","reside","tollfree", "equip","callcard", "wireless", "longmon",
                 "tollmon",  "equipmon", "cardmon",  "wiremon",  "multline", "voice", "pager" ,
                 "internet","callid", "callwait", "forward", "confer", "ebill","custcat")
cluster_profiling<-t(tt1)

write.csv(cluster_profiling, "D:/Dropbox/Public/Temp/Analytics/BA/Clustering and factoring/Cluster and Factor in R/Latest/cluster_profiling.csv") 

