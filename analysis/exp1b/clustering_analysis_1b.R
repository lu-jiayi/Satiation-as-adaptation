setwd("C:/Users/aldos/submiterator")
library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(CRPClustering)
library(cluster)
library(factoextra)
`%notin%` <- Negate(`%in%`)
data<-read.csv("satiation_1b_98item-trials.csv")

#Step 1: Filter out the participants who responded incorrectely more than once to the practice questions:
practice_data=subset(data,block_sequence == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
data=subset(data, is.element(workerid, practice_good_data$workerid))


#Step 2: filter: no overlap of 95%CI of FILL and UNGRAM

filler_data = subset(data, condition == "FILL")
ungram_data = subset(data, condition == "UNGRAM")
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

filler_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), mean)
ungram_by_subject = aggregate(ungram_data[,"response"],list(ungram_data$workerid), ci.high)

names(filler_by_subject)[names(filler_by_subject) == "Group.1"] <- "subject"
names(filler_by_subject)[names(filler_by_subject) == "x"] <- "fill_avg"

names(ungram_by_subject)[names(ungram_by_subject) == "Group.1"] <- "subject"
names(ungram_by_subject)[names(ungram_by_subject) == "x"] <- "ungram_avg"



all_filler <- merge(ungram_by_subject, filler_by_subject, by.x="subject")

eligible_subjects = c()
for (i in (1:length(all_filler$subject))){
  row = all_filler[i,]
  if (row$ungram_avg <= row$fill_avg){
    eligible_subjects <- c(eligible_subjects, row$subject)
  }
}
data = subset(data, workerid %in% eligible_subjects)

#Step 3: exclude non-English speakers
non_Eng <- c()

data = subset(data, workerid %notin% non_Eng)


#clustering analysis (k-means)
all_vec <- data.frame(workerid = integer(), wh_intercept=double(), wh_slope = double(),
                      subj_intercept = double(), subj_slope = double(), 
                      cnpc_intercept = double(), cnpc_slope = double())
for (i in data$workerid){
  if (i %notin% all_vec$workerid){
    
    d_1 <- subset(data, workerid == i)
    
    d_1_wh <- subset(d_1, condition == "WH")
    fitwh = lm(response~trial_sequence_total, data = d_1_wh)
    
    d_1_subj <- subset(d_1, condition =="SUBJ")
    fitsubj = lm(response~trial_sequence_total, data = d_1_subj)
    
    d_1_cnpc <- subset(d_1, condition == "CNPC")
    fitcnpc <- lm(response~trial_sequence_total, data = d_1_cnpc)
    
    wh <- fitwh$coefficients
    subj <- fitsubj$coefficients
    cnpc <- fitcnpc$coefficients
    
    all_vec[nrow(all_vec) + 1,] = c(i, wh, subj, cnpc)
    print(c(i, wh, subj, cnpc))
  }
  
}

all_vec_features = all_vec
all_vec_features$workerid <- NULL

df <- scale(all_vec_features)
distance <- get_dist(df)
distance_graph = fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)
k6 <- kmeans(df, centers = 6, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = df) + ggtitle("k = 6")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#silhouettes method to determine optimal number of clusters
sil<- fviz_nbclust(df, kmeans, method = "silhouette")
#gap-statistics method

set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
## Print the result
print(gap_stat, method = "firstmax")
gap <- fviz_gap_stat(gap_stat)

#elbow-method
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

elbow <- plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


all_vec$cluster <- k2$cluster
cluster_1 <- subset(all_vec, cluster == 1)
cluster_2 <- subset(all_vec, cluster == 2)

data_1 <- subset(data, workerid %in% cluster_1$workerid)
data_2 <- subset(data, workerid %in% cluster_2$workerid)

c_clean1 = ggplot(data_1, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()

c_clean2 = ggplot(data_2, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
library(ggpubr)
theme_set(theme_pubr())
figure <- ggarrange(c_clean1, c_clean2,
                    labels = c("Cluster A: n=57", "Cluster B: n=90"),
                    ncol = 1, nrow = 2)
ggsave("cluster_1b_2clusters.pdf", width=10, height = 12)

all_vec$cluster <- k4$cluster
cluster_1 <- subset(all_vec, cluster == 1)
cluster_2 <- subset(all_vec, cluster == 2)
cluster_3 <- subset(all_vec, cluster == 3)
cluster_4 <- subset(all_vec, cluster == 4)

data_1 <- subset(data, workerid %in% cluster_1$workerid)
data_2 <- subset(data, workerid %in% cluster_2$workerid)
data_3 <- subset(data, workerid %in% cluster_3$workerid)
data_4 <- subset(data, workerid %in% cluster_4$workerid)

c_clean1 = ggplot(data_1, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()

c_clean2 = ggplot(data_2, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
c_clean3 = ggplot(data_3, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
c_clean4 = ggplot(data_4, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
library(ggpubr)
theme_set(theme_pubr())
figure4 <- ggarrange(c_clean1, c_clean2, c_clean3, c_clean4,
                     labels = c("Cluster A", "Cluster B", "Cluster C", "Cluster D"),
                     ncol = 2, nrow = 2)
ggsave("cluster_1b_4clusters.pdf", width=10, height = 24)


all_vec$cluster <- k6$cluster
cluster_1 <- subset(all_vec, cluster == 1)
cluster_2 <- subset(all_vec, cluster == 2)
cluster_3 <- subset(all_vec, cluster == 3)
cluster_4 <- subset(all_vec, cluster == 4)
cluster_5 <- subset(all_vec, cluster == 5)
cluster_6 <- subset(all_vec, cluster == 6)

data_1 <- subset(data, workerid %in% cluster_1$workerid)
data_2 <- subset(data, workerid %in% cluster_2$workerid)
data_3 <- subset(data, workerid %in% cluster_3$workerid)
data_4 <- subset(data, workerid %in% cluster_4$workerid)
data_5 <- subset(data, workerid %in% cluster_5$workerid)
data_6 <- subset(data, workerid %in% cluster_6$workerid)

c_clean1 = ggplot(data_1, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()

c_clean2 = ggplot(data_2, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
c_clean3 = ggplot(data_3, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
c_clean4 = ggplot(data_4, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
c_clean5 = ggplot(data_5, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
c_clean6 = ggplot(data_6, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
theme_set(theme_pubr())
figure6 <- ggarrange(c_clean1, c_clean2, c_clean3, c_clean4, c_clean5, c_clean6,
                     labels = c("Cluster A", "Cluster B", "Cluster C", "Cluster D", "Cluster E", "Cluster F"),
                     ncol = 2, nrow = 3)
ggsave("cluster_1b_6clusters.pdf", width=10, height = 24)
##CRPClustering
# slope_vec <- all_vec_features
# slope_vec$cnpc_intercept <- NULL
# slope_vec$subj_intercept <- NULL
# slope_vec$wh_intercept <- NULL
# 
# intercept_vec <- all_vec_features
# intercept_vec$cnpc_slope <- NULL
# intercept_vec$subj_slope <- NULL
# intercept_vec$wh_slope <- NULL
# 
# 
# 
# z_results<- crp_gibbs(as.matrix(slope_vec),
#                       mu = c(0,0,0),
#                       iteration = 100)
# 
# 
##CRPClustering

# slope_vec <- all_vec_features
# slope_vec$cnpc_intercept <- NULL
# slope_vec$subj_intercept <- NULL
# slope_vec$wh_intercept <- NULL
# 
# intercept_vec <- all_vec_features
# intercept_vec$cnpc_slope <- NULL
# intercept_vec$subj_slope <- NULL
# intercept_vec$wh_slope <- NULL
# 


#z_results<- crp_gibbs(as.matrix(slope_vec),
#                      mu = c(0,0,0),
#                      iteration = 100)

