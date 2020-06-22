setwd("C:/Users/aldos/submiterator")
library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(CRPClustering)
`%notin%` <- Negate(`%in%`)
data<-read.csv("satiation_1c_75item-trials.csv")

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
non_Eng <- c(119)

data = subset(data, workerid %notin% non_Eng)

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
results<-kmeans(intercept_vec, 5)
plot(intercept_vec[c("wh_intercept",  "cnpc_intercept", "subj_intercept")], col=results$cluster)




##CRPClustering
slope_vec <- all_vec_features
slope_vec$cnpc_intercept <- NULL
slope_vec$subj_intercept <- NULL
slope_vec$wh_intercept <- NULL

intercept_vec <- all_vec_features
intercept_vec$cnpc_slope <- NULL
intercept_vec$subj_slope <- NULL
intercept_vec$wh_slope <- NULL


z_results_slope<- crp_gibbs(as.matrix(slope_vec),
                            mu = c(0,0,0),
                            iteration = 2000)

z_results_intercept<- crp_gibbs(as.matrix(intercept_vec),
                            mu = c(0.5,0.5,0.5),
                            iteration = 2000)

z_results<- crp_gibbs(as.matrix(all_vec_features),
                      mu = c(mean(all_vec_features$wh_intercept),
                             mean(all_vec_features$wh_slope),
                             mean(all_vec_features$subj_intercept),
                             mean(all_vec_features$subj_slope),
                             mean(all_vec_features$cnpc_intercept),
                             mean(all_vec_features$cnpc_slope)),
                      iteration = 2000)

z_results


