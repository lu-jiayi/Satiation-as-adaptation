setwd("C:/Users/aldos/submiterator")
library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(lmerTest)
library(simr)
library(brms)
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
# data_1 = subset(data, workerid < 41)
# data_2 = subset(data, workerid > 120)
# data<- rbind(data_1, data_2)
#Step 3: exclude non-English speakers
non_Eng <- c(119)

data = subset(data, workerid %notin% non_Eng)


#calculate and plot trial/cumulative average
data = subset(data, block_sequence != "practice")
trial_avg <- aggregate(data[,"response"],list(data$trial_sequence_total), mean)
names(trial_avg)[names(trial_avg) == "Group.1"] <- "trial"
names(trial_avg)[names(trial_avg) == "x"] <- "avg"
#trial_average plot

trial_avg <- trial_avg[order(trial_avg$trial),]
cum <- cumsum(trial_avg$avg) / seq_along(trial_avg$avg) 
trial_avg$cum <- cum

a= ggplot(trial_avg, aes(x=trial, y=avg)) + 
    geom_smooth(method = lm, se = F) + geom_point()+
theme_bw()
#a
#cum_average plot
b=ggplot(trial_avg, aes(x=trial, y=cum)) + 
  
  geom_smooth (se = F) + geom_point()+
theme_bw()

#b

#Clean practice trials and control trials.
data = subset(data, block_sequence != "practice")
#data = subset(data, condition != "UNGRAM")
#data = subset(data, condition != "FILL")
d=transform(data, block_sequence = as.numeric(block_sequence))
write.csv(d,"satiation_baseline_cleaned.csv", row.names = FALSE)
d <- read.csv("satiation_baseline_cleaned.csv")
d$condition <- factor(d$condition, levels = c("FILL", "WH", "CNPC","SUBJ", "UNGRAM"))
d$trial_sequence_total <- as.numeric(d$trial_sequence_total)

#look at subset of conditions
#data = subset(data, condition =="CNPC")
#data = subset(data, condition =="SUBJ")
#data = subset(data, condition == "UNGRAM")
#data = subset(data, condition == "WH")
#Step 6: Statistics
#model_block <- lmer(response~block_sequence*condition + (1+block_sequence*condition|workerid)+(1+condition|item_number), data = d)
#summary(model_block)
model_global2 <- lmer(response~trial_sequence_total*condition + 
                     (1+trial_sequence_total*condition|workerid)+(1+trial_sequence_total*condition|item_number), data=d)
summary(model_global2)

model <- lm(response~trial_sequence_total + 
                (1+trial_sequence_total|workerid)+(1+trial_sequence_total|item_number), data=subset(d, condition == "FILL"))
summary(model)
#model_global3 <- brm(response~trial_sequence_total*condition + 
#                        (1+trial_sequence_total*condition|workerid)+(1+trial_sequence_total*condition|item_number), data = d)
#summary(model_global3)

#power analysis
model_ext_class <- extend(model_global2, along="workerid", n=150)

p_curve_treat <- powerCurve(model_ext_class, nsim=100, test = fixed(trial_sequence_total:conditionWH), along="workerid", breaks=c(50,100,150))
plot(p_curve_treat)
#powerSim(model_global2, test=fcompare(response~trial_sequence_total*condition))

#overall plot:

#f

c = ggplot(d, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  geom_point() + 
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()

#c
c_clean = ggplot(d, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
 
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()

#c_clean
d_1 <- subset(d, workerid = 1)
lm(response~condition*trial_sequence_total, data = d_1)
#by-subject Plot
d_n <- subset(d, condition != "UNGRAM")
ggplot(d_n, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  geom_point() + 
 geom_smooth(method=lm, aes(fill=condition))+facet_wrap(~workerid)
ggsave("subject_variability_1b.pdf", width=20, height = 25)

trial_means = d %>%
  group_by(condition,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup() 

ggplot(d, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  geom_point(data=trial_means,alpha=.9) + 
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  xlab("Presentation Order") +
  ylab("Acceptability rating")+
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()

ggsave("satiation_1b_plot.pdf",width=5,height=2.5)

